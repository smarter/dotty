package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees.NamedArg
import dotty.tools.dotc.ast.{Trees, untpd, tpd, TreeTypeMap}
import Trees._
import core._
import Flags._
import Symbols._
import Types._
import Decorators._
import Constants._
import StdNames.nme
import Contexts.Context
import Names.{Name, TermName, EmptyTermName}
import NameOps._
import NameKinds.{ClassifiedNameKind, InlineAccessorName}
import ProtoTypes.selectionProto
import SymDenotations.SymDenotation
import Annotations._
import transform.{ExplicitOuter, AccessProxies}
import Inferencing.fullyDefinedType
import config.Printers.inlining
import ErrorReporting.errorTree
import collection.mutable
import transform.TypeUtils._
import reporting.trace
import util.Positions.Position

object Inliner {
  import tpd._

  class InlineAccessors extends AccessProxies {

    /** A tree map which inserts accessors for all non-public term members accessed
      *  from inlined code. Accessors are collected in the `accessors` buffer.
      */
    class MakeInlineable(inlineSym: Symbol) extends TreeMap with Insert {
      def accessorNameKind = InlineAccessorName

      /** A definition needs an accessor if it is private, protected, or qualified private
        *  and it is not part of the tree that gets inlined. The latter test is implemented
        *  by excluding all symbols properly contained in the inlined method.
        */
      def needsAccessor(sym: Symbol)(implicit ctx: Context) =
        sym.isTerm &&
        (sym.is(AccessFlags) || sym.privateWithin.exists) &&
        !sym.isContainedIn(inlineSym)


      // TODO: Also handle references to non-public types.
      // This is quite tricky, as such types can appear anywhere, including as parts
      // of types of other things. For the moment we do nothing and complain
      // at the implicit expansion site if there's a reference to an inaccessible type.
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case tree: Assign =>
          transform(tree.lhs) match {
            case lhs1: RefTree =>
              lhs1.name match {
                case InlineAccessorName(name) =>
                  cpy.Apply(tree)(useSetter(lhs1), transform(tree.rhs) :: Nil)
                case _ =>
                  super.transform(tree)
              }
            case _ =>
              super.transform(tree)
          }
        case _ =>
          super.transform(accessorIfNeeded(tree))
      }

    }

    /** Adds accessors for all non-public term members accessed
    *  from `tree`. Non-public type members are currently left as they are.
    *  This means that references to a private type will lead to typing failures
    *  on the code when it is inlined. Less than ideal, but hard to do better (see below).
    *
    *  @return If there are accessors generated, a thicket consisting of the rewritten `tree`
    *          and all accessors, otherwise the original tree.
    */
    def makeInlineable(tree: Tree)(implicit ctx: Context) = {
      val inlineSym = ctx.owner
      if (inlineSym.owner.isTerm)
        // Inline methods in local scopes can only be called in the scope they are defined,
        // so no accessors are needed for them.
        tree
      else new MakeInlineable(inlineSym).transform(tree)
    }
  }

  /** Register inline info for given inline method `sym`.
   *
   *  @param sym         The symbol denotatioon of the inline method for which info is registered
   *  @param treeExpr    A function that computes the tree to be inlined, given a context
   *                     This tree may still refer to non-public members.
   *  @param ctx         The context to use for evaluating `treeExpr`. It needs
   *                     to have the inlined method as owner.
   */
  def registerInlineInfo(
      sym: SymDenotation, treeExpr: Context => Tree)(implicit ctx: Context): Unit = {
    sym.unforcedAnnotation(defn.BodyAnnot) match {
      case Some(ann: ConcreteBodyAnnotation) =>
      case Some(ann: LazyBodyAnnotation) if ann.isEvaluated =>
      case _ =>
        if (!ctx.isAfterTyper) {
          val inlineCtx = ctx
          sym.updateAnnotation(LazyBodyAnnotation { _ =>
            implicit val ctx = inlineCtx
            val body = treeExpr(ctx)
            if (ctx.reporter.hasErrors) body else ctx.compilationUnit.inlineAccessors.makeInlineable(body)
          })
        }
    }
  }

  /** `sym` has an inline method with a known body to inline (note: definitions coming
   *  from Scala2x class files might be `@inline`, but still lack that body.
   */
  def hasBodyToInline(sym: SymDenotation)(implicit ctx: Context): Boolean =
    sym.isInlineMethod && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(implicit ctx: Context): Tree =
    sym.unforcedAnnotation(defn.BodyAnnot).get.tree

  /** Try to inline a call to a `@inline` method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param tree   The call to inline
   *  @param pt     The expected type of the call.
   *  @return   An `Inlined` node that refers to the original call and the inlined bindings
   *            and body that replace it.
   */
  def inlineCall(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
    if (enclosingInlineds.length < ctx.settings.XmaxInlines.value) {
      val body = bodyToInline(tree.symbol) // can typecheck the tree and thereby produce errors
      if (ctx.reporter.hasErrors) tree else new Inliner(tree, body).inlined(pt)
    }
    else errorTree(
      tree,
      i"""|Maximal number of successive inlines (${ctx.settings.XmaxInlines.value}) exceeded,
          |Maybe this is caused by a recursive inline method?
          |You can use -Xmax:inlines to change the limit."""
    )

  /** Replace `Inlined` node by a block that contains its bindings and expansion */
  def dropInlined(inlined: tpd.Inlined)(implicit ctx: Context): Tree = {
    val reposition = new TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = {
        super.transform(tree).withPos(inlined.call.pos)
      }
    }
    tpd.seq(inlined.bindings, reposition.transform(inlined.expansion))
  }

  /** The qualifier part of a Select or Ident.
   *  For an Ident, this is the `This` of the current class. (TODO: use elsewhere as well?)
   */
  private def qualifier(tree: Tree)(implicit ctx: Context) = tree match {
    case Select(qual, _) => qual
    case _ => This(ctx.owner.enclosingClass.asClass)
  }
}

/** Produces an inlined version of `call` via its `inlined` method.
 *
 *  @param  call  The original call to a `@inline` method
 *  @param  rhs   The body of the inline method that replaces the call.
 */
class Inliner(call: tpd.Tree, rhs: tpd.Tree)(implicit ctx: Context) {
  import tpd._
  import Inliner._

  private val (methPart, targs, argss) = decomposeCall(call)
  private val meth = methPart.symbol
  private val prefix = qualifier(methPart)

  // Make sure all type arguments to the call are fully determined
  for (targ <- targs) fullyDefinedType(targ.tpe, "inlined type argument", targ.pos)

  /** A map from parameter names of the inline method to references of the actual arguments.
   *  For a type argument this is the full argument type.
   *  For a value argument, it is a reference to either the argument value
   *  (if the argument is a pure expression of singleton type), or to `val` or `def` acting
   *  as a proxy (if the argument is something else).
   */
  private val paramBinding = new mutable.HashMap[Name, Type]

  /** A map from references to (type and value) parameters of the inline method
   *  to their corresponding argument or proxy references, as given by `paramBinding`.
   */
  private val paramProxy = new mutable.HashMap[Type, Type]

  /** A map from the classes of (direct and outer) this references in `rhs`
   *  to references of their proxies.
   *  Note that we can't index by the ThisType itself since there are several
   *  possible forms to express what is logicaly the same ThisType. E.g.
   *
   *     ThisType(TypeRef(ThisType(p), cls))
   *
   *  vs
   *
   *     ThisType(TypeRef(TermRef(ThisType(<root>), p), cls))
   *
   *  These are different (wrt ==) types but represent logically the same key
   */
  private val thisProxy = new mutable.HashMap[ClassSymbol, TermRef]

  /** A buffer for bindings that define proxies for actual arguments */
  val bindingsBuf = new mutable.ListBuffer[ValOrDefDef]

  computeParamBindings(meth.info, targs, argss)

  private def newSym(name: Name, flags: FlagSet, info: Type): Symbol =
    ctx.newSymbol(ctx.owner, name, flags, info, coord = call.pos)

  /** A binding for the parameter of an inlined method. This is a `val` def for
   *  by-value parameters and a `def` def for by-name parameters. `val` defs inherit
   *  inline annotations from their parameters. The generated `def` is appended
   *  to `bindingsBuf`.
   *  @param name        the name of the parameter
   *  @param paramtp     the type of the parameter
   *  @param arg         the argument corresponding to the parameter
   *  @param bindingsBuf the buffer to which the definition should be appended
   */
  private def paramBindingDef(name: Name, paramtp: Type, arg: Tree,
                              bindingsBuf: mutable.ListBuffer[ValOrDefDef]): ValOrDefDef = {
    val argtpe = arg.tpe.dealias
    val isByName = paramtp.dealias.isInstanceOf[ExprType]
    val inlineFlag = if (paramtp.hasAnnotation(defn.InlineParamAnnot)) Inline else EmptyFlags
    val (bindingFlags, bindingType) =
      if (isByName) (Method, ExprType(argtpe.widen))
      else (inlineFlag, argtpe.widen)
    val boundSym = newSym(name, bindingFlags, bindingType).asTerm
    val binding =
      if (isByName) DefDef(boundSym, arg.changeOwner(ctx.owner, boundSym))
      else ValDef(boundSym, arg)
    bindingsBuf += binding
    binding
  }

  /** Populate `paramBinding` and `bindingsBuf` by matching parameters with
   *  corresponding arguments. `bindingbuf` will be further extended later by
   *  proxies to this-references.
   */
  private def computeParamBindings(tp: Type, targs: List[Tree], argss: List[List[Tree]]): Unit = tp match {
    case tp: PolyType =>
      (tp.paramNames, targs).zipped.foreach { (name, arg) =>
        paramBinding(name) = arg.tpe.stripTypeVar
      }
      computeParamBindings(tp.resultType, Nil, argss)
    case tp: MethodType =>
      (tp.paramNames, tp.paramInfos, argss.head).zipped.foreach { (name, paramtp, arg) =>
        paramBinding(name) = arg.tpe.dealias match {
          case _: SingletonType if isIdempotentExpr(arg) => arg.tpe
          case _ => paramBindingDef(name, paramtp, arg, bindingsBuf).symbol.termRef
        }
      }
      computeParamBindings(tp.resultType, targs, argss.tail)
    case _ =>
      assert(targs.isEmpty)
      assert(argss.isEmpty)
  }

  private def canElideThis(tpe: ThisType): Boolean =
    prefix.tpe == tpe && ctx.owner.isContainedIn(tpe.cls) ||
    tpe.cls.isContainedIn(meth) ||
    tpe.cls.is(Package)

  /** Populate `thisProxy` and `paramProxy` as follows:
   *
   *  1a. If given type refers to a static this, thisProxy binds it to corresponding global reference,
   *  1b. If given type refers to an instance this to a class that is not contained in the
   *      inlined method, create a proxy symbol and bind the thistype to refer to the proxy.
   *      The proxy is not yet entered in `bindingsBuf`; that will come later.
   *  2.  If given type refers to a parameter, make `paramProxy` refer to the entry stored
   *      in `paramNames` under the parameter's name. This roundabout way to bind parameter
   *      references to proxies is done because  we don't know a priori what the parameter
   *      references of a method are (we only know the method's type, but that contains TypeParamRefs
   *      and MethodParams, not TypeRefs or TermRefs.
   */
  private def registerType(tpe: Type): Unit = tpe match {
    case tpe: ThisType if !canElideThis(tpe) && !thisProxy.contains(tpe.cls) =>
      val proxyName = s"${tpe.cls.name}_this".toTermName
      val proxyType = tpe.asSeenFrom(prefix.tpe, meth.owner)
      thisProxy(tpe.cls) = newSym(proxyName, Synthetic, proxyType).termRef
      if (!tpe.cls.isStaticOwner)
        registerType(meth.owner.thisType) // make sure we have a base from which to outer-select
    case tpe: NamedType
    if tpe.symbol.is(Param) && tpe.symbol.owner == meth &&
       !paramProxy.contains(tpe) =>
      paramProxy(tpe) = paramBinding(tpe.name)
    case _ =>
  }

  /** Register type of leaf node */
  private def registerLeaf(tree: Tree): Unit = tree match {
    case _: This | _: Ident | _: TypeTree =>
      tree.tpe.foreachPart(registerType, stopAtStatic = true)
    case _ =>
  }

  /** The Inlined node representing the inlined call */
  def inlined(pt: Type) = {
    // make sure prefix is executed if it is impure
    if (!isIdempotentExpr(prefix)) registerType(meth.owner.thisType)

    // Register types of all leaves of inlined body so that the `paramProxy` and `thisProxy` maps are defined.
    rhs.foreachSubTree(registerLeaf)

    // The class that the this-proxy `selfSym` represents
    def classOf(selfSym: Symbol) = selfSym.info.widen.classSymbol

    // The total nesting depth of the class represented by `selfSym`.
    def outerLevel(selfSym: Symbol): Int = classOf(selfSym).ownersIterator.length

    // All needed this-proxies, paired-with and sorted-by nesting depth of
    // the classes they represent (innermost first)
    val sortedProxies = thisProxy.toList.map {
      case (cls, proxy) => (outerLevel(cls), proxy.symbol)
    } sortBy (-_._1)

    // Compute val-definitions for all this-proxies and append them to `bindingsBuf`
    var lastSelf: Symbol = NoSymbol
    var lastLevel: Int = 0
    for ((level, selfSym) <- sortedProxies) {
      lazy val rhsClsSym = selfSym.info.widenDealias.classSymbol
      val rhs =
        if (lastSelf.exists)
          ref(lastSelf).outerSelect(lastLevel - level, selfSym.info)
        else if (rhsClsSym.is(Module) && rhsClsSym.isStatic)
          ref(rhsClsSym.sourceModule)
        else
          prefix
      bindingsBuf += ValDef(selfSym.asTerm, rhs)
      inlining.println(i"proxy at $level: $selfSym = ${bindingsBuf.last}")
      lastSelf = selfSym
      lastLevel = level
    }

    // The type map to apply to the inlined tree. This maps references to this-types
    // and parameters to type references of their arguments or proxies.
    val typeMap = new TypeMap {
      def apply(t: Type) = t match {
        case t: ThisType => thisProxy.getOrElse(t.cls, t)
        case t: TypeRef => paramProxy.getOrElse(t, mapOver(t))
        case t: SingletonType => paramProxy.getOrElse(t, mapOver(t))
        case t => mapOver(t)
      }
      override def mapClassInfo(tp: ClassInfo) = mapFullClassInfo(tp)
    }

    // The tree map to apply to the inlined tree. This maps references to this-types
    // and parameters to references of their arguments or their proxies.
    def treeMap(tree: Tree) = {
      tree match {
      case _: This =>
        tree.tpe match {
          case thistpe: ThisType =>
            thisProxy.get(thistpe.cls) match {
              case Some(t) => ref(t).withPos(tree.pos)
              case None => tree
            }
          case _ => tree
        }
      case _: Ident =>
        paramProxy.get(tree.tpe) match {
          case Some(t) if tree.isTerm && t.isSingleton => singleton(t).withPos(tree.pos)
          case Some(t) if tree.isType => TypeTree(t).withPos(tree.pos)
          case _ => tree
        }
      case _ => tree
    }}

    val inlineCtx = inlineContext(call)
    // The complete translation maps references to `this` and parameters to
    // corresponding arguments or proxies on the type and term level. It also changes
    // the owner from the inlined method to the current owner.
    val inliner = new TreeTypeMap(typeMap, treeMap, meth :: Nil, ctx.owner :: Nil)(inlineCtx)

    val expansion = inliner(rhs)
    trace(i"inlining $call\n, BINDINGS =\n${bindingsBuf.toList}%\n%\nEXPANSION =\n$expansion", inlining, show = true) {

      // The final expansion runs a typing pass over the inlined tree. See InlineTyper for details.
      val expansion1 = InlineTyper.typed(expansion, pt)(inlineCtx)

      /** All bindings in `bindingsBuf` except bindings of inlineable closures */
      val bindings = bindingsBuf.toList.map(_.withPos(call.pos))

      inlining.println(i"original bindings = $bindings%\n%")
      inlining.println(i"original expansion = $expansion1")

      val (finalBindings, finalExpansion) = dropUnusedDefs(bindings, expansion1)

      tpd.Inlined(call, finalBindings, finalExpansion)
    }
  }

  /** An extractor for references to inlineable arguments. These are :
   *   - by-value arguments marked with `inline`
   *   - all by-name arguments
   */
  private object InlineableArg {
    lazy val paramProxies = paramProxy.values.toSet
    def unapply(tree: Ident)(implicit ctx: Context): Option[Tree] =
      if (paramProxies.contains(tree.tpe))
        bindingsBuf.find(_.name == tree.name) match {
          case Some(vdef: ValDef) if vdef.symbol.is(Inline) =>
            Some(vdef.rhs.changeOwner(vdef.symbol, ctx.owner))
          case Some(ddef: DefDef) =>
            Some(ddef.rhs.changeOwner(ddef.symbol, ctx.owner))
          case _ => None
        }
      else None
  }

  /** A typer for inlined code. Its purpose is:
   *  1. Implement constant folding over inlined code
   *  2. Selectively expand ifs with constant conditions
   *  3. Inline arguments that are inlineable closures
   *  4. Make sure inlined code is type-correct.
   *  5. Make sure that the tree's typing is idempotent (so that future -Ycheck passes succeed)
   */
  private object InlineTyper extends ReTyper {

    override def ensureAccessible(tpe: Type, superAccess: Boolean, pos: Position)(implicit ctx: Context): Type = {
      tpe match {
        case tpe @ TypeRef(pre, _) if !tpe.symbol.isAccessibleFrom(pre, superAccess) =>
          tpe.info match {
            case TypeAlias(alias) => return ensureAccessible(alias, superAccess, pos)
            case _ =>
          }
        case _ =>
      }
      super.ensureAccessible(tpe, superAccess, pos)
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context) =
      tree.asInstanceOf[tpd.Tree] match {
        case InlineableArg(rhs) => inlining.println(i"inline arg $tree -> $rhs"); rhs
        case _ => super.typedIdent(tree, pt)
      }

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.hasType, tree)
      val qual1 = typed(tree.qualifier, selectionProto(tree.name, pt, this))
      val res = untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
      ensureAccessible(res.tpe, tree.qualifier.isInstanceOf[untpd.Super], tree.pos)
      res
    }

    override def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context) = {
      val cond1 = typed(tree.cond, defn.BooleanType)
      cond1.tpe.widenTermRefExpr match {
        case ConstantType(Constant(condVal: Boolean)) =>
          val selected = typed(if (condVal) tree.thenp else tree.elsep, pt)
          if (isIdempotentExpr(cond1)) selected
          else Block(cond1 :: Nil, selected)
        case _ =>
          val if1 = untpd.cpy.If(tree)(cond = untpd.TypedSplice(cond1))
          super.typedIf(if1, pt)
      }
    }

    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context) = {

      /** Rewrite an application
       *
       *    ((x1, ..., sn) => b)(e1, ..., en)
       *
       *  to
       *
       *    val/def x1 = e1; ...; val/def xn = en; b
       *
       *  where `def` is used for call-by-name parameters. However, we shortcut any NoPrefix
       *  refs among the ei's directly without creating an intermediate binding.
       */
      def betaReduce(tree: Tree) = tree match {
        case Apply(Select(cl @ closureDef(ddef), nme.apply), args) if defn.isFunctionType(cl.tpe) =>
          ddef.tpe.widen match {
            case mt: MethodType if ddef.vparamss.head.length == args.length =>
              val bindingsBuf = new mutable.ListBuffer[ValOrDefDef]
              val argSyms = (mt.paramNames, mt.paramInfos, args).zipped.map { (name, paramtp, arg) =>
                arg.tpe.dealias match {
                  case ref @ TermRef(NoPrefix, _) => ref.symbol
                  case _ => paramBindingDef(name, paramtp, arg, bindingsBuf).symbol
                }
              }
              val expander = new TreeTypeMap(
                oldOwners = ddef.symbol :: Nil,
                newOwners = ctx.owner :: Nil,
                substFrom = ddef.vparamss.head.map(_.symbol),
                substTo = argSyms)
              Block(bindingsBuf.toList, expander.transform(ddef.rhs))
            case _ => tree
          }
        case _ => tree
      }

      betaReduce(super.typedApply(tree, pt))
    }
  }

  /** Drop any side-effect-free bindings that are unused in expansion or other reachable bindings.
   *  Inline def bindings that are used only once.
   */
  def dropUnusedDefs(bindings: List[ValOrDefDef], tree: Tree)(implicit ctx: Context): (List[ValOrDefDef], Tree) = {
    val refCount = newMutableSymbolMap[Int]
    val bindingOfSym = newMutableSymbolMap[ValOrDefDef]
    def isInlineable(binding: ValOrDefDef) = binding match {
      case DefDef(_, Nil, Nil, _, _) => true
      case vdef @ ValDef(_, _, _) => isPureExpr(vdef.rhs)
      case _ => false
    }
    for (binding <- bindings if isInlineable(binding)) {
      refCount(binding.symbol) = 0
      bindingOfSym(binding.symbol) = binding
    }
    val countRefs = new TreeTraverser {
      override def traverse(t: Tree)(implicit ctx: Context) = {
        t match {
          case t: RefTree =>
            refCount.get(t.symbol) match {
              case Some(x) => refCount(t.symbol) = x + 1
              case none =>
            }
          case _: New | _: TypeTree =>
            t.tpe.foreachPart {
              case ref: TermRef =>
                refCount.get(ref.symbol) match {
                  case Some(x) => refCount(ref.symbol) = x + 2
                  case none =>
                }
              case _ =>
            }
          case _ =>
        }
        traverseChildren(t)
      }
    }
    countRefs.traverse(tree)
    for (binding <- bindings) countRefs.traverse(binding.rhs)
    val inlineBindings = new TreeMap {
      override def transform(t: Tree)(implicit ctx: Context) =
        super.transform {
          t match {
            case t: RefTree =>
              val sym = t.symbol
              refCount.get(sym) match {
                case Some(1) if sym.is(Method) =>
                  bindingOfSym(sym).rhs.changeOwner(sym, ctx.owner)
                case none => t
              }
            case _ => t
          }
        }
      }
    def retain(binding: ValOrDefDef) = refCount.get(binding.symbol) match {
      case Some(x) => x > 1 || x == 1 && !binding.symbol.is(Method)
      case none => true
    }
    val retained = bindings.filterConserve(retain)
    if (retained `eq` bindings) {
      (bindings, tree)
    }
    else {
      val expanded = inlineBindings.transform(tree)
      dropUnusedDefs(retained, expanded)
    }
  }
}
