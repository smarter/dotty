package dotty.tools
package dotc
package core
package tasty

import ast.Trees._
import ast.untpd
import TastyFormat._
import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._, StdNames.tpnme, NameOps._
import collection.mutable
import typer.Inliner
import NameOps._, NameKinds._
import StdNames.nme
import TastyBuffer._
import TypeApplications._
import transform.SymUtils._
import config.Config

class TreePickler(pickler: TastyPickler) {
  val buf = new TreeBuffer
  pickler.newSection("ASTs", buf)
  import buf._
  import pickler.nameBuffer.nameIndex
  import ast.tpd._

  private val symRefs = Symbols.newMutableSymbolMap[Addr]
  private val forwardSymRefs = Symbols.newMutableSymbolMap[List[Addr]]
  private val pickledTypes = new java.util.IdentityHashMap[Type, Any] // Value type is really Addr, but that's not compatible with null

  private def withLength(op: => Unit) = {
    val lengthAddr = reserveRef(relative = true)
    op
    fillRef(lengthAddr, currentAddr, relative = true)
  }

  def addrOfSym(sym: Symbol): Option[Addr] = {
    symRefs.get(sym)
  }

  def preRegister(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: MemberDef =>
      if (!symRefs.contains(tree.symbol)) symRefs(tree.symbol) = NoAddr
    case _ =>
  }

  def registerDef(sym: Symbol): Unit = {
    symRefs(sym) = currentAddr
    forwardSymRefs.get(sym) match {
      case Some(refs) =>
        refs.foreach(fillRef(_, currentAddr, relative = false))
        forwardSymRefs -= sym
      case None =>
    }
  }

  private def pickleName(name: Name): Unit = writeNat(nameIndex(name).index)

  private def pickleNameAndSig(name: Name, sig: Signature): Unit =
    pickleName(
      if (sig eq Signature.NotAMethod) name
      else SignedName(name.toTermName, sig))

  private def pickleSymRef(sym: Symbol)(implicit ctx: Context) = symRefs.get(sym) match {
    case Some(label) =>
      if (label != NoAddr) writeRef(label) else pickleForwardSymRef(sym)
    case None =>
      // See pos/t1957.scala for an example where this can happen.
      // I believe it's a bug in typer: the type of an implicit argument refers
      // to a closure parameter outside the closure itself. TODO: track this down, so that we
      // can eliminate this case.
      ctx.log(i"pickling reference to as yet undefined $sym in ${sym.owner}", sym.pos)
      pickleForwardSymRef(sym)
  }

  private def pickleForwardSymRef(sym: Symbol)(implicit ctx: Context) = {
    val ref = reserveRef(relative = false)
    assert(!sym.is(Flags.Package), sym)
    forwardSymRefs(sym) = ref :: forwardSymRefs.getOrElse(sym, Nil)
  }

  private def isLocallyDefined(sym: Symbol)(implicit ctx: Context) =
    sym.topLevelClass.isLinkedWith(pickler.rootCls)

  def pickleConstant(c: Constant)(implicit ctx: Context): Unit = c.tag match {
    case UnitTag =>
      writeByte(UNITconst)
    case BooleanTag =>
      writeByte(if (c.booleanValue) TRUEconst else FALSEconst)
    case ByteTag =>
      writeByte(BYTEconst)
      writeInt(c.byteValue)
    case ShortTag =>
      writeByte(SHORTconst)
      writeInt(c.shortValue)
    case CharTag =>
      writeByte(CHARconst)
      writeNat(c.charValue)
    case IntTag =>
      writeByte(INTconst)
      writeInt(c.intValue)
    case LongTag =>
      writeByte(LONGconst)
      writeLongInt(c.longValue)
    case FloatTag =>
      writeByte(FLOATconst)
      writeInt(java.lang.Float.floatToRawIntBits(c.floatValue))
    case DoubleTag =>
      writeByte(DOUBLEconst)
      writeLongInt(java.lang.Double.doubleToRawLongBits(c.doubleValue))
    case StringTag =>
      writeByte(STRINGconst)
      pickleName(c.stringValue.toTermName)
    case NullTag =>
      writeByte(NULLconst)
    case ClazzTag =>
      writeByte(CLASSconst)
      pickleType(c.typeValue)
    case EnumTag =>
      writeByte(ENUMconst)
      pickleType(c.symbolValue.termRef)
    case ScalaSymbolTag =>
      writeByte(SYMBOLconst)
      pickleName(c.scalaSymbolValue.name.toTermName)
  }

  def pickleType(tpe0: Type, richTypes: Boolean = false)(implicit ctx: Context): Unit = {
    val tpe = tpe0.stripTypeVar
    try {
      val prev = pickledTypes.get(tpe)
      if (prev == null) {
        pickledTypes.put(tpe, currentAddr)
        pickleNewType(tpe, richTypes)
      }
      else {
        writeByte(SHARED)
        writeRef(prev.asInstanceOf[Addr])
      }
    } catch {
      case ex: AssertionError =>
        println(i"error when pickling type $tpe")
        throw ex
    }
  }

  private def pickleNewType(tpe: Type, richTypes: Boolean)(implicit ctx: Context): Unit = tpe match {
    case AppliedType(tycon, args) =>
      writeByte(APPLIEDtype)
      withLength { pickleType(tycon); args.foreach(pickleType(_)) }
    case ConstantType(value) =>
      pickleConstant(value)
    case tpe: NamedType =>
      val sym = tpe.symbol
      def pickleDirectRef() = {
        writeByte(if (tpe.isType) TYPEREFdirect else TERMREFdirect)
        pickleSymRef(sym)
      }
      def pickleExternalRef(sym: Symbol) = {
        def pickleCore() = {
          pickleNameAndSig(sym.name, tpe.signature)
          pickleType(tpe.prefix)
        }
        val isShadowedRef =
          sym.isClass && tpe.prefix.member(sym.name).symbol != sym
        if (sym.is(Flags.Private) || isShadowedRef) {
          writeByte(if (tpe.isType) TYPEREFin else TERMREFin)
          withLength {
            pickleCore()
            pickleType(sym.owner.typeRef)
          }
        }
        else {
          writeByte(if (tpe.isType) TYPEREF else TERMREF)
          pickleCore()
        }
      }
      if (sym.is(Flags.Package)) {
        writeByte(if (tpe.isType) TYPEREFpkg else TERMREFpkg)
        pickleName(sym.fullName)
      }
      else if (tpe.prefix == NoPrefix)
        if (sym is Flags.BindDefinedType) {
          registerDef(sym)
          writeByte(BIND)
          withLength {
            pickleName(sym.name)
            pickleType(sym.info)
            pickleDirectRef()
          }
        }
        else pickleDirectRef()
      else if (isLocallyDefined(sym)) {
        writeByte(if (tpe.isType) TYPEREFsymbol else TERMREFsymbol)
        pickleSymRef(sym); pickleType(tpe.prefix)
      }
      else tpe.designator match {
        case name: Name =>
          writeByte(if (tpe.isType) TYPEREF else TERMREF)
          pickleName(name); pickleType(tpe.prefix)
        case sym: Symbol =>
          pickleExternalRef(sym)
      }
    case tpe: ThisType =>
      if (tpe.cls.is(Flags.Package) && !tpe.cls.isEffectiveRoot) {
        writeByte(TERMREFpkg)
        pickleName(tpe.cls.fullName)
      }
      else {
        writeByte(THIS)
        pickleType(tpe.tref)
      }
    case tpe: SuperType =>
      writeByte(SUPERtype)
      withLength { pickleType(tpe.thistpe); pickleType(tpe.supertpe) }
    case tpe: RecThis =>
      writeByte(RECthis)
      val binderAddr = pickledTypes.get(tpe.binder)
      assert(binderAddr != null, tpe.binder)
      writeRef(binderAddr.asInstanceOf[Addr])
    case tpe: SkolemType =>
      pickleType(tpe.info)
    case tpe: RefinedType =>
      writeByte(REFINEDtype)
      withLength {
        pickleName(tpe.refinedName)
        pickleType(tpe.parent)
        pickleType(tpe.refinedInfo, richTypes = true)
      }
    case tpe: RecType =>
      writeByte(RECtype)
      pickleType(tpe.parent)
    case tpe: TypeAlias =>
      writeByte(TYPEALIAS)
      pickleType(tpe.alias, richTypes)
    case tpe: TypeBounds =>
      writeByte(TYPEBOUNDS)
      withLength { pickleType(tpe.lo, richTypes); pickleType(tpe.hi, richTypes) }
    case tpe: AnnotatedType =>
      writeByte(ANNOTATEDtype)
      withLength { pickleType(tpe.tpe, richTypes); pickleTree(tpe.annot.tree) }
    case tpe: AndOrType =>
      writeByte(if (tpe.isAnd) ANDtype else ORtype)
      withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
    case tpe: ExprType =>
      writeByte(BYNAMEtype)
      pickleType(tpe.underlying)
    case tpe: HKTypeLambda =>
      pickleMethodic(TYPELAMBDAtype, tpe)
    case tpe: PolyType if richTypes =>
      pickleMethodic(POLYtype, tpe)
    case tpe: MethodType if richTypes =>
      pickleMethodic(METHODtype, tpe)
    case tpe: ParamRef =>
      assert(pickleParamRef(tpe), s"orphan parameter reference: $tpe")
    case tpe: LazyRef =>
      pickleType(tpe.ref)
  }

  def pickleMethodic(tag: Int, tpe: LambdaType)(implicit ctx: Context) = {
    writeByte(tag)
    withLength {
      pickleType(tpe.resultType, richTypes = true)
      (tpe.paramNames, tpe.paramInfos).zipped.foreach { (name, tpe) =>
        pickleName(name); pickleType(tpe)
      }
    }
  }

  def pickleParamRef(tpe: ParamRef)(implicit ctx: Context): Boolean = {
    val binder = pickledTypes.get(tpe.binder)
    val pickled = binder != null
    if (pickled) {
      writeByte(PARAMtype)
      withLength { writeRef(binder.asInstanceOf[Addr]); writeNat(tpe.paramNum) }
    }
    pickled
  }

  def pickleTpt(tpt: Tree)(implicit ctx: Context): Unit =
    pickleTree(tpt)

  def pickleTreeUnlessEmpty(tree: Tree)(implicit ctx: Context): Unit =
    if (!tree.isEmpty) pickleTree(tree)

  def pickleDef(tag: Int, sym: Symbol, tpt: Tree, rhs: Tree = EmptyTree, pickleParams: => Unit = ())(implicit ctx: Context) = {
    assert(symRefs(sym) == NoAddr, sym)
    registerDef(sym)
    writeByte(tag)
    withLength {
      pickleName(sym.name)
      pickleParams
      tpt match {
        case templ: Template => pickleTree(tpt)
        case _ if tpt.isType => pickleTpt(tpt)
      }
      pickleTreeUnlessEmpty(rhs)
      pickleModifiers(sym)
    }
  }

  def pickleParam(tree: Tree)(implicit ctx: Context): Unit = {
    registerTreeAddr(tree)
    tree match {
      case tree: ValDef => pickleDef(PARAM, tree.symbol, tree.tpt)
      case tree: DefDef => pickleDef(PARAM, tree.symbol, tree.tpt, tree.rhs)
      case tree: TypeDef => pickleDef(TYPEPARAM, tree.symbol, tree.rhs)
    }
  }

  def pickleParams(trees: List[Tree])(implicit ctx: Context): Unit = {
    trees.foreach(preRegister)
    trees.foreach(pickleParam)
  }

  def pickleStats(stats: List[Tree])(implicit ctx: Context) = {
    stats.foreach(preRegister)
    stats.foreach(stat => if (!stat.isEmpty) pickleTree(stat))
  }

  def pickleTree(tree: Tree)(implicit ctx: Context): Unit = {
    val addr = registerTreeAddr(tree)
    if (addr != currentAddr) {
      writeByte(SHARED)
      writeRef(addr)
    }
    else
      try tree match {
        case Ident(name) =>
          tree.tpe match {
            case tp: TermRef if name != nme.WILDCARD =>
              // wildcards are pattern bound, need to be preserved as ids.
              pickleType(tp)
            case _ =>
              writeByte(if (tree.isType) IDENTtpt else IDENT)
              pickleName(name)
              pickleType(tree.tpe)
          }
        case This(qual) =>
          if (qual.isEmpty) pickleType(tree.tpe)
          else {
            writeByte(QUALTHIS)
            val ThisType(tref) = tree.tpe
            pickleTree(qual.withType(tref))
          }
        case Select(qual, name) =>
          name match {
            case OuterSelectName(_, levels) =>
              writeByte(SELECTouter)
              withLength {
                writeNat(levels)
                pickleTree(qual)
                val SkolemType(tp) = tree.tpe
                pickleType(tp)
              }
            case _ =>
              writeByte(if (name.isTypeName) SELECTtpt else SELECT)
              val sig = tree.tpe.signature
              pickleNameAndSig(name, sig)
              pickleTree(qual)
          }
        case Apply(fun, args) =>
          writeByte(APPLY)
          withLength {
            pickleTree(fun)
            args.foreach(pickleTree)
          }
        case TypeApply(fun, args) =>
          writeByte(TYPEAPPLY)
          withLength {
            pickleTree(fun)
            args.foreach(pickleTpt)
          }
        case Literal(const1) =>
          pickleConstant {
            tree.tpe match {
              case ConstantType(const2) => const2
              case _ => const1
            }
          }
        case Super(qual, mix) =>
          writeByte(SUPER)
          withLength {
            pickleTree(qual);
            if (!mix.isEmpty) {
              val SuperType(_, mixinType: TypeRef) = tree.tpe
              pickleTree(mix.withType(mixinType))
            }
          }
        case New(tpt) =>
          writeByte(NEW)
          pickleTpt(tpt)
        case Typed(expr, tpt) =>
          writeByte(TYPED)
          withLength { pickleTree(expr); pickleTpt(tpt) }
        case NamedArg(name, arg) =>
          writeByte(NAMEDARG)
          pickleName(name)
          pickleTree(arg)
        case Assign(lhs, rhs) =>
          writeByte(ASSIGN)
          withLength { pickleTree(lhs); pickleTree(rhs) }
        case Block(stats, expr) =>
          writeByte(BLOCK)
          stats.foreach(preRegister)
          withLength { pickleTree(expr); stats.foreach(pickleTree) }
        case If(cond, thenp, elsep) =>
          writeByte(IF)
          withLength { pickleTree(cond); pickleTree(thenp); pickleTree(elsep) }
        case Closure(env, meth, tpt) =>
          writeByte(LAMBDA)
          assert(env.isEmpty)
          withLength {
            pickleTree(meth)
            if (tpt.tpe.exists) pickleTpt(tpt)
          }
        case Match(selector, cases) =>
          writeByte(MATCH)
          withLength { pickleTree(selector); cases.foreach(pickleTree) }
        case CaseDef(pat, guard, rhs) =>
          writeByte(CASEDEF)
          withLength { pickleTree(pat); pickleTree(rhs); pickleTreeUnlessEmpty(guard) }
        case Return(expr, from) =>
          writeByte(RETURN)
          withLength { pickleSymRef(from.symbol); pickleTreeUnlessEmpty(expr) }
        case Try(block, cases, finalizer) =>
          writeByte(TRY)
          withLength { pickleTree(block); cases.foreach(pickleTree); pickleTreeUnlessEmpty(finalizer) }
        case SeqLiteral(elems, elemtpt) =>
          writeByte(REPEATED)
          withLength { pickleTree(elemtpt); elems.foreach(pickleTree) }
        case Inlined(call, bindings, expansion) =>
          writeByte(INLINED)
          bindings.foreach(preRegister)
          withLength { pickleTree(call); pickleTree(expansion); bindings.foreach(pickleTree) }
        case Bind(name, body) =>
          registerDef(tree.symbol)
          writeByte(BIND)
          withLength { pickleName(name); pickleType(tree.symbol.info); pickleTree(body) }
        case Alternative(alts) =>
          writeByte(ALTERNATIVE)
          withLength { alts.foreach(pickleTree) }
        case UnApply(fun, implicits, patterns) =>
          writeByte(UNAPPLY)
          withLength {
            pickleTree(fun)
            for (implicitArg <- implicits) {
              writeByte(IMPLICITarg)
              pickleTree(implicitArg)
            }
            pickleType(tree.tpe)
            patterns.foreach(pickleTree)
          }
        case tree: ValDef =>
          pickleDef(VALDEF, tree.symbol, tree.tpt, tree.rhs)
        case tree: DefDef =>
          def pickleAllParams = {
            pickleParams(tree.tparams)
            for (vparams <- tree.vparamss) {
              writeByte(PARAMS)
              withLength { pickleParams(vparams) }
            }
          }
          pickleDef(DEFDEF, tree.symbol, tree.tpt, tree.rhs, pickleAllParams)
        case tree: TypeDef =>
          pickleDef(TYPEDEF, tree.symbol, tree.rhs)
        case tree: Template =>
          registerDef(tree.symbol)
          writeByte(TEMPLATE)
          val (params, rest) = tree.body partition {
            case stat: TypeDef => stat.symbol is Flags.Param
            case stat: ValOrDefDef =>
              stat.symbol.is(Flags.ParamAccessor) && !stat.symbol.isSetter
            case _ => false
          }
          withLength {
            pickleParams(params)
            tree.parents.foreach(pickleTree)
            val cinfo @ ClassInfo(_, _, _, _, selfInfo) = tree.symbol.owner.info
            if ((selfInfo ne NoType) || !tree.self.isEmpty) {
              writeByte(SELFDEF)
              pickleName(tree.self.name)

              if (!tree.self.tpt.isEmpty) pickleTree(tree.self.tpt)
              else {
                if (!tree.self.isEmpty) registerTreeAddr(tree.self)
                pickleType {
                  cinfo.selfInfo match {
                    case sym: Symbol => sym.info
                    case tp: Type => tp
                  }
                }
              }
            }
            pickleStats(tree.constr :: rest)
          }
        case Import(expr, selectors) =>
          writeByte(IMPORT)
          withLength {
            pickleTree(expr)
            selectors foreach {
              case Thicket((from @ Ident(_)) :: (to @ Ident(_)) :: Nil) =>
                pickleSelector(IMPORTED, from)
                pickleSelector(RENAMED, to)
              case id @ Ident(_) =>
                pickleSelector(IMPORTED, id)
            }
          }
        case PackageDef(pid, stats) =>
          writeByte(PACKAGE)
          withLength { pickleType(pid.tpe); pickleStats(stats) }
        case tree: TypeTree =>
          pickleType(tree.tpe)
        case SingletonTypeTree(ref) =>
          writeByte(SINGLETONtpt)
          pickleTree(ref)
        case RefinedTypeTree(parent, refinements) =>
          if (refinements.isEmpty) pickleTree(parent)
          else {
            val refineCls = refinements.head.symbol.owner.asClass
            pickledTypes.put(refineCls.typeRef, currentAddr)
            writeByte(REFINEDtpt)
            refinements.foreach(preRegister)
            withLength { pickleTree(parent); refinements.foreach(pickleTree) }
          }
        case AppliedTypeTree(tycon, args) =>
          writeByte(APPLIEDtpt)
          withLength { pickleTree(tycon); args.foreach(pickleTree) }
        case AndTypeTree(tp1, tp2) =>
          writeByte(ANDtpt)
          withLength { pickleTree(tp1); pickleTree(tp2) }
        case OrTypeTree(tp1, tp2) =>
          writeByte(ORtpt)
          withLength { pickleTree(tp1); pickleTree(tp2) }
        case ByNameTypeTree(tp) =>
          writeByte(BYNAMEtpt)
          pickleTree(tp)
        case Annotated(tree, annot) =>
          writeByte(ANNOTATEDtpt)
          withLength { pickleTree(tree); pickleTree(annot.tree) }
        case LambdaTypeTree(tparams, body) =>
          writeByte(LAMBDAtpt)
          withLength { pickleParams(tparams); pickleTree(body) }
        case TypeBoundsTree(lo, hi) =>
          writeByte(TYPEBOUNDStpt)
          withLength { pickleTree(lo); pickleTree(hi) }
      }
      catch {
        case ex: AssertionError =>
          println(i"error when pickling tree $tree")
          throw ex
      }
  }

  def pickleSelector(tag: Int, id: untpd.Ident)(implicit ctx: Context): Unit = {
    registerTreeAddr(id)
    writeByte(tag)
    pickleName(id.name)
  }

  def pickleModifiers(sym: Symbol)(implicit ctx: Context): Unit = {
    import Flags._
    val flags = sym.flags
    val privateWithin = sym.privateWithin
    if (privateWithin.exists) {
      writeByte(if (flags is Protected) PROTECTEDqualified else PRIVATEqualified)
      pickleType(privateWithin.typeRef)
    }
    if (flags is Private) writeByte(PRIVATE)
    if (flags is Protected) if (!privateWithin.exists) writeByte(PROTECTED)
    if ((flags is Final) && !(sym is Module)) writeByte(FINAL)
    if (flags is Case) writeByte(CASE)
    if (flags is Override) writeByte(OVERRIDE)
    if (flags is Inline) writeByte(INLINE)
    if (flags is JavaStatic) writeByte(STATIC)
    if (flags is Module) writeByte(OBJECT)
    if (flags is Local) writeByte(LOCAL)
    if (flags is Synthetic) writeByte(SYNTHETIC)
    if (flags is Artifact) writeByte(ARTIFACT)
    if (flags is Scala2x) writeByte(SCALA2X)
    if (sym.isTerm) {
      if (flags is Implicit) writeByte(IMPLICIT)
      if ((flags is Lazy) && !(sym is Module)) writeByte(LAZY)
      if (flags is AbsOverride) { writeByte(ABSTRACT); writeByte(OVERRIDE) }
      if (flags is Mutable) writeByte(MUTABLE)
      if (flags is Accessor) writeByte(FIELDaccessor)
      if (flags is CaseAccessor) writeByte(CASEaccessor)
      if (flags is DefaultParameterized) writeByte(DEFAULTparameterized)
      if (flags is Stable) writeByte(STABLE)
    } else {
      if (flags is Sealed) writeByte(SEALED)
      if (flags is Abstract) writeByte(ABSTRACT)
      if (flags is Trait) writeByte(TRAIT)
      if (flags is Covariant) writeByte(COVARIANT)
      if (flags is Contravariant) writeByte(CONTRAVARIANT)
    }
    sym.annotations.foreach(pickleAnnotation(sym, _))
  }

  private def isUnpicklable(owner: Symbol, ann: Annotation)(implicit ctx: Context) = ann match {
    case Annotation.Child(sym) => sym.isInaccessibleChildOf(owner)
      // If child annotation refers to a local class or enum value under
      // a different toplevel class, it is impossible to pickle a reference to it.
      // Such annotations will be reconstituted when unpickling the child class.
      // See tests/pickling/i3149.scala
    case _ => ann.symbol == defn.BodyAnnot
      // inline bodies are reconstituted automatically when unpickling
  }

  def pickleAnnotation(owner: Symbol, ann: Annotation)(implicit ctx: Context) =
    if (!isUnpicklable(owner, ann)) {
      writeByte(ANNOTATION)
      withLength { pickleType(ann.symbol.typeRef); pickleTree(ann.tree) }
    }

  def pickle(trees: List[Tree])(implicit ctx: Context) = {
    trees.foreach(tree => if (!tree.isEmpty) pickleTree(tree))
    def missing = forwardSymRefs.keysIterator.map(_.showLocated).toList
    assert(forwardSymRefs.isEmpty, i"unresolved symbols: $missing%, % when pickling ${ctx.source}")
  }

  def compactify() = {
    buf.compactify()

    def updateMapWithDeltas(mp: MutableSymbolMap[Addr]) =
      for (key <- mp.keysIterator.toBuffer[Symbol]) mp(key) = adjusted(mp(key))

    updateMapWithDeltas(symRefs)
  }
}
