package scala.meta.internal.hosts.dotty
package reflect

import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

import dotty.tools.dotc.core.Contexts.{Context => DottyContext}
import dotty.tools.dotc.{util => dut}
import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.TreeInfo
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Constants.Constant

import dotty.tools.dotc.ast.tpd.modsDeco // FIXME not used because we have our own modsDeco

trait LogicalTrees[A >: dtr.Untyped <: dty.Type] extends TreeInfo[A] {
  self: ReflectToolkit[A] =>

  type tpdTree = dtr.Tree[dty.Type]

  val IsLParentAttachment = new dut.Attachment.Key[Boolean]
  val ParentAttachment = new dut.Attachment.Key[g.Tree]
  val IndexAttachment = new dut.Attachment.Key[Int]

  // NOTE: The idea behind LogicalTrees is to provide a layer that undoes
  // anti-syntactic ensugarings and encodings of scalac (i.e. ones that make scala.reflect trees
  // lose resemblance to the original syntax of the program that they are modeling).
  //
  // The motivation for existence of this layer is the desire to make
  // the scala.reflect > scala.meta converter as modular as possible.
  // It turns out that it's really easy to turn the converter into spaghetti,
  // so we need to be vigilant towards this danger. The approach that I like the most for now
  // is to split the converter into: 1) LogicalTrees, 2) ToMtree.
  // The former is this file, the latter is a trivial pattern match of the form:
  // `case l.Something(a, b, c) => m.Something(convert(a), convert(b), convert(c))`.
  //
  // Now there can be multiple ways of exposing the extractors for ToMtree,
  // but here I'm opting for one that has the lowest overhead:
  // no intermediate data structures are created unless absolutely necessary,
  // so the only thing that we have to do is to write Something.unapply methods and that's it
  // (later on, we might optimize them to use name-based pattern matching ala Dmitry's backend interface).
  //
  // Guidelines for creating extractors:
  //
  // 1) Most m.Something nodes will need one or, much more rarely, more l.Something extractors that
  //    should mirror the fields that m.Something accepts. For instance, the l.ClassDef extractor
  //    returns not the 4 fields that are present in g.ClassDef (mods, name, tparams, impl),
  //    but rather the 5 fields that are present in m.Defn.Class (mods, name, tparams, ctor, template).
  //
  // 2) Intermediate data structures should only be created when the correspoding scala.reflect concept
  //    can't be modelled as a tree (and therefore can't be processed in a modular fashion, because
  //    it can't have attachments, which at the very least means that it can't have parent links).
  //    E.g. we don't have `case class PrimaryCtorDef(...)`, because all the necessary information
  //    can be figured out from the corresponding g.DefDef (possibly requiring navigation through parent links).
  //    But we do have `case class Modifiers(...)`, because g.Modifiers isn't a tree, so we can't really
  //    adorn it with additional metadata that will help the converter.
  //
  // 3) Since we generally don't create intermediate data structures, the unapply methods in extractors
  //    should have comments that explain the fields that they are extracting.
  //
  // 4) Extractors and supporting intermediate data structures should be created in the same order
  //    that the corresponding AST nodes in scala/meta/Trees.scala are in.
  //
  // Source code that one might find helpful in implementing extractors:
  //
  // 1) Ensugar.scala (the resugaring module of the previous implementation of scalahost).
  //    Contains several dozen clearly modularized recipes for resugarings.
  //    Want to know what synthetic members are generated for lazy abstract vals?
  //    What about default parameters on class constructors? It's all there.
  //    https://github.com/scalameta/scalahost/blob/92b65b841685871b4401f00456a25de2b7a177b6/foundation/src/main/scala/org/scalameta/reflection/Ensugar.scala
  //
  // 2) ToMtree.scala (the converter module of the previous implementation of scalahost).
  //    Transforms scala.reflect's encodings of Scala syntax into scala.meta.
  //    Contains knowledge how to collapse multipart val/var definitions and other related stuff.
  //    https://github.com/scalameta/scalahost/blob/92b65b841685871b4401f00456a25de2b7a177b6/interface/src/main/scala/scala/meta/internal/hosts/scalac/converters/ToMtree.scala
  //
  // 3) ReificationSupport.scala (the quasiquote support module of scala/scala)
  //    Contains various SyntacticXXX extractors that do things similar to our l.XXX extractors.
  //    https://github.com/scala/scala/blob/1fbce4612c21a4d0c553ea489b4765494828c09f/src/reflect/scala/reflect/internal/ReificationSupport.scala
  trait LogicalTrees { l: self.l.type =>
    // ============ NAMES ============

    case class Denotation(pre: dty.Type, sym: l.Symbol) {
      def isEmpty = pre == dty.NoType || sym == l.Zero
      def nonEmpty = !isEmpty
    }
    implicit class RichDenotationTree(tree: g.NameTree) {
      def metaDenot = tree match {
        case tree: g.RefTree =>
          val qual = tree.qualifier
          if (qual.isEmpty)
            l.Denotation(tree.symbol.prefix, tree.symbol.toLogical)
          else
            l.Denotation(if (qual.tpe != null) qual.tpe else dty.NoType, tree.symbol.toLogical)
        case tree: g.DefTree => l.Denotation(tree.symbol.prefix, tree.symbol.toLogical)
        case _ => unreachable(debug(tree, tree.show))
      }
    }

    trait Name extends dtr.Tree[dty.Type] { def denot: l.Denotation }

    case class AnonymousName(denot: l.Denotation) extends Name with TermParamName with TypeParamName with QualifierName
    object AnonymousName {
      def apply(pre: dty.Type, sym: l.Symbol): l.AnonymousName = apply(l.Denotation(pre, sym))
    }

    case class IndeterminateName(denot: l.Denotation, value: String) extends Name with QualifierName
    object IndeterminateName {
      def apply(pre: dty.Type, sym: l.Symbol, value: String): l.IndeterminateName = {
        l.IndeterminateName(l.Denotation(pre, sym), value)
      }
    }

    trait QualifierName extends Name

    // ============ TERMS ============

    object TermThis {
      def unapply(tree: g.This): Option[l.QualifierName] = {
        ???
      }
    }

    case class TermName(val denot: l.Denotation, val value: String) extends Name with TermParamName
    object TermName {
      def apply(tree: g.NameTree) = {
        require(tree.name.isTermName && tree.name != nme.WILDCARD && tree.name != nme.CONSTRUCTOR && debug(tree, tree.show))
        new l.TermName(tree.metaDenot, tree.displayName).setType(tree.tpe)
      }
    }

    object TermIdent {
      def unapply(tree: g.Ident): Option[g.Tree] = tree match {
        case dtr.Ident(name) =>
          val ldenot = l.Denotation(tree.symbol.prefix, tree.symbol.toLogical)
          Some(l.TermName(ldenot, name.toString).setParent(tree).setType(tree.tpe))
        case _ =>
          None
      }
    }

    object TermSelect {
      def unapply(tree: g.Select): Option[(g.Tree, l.TermName)] = {
        val dtr.Select(qual, name) = tree
        if (name.isTypeName) return None
        Some((qual, l.TermName(tree).setParent(tree)))
      }
    }

    object TermApply {
      def unapply(tree: g.Apply): Option[(g.Tree, List[g.Tree])] = {
        if (tree.getAttachment(IsLParentAttachment).isDefined) return None
        Some((tree.fun, tree.args))
      }
    }

    trait TermParamName extends Name

    object TermParamDef {
      // mods, pats, tpt, default
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], l.TermName, g.Tree, g.Tree)] = {
        val default = tree.rhs // default?
        if (tree.name == nme.WILDCARD || tree.name.startsWith(nme.EVIDENCE_PARAM_PREFIX)) return None
        Some((l.Modifiers(tree), l.TermName(tree).setParent(tree), tree.tpt, default))
      }}


    // ============ TYPES ============

    object TypeTree {
      def unapply(tpt: g.TypeTree): Option[dty.Type] = {
        if (!tpt.isEmpty) Some((tpt.tpe)) else None
      }
    }

    case class TypeName(val denot: l.Denotation, val value: String) extends Name with TypeParamName
    object TypeName {
      def apply(tree: g.NameTree): l.TypeName = {
        require(tree.name.isTypeName && tree.name != nme.WILDCARD && debug(tree, tree.show))
        new l.TypeName(tree.metaDenot, tree.displayName)
      }
    }

    object TypeIdent {
      def unapply(tree: g.Tree): Option[l.TypeName] = tree match {
        case tree @ dtr.Ident(name) =>
          val ldenot = l.Denotation(dty.NoPrefix, tree.symbol.toLogical)
          Some(l.TypeName(ldenot, name.toString).setParent(tree))
        case _ =>
          None
      }
    }

    object TypeSelect {
      def unapply(tree: g.Select): Option[(g.Tree, l.TypeName)] = {
        val dtr.Select(qual, name) = tree
        if (name.isTermName) return None
        Some((qual, l.TypeName(tree).setParent(tree)))
      }
    }

    trait TypeParamName extends Name

    object TypeParamDef {
      // mods, name, tparams, typeBounds, viewBounds, contextBounds
      def unapply(tree: g.TypeDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.TypeBoundsTree, g.Tree, g.Tree)] = {
        ???
      }
    }

    // ============ DECLS ============

    object AbstractValDef {
      // mods, pats, tpt
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[l.TermName], g.Tree)] = {
        ???
      }
    }

    object AbstractVarDef {
      // mods, pats, tpt
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[l.TermName], g.Tree)] = {
        ???
      }
    }

    object AbstractDefDef {
      // mods, name, tparams, paramss, tpt
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.TermName, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    object AbstractTypeDef {
      // mods, name, tparams, typeBounds
      def unapply(tree: g.TypeDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.TypeBoundsTree)] = {
        ???
      }
    }

    // ============ PATTERNS ============

    // ============ LITERALS ============

    object Literal {
      // value
      def unapply(tree: g.Literal): Option[Any] = tree match {
        case dtr.Literal(Constant(_: dty.Type)) => None
        case dtr.Literal(Constant(_: dsy.Symbol)) => None
        case dtr.Literal(Constant(value)) => Some(value)
        case _ => None
      }
    }

    // ============ DEFNS ============

    object ValDef {
      // mods, pats, tpt, rhs
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[g.Tree], g.Tree, g.Tree)] = {
        ???
      }
    }

    object VarDef {
      // mods, pats, tpt, rhs
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[g.Tree], g.Tree, g.Tree)] = {
        ???
      }
    }

    object DefDef {
      // mods, name, tparams, paramss, tpt, rhs
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.TermName, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        val dtr.DefDef(_, tparams, paramss, tpt, _) = tree
        val rhs = tree.rhs
        if (tree.name == nme.CONSTRUCTOR || rhs.isEmpty || tree.symbol.is(Flags.Macro)) return None
        val ltparams = applyBounds(tparams, paramss)
        val lparamss = removeBounds(paramss)
        Some((l.Modifiers(tree), l.TermName(tree).setParent(tree), ltparams, paramss, tpt, rhs))
      }
    }

    object MacroDef {
      // mods, name, tparams, paramss, tpt, rhs
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.TermName, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    object TypeDef {
      // mods, name, tparams, rhs
      def unapply(tree: g.TypeDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.Tree)] = {
        ???
      }
    }

    object ClassDef {
      // mods, name, tparams, primaryCtor, template
      def unapply(tree: g.TypeDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.DefDef, g.Template)] = {
        tree match {
          case dtr.TypeDef(_, templ @ dtr.Template(primaryCtor, _, _, _)) if !tree.symbol.is(Flags.Trait) =>
            //val dtr.ClassDef(_, _, classTparams, templ @ dtr.Template(_, _, body)) = tree
            val body = templ.body
            val classTparams = tree.tpe.typeParams.map(tpd.TypeDef)
            val ltparams = applyBounds(classTparams, primaryCtor.vparamss)
            Some((l.Modifiers(tree), l.TypeName(tree).setParent(tree), ltparams, primaryCtor, templ))
          case _ =>
            None
        }
      }
    }

    // ============ PKGS ============

    object EmptyPackageDef {
      // stats
      def unapply(tree: g.PackageDef): Option[List[g.Tree]] = {
        if (tree.symbol.isEmptyPackage) {
          require(tree.parent.isEmpty)
          Some(tree.stats)
        } else {
          None
        }
      }
    }

    object ToplevelPackageDef {
      // pid, stats
      def unapply(tree: g.PackageDef): Option[(l.TermName, List[g.Tree])] = {
        if (!tree.symbol.isEmptyPackage && tree.parent.isEmpty) {
          Some((l.TermName(tree.pid).setParent(tree), tree.stats))
        } else {
          None
        }
      }
    }

    object NestedPackageDef {
      // pid, stats
      def unapply(tree: g.PackageDef): Option[(l.TermName, List[g.Tree])] = {
        if (!tree.symbol.isEmptyPackage && !tree.parent.isEmpty) {
          Some((l.TermName(tree.pid).setParent(tree), tree.stats))
        } else {
          None
        }
      }
    }

    object PackageModuleDef {
      // mods, name, primaryCtor, template
      def unapply(tree: g.PackageDef): Option[(List[l.Modifier], l.TermName, g.DefDef, g.Template)] = ???
    }

    // ============ CTORS ============

    object PrimaryCtorDef {
      // mods, name, paramss
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.CtorName, List[List[g.ValDef]])] = {
        if (tree.symbol.isPrimaryConstructor) {
          val lname = l.CtorName(tree).setParent(tree)
          val paramss = tree.vparamss
          Some((Nil, lname, paramss))
        } else {
          None
        }
      }
    }

    object SecondaryCtorDef {
      // mods, name, paramss, stats
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.CtorName, List[List[g.ValDef]], List[g.Tree])] = ???
    }

    case class CtorName(denot: l.Denotation, value: String) extends Name
    object CtorName {
      def apply(ctorDef: g.DefDef): l.CtorName = {
        require(ctorDef.name == nme.CONSTRUCTOR)
        l.CtorName(ctorDef.metaDenot, ctorDef.symbol.enclosingClass.displayName).setType(ctorDef.tpe)
      }
    }

    case class CtorIdent(name: l.CtorName) extends g.Tree
    object CtorIdent {
      def apply(ctorSym: l.Symbol, classRef: g.RefTree): l.CtorIdent = {
        // NOTE: We can't use the commented snippet of code,
        // because then we'll end up in a really strange place when type aliases are involved.
        // Consider the `AnyRef` ctorident in `class C extends scala.AnyRef`.
        // We would like to emit `Ctor.Ref.Name("AnyRef")`, but what denotation should we assign to it?
        // The symbol is obvious: `java.lang#Object.<init>()V`, but the prefix is harder than it looks.
        // The knee-jerk reaction would be not to dealias and take the prefix of `scala.AnyRef`, which is `scala`.
        // But then we'd end up with `scala::java.lang#Object.<init>()V` which doesn't make any sense.
        // Hence here we actually have to disregard the prefix (i.e. inheritance and type aliases) and
        // simply go for the owner of the symbol.
        // val lpre = ctorRef.qualifier.tpe.prefix.orElse(dty.NoPrefix)
        val gctor = ctorSym.gsymbol
        val ldenot = l.Denotation(gctor.owner.prefix, ctorSym)
        val lname = l.CtorName(ldenot, classRef.displayName).setType(gctor.info)
        val lresult = l.CtorIdent(lname)
        lname.setParent(lresult)
        lresult
      }
    }

    // ============ TEMPLATES ============

    object Template {
      // early, parents, self, stats
      def unapply(tree: g.Template): Some[(List[g.Tree], List[g.Tree], g.ValDef, List[g.Tree])] = {
        val dtr.Template(_, parents, lself, _) = tree
        val stats = tree.body
        val lparents = parents.zipWithIndex.map{ case (parent, i) =>
          parent.setParent(tree)
        }
        val lstats = removeSyntheticDefinitions(stats)
        Some((Nil, lparents, lself, lstats))
      }
    }

    object Parent {
      // tpt, ctor, argss
      def unapply(tree: g.Tree)
        : Option[(g.Tree, l.CtorIdent, List[List[g.Tree]])] = {
        //if (tree.getAttachment(IsLParentAttachment).isDefined) { // "isLparent" in scalahost
        if (true) {
          val applied = dissectApplied(tree)
            (applied.callee, applied.core, applied.argss) match {
            case (dtr.Select(dtr.New(tpt: g.TypeTree), nme.CONSTRUCTOR), classRef: g.RefTree, argss) =>
              //val ctorSym = tree.metadata.get("superCtor").map(_.require[l.Symbol]).getOrElse(l.Zero)
              val ctorSym = tree.symbol.toLogical
              val ctor = l.CtorIdent(ctorSym, classRef).setParent(tree.parent)
              Some((tpt, ctor, argss))
            case _ =>
              None
          }
        } else {
          None
        }
      }
    }

    object SelfDef {
      // name, tpt
      def unapply(tree: g.ValDef): Option[(l.TermParamName, g.Tree)] = {
        // if (tree.parent.isInstanceOf[g.Template] && tree.index == -1) {
        if (tree == g.EmptyValDef || tree.symbol.isSelfSym) {
          val isAnonymous = tree.name == nme.WILDCARD
          val ldenot = {
            val lmdef = tree.parent.parent.require[g.TypeDef]
            val lsym = if (lmdef.symbol.exists) l.Self(lmdef.symbol) else l.Zero
            l.Denotation(lmdef.symbol.thisType, lsym)

            //val csym = tree.symbol.enclosingClass
            //val lsym = l.Self(csym)
            //l.Denotation(csym.thisType, lsym)
          }
          val lvalue = if (isAnonymous) "this" else tree.displayName
          val lname = {
            if (isAnonymous) l.AnonymousName(ldenot).setParent(tree)
            else l.TermName(ldenot, lvalue).setParent(tree)
          }
          Some((lname, tree.tpt))
        } else {
          None
        }
      }
    }

    // ============ MODIFIERS ============

    trait Modifier extends g.Tree
    case class Private(within: g.Tree) extends Modifier // TODO: `within: l.QualifierName`
    case class Protected(within: g.Tree) extends Modifier // TODO: `within: l.QualifierName`
    case class Implicit() extends Modifier
    case class Final() extends Modifier
    case class Sealed() extends Modifier
    case class Override() extends Modifier
    case class Case() extends Modifier
    case class Abstract() extends Modifier
    case class Covariant() extends Modifier
    case class Contravariant() extends Modifier
    case class Lazy() extends Modifier
    case class ValParam() extends Modifier
    case class VarParam() extends Modifier

    object Modifiers {
      def apply(tree: g.MemberDef): List[l.Modifier] = Nil
    }

    private def aggregateAnnotations(mdef: g.MemberDef): List[l.Annotation] = Nil

    case class Annotation(val tree: g.Tree) extends Modifier
    // object Annotation {
    //   def apply(info: g.AnnotationInfo): l.Annotation = {
    //     // TODO: set Annotation.tree's parent to g.EmptyTree
    //     ???
    //   }
    // }

    // ============ ODDS & ENDS ============

    // ============ HELPERS ============

    private def applyBounds(tparams: List[g.TypeDef], paramss: List[List[g.ValDef]]): List[g.TypeDef] = {
      // def tparam(targ: Tree): Option[g.TypeDef] = tparams.filter(tparam => {
      //   if (tparam.symbol != g.NoSymbol) tparam.symbol == targ.symbol
      //   else targ match { case g.Ident(name) => name == tparam.name; case _ => false }
      // }).headOption

      // object ContextBound {
      //   def unapply(tree: g.ValDef): Option[(g.TypeDef, g.Tree)] = tree match {
      //     case dtr.ValDef(_, _, tpt @ dty.TypeTree(), _) =>
      //       val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else g.NoSymbol
      //       val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
      //       val targ = targs.map(_.typeSymbol) match { case List(sym) => sym; case _ => g.NoSymbol }
      //       tparam(Ident(targ)).map(tparam => (tparam, Ident(tycon)))
      //     case dtr.ValDef(_, _, g.AppliedTypeTree(tycon, targ :: Nil), _) =>
      //       tparam(targ).map(tparam => (tparam, tycon))
      //     case _ =>
      //       None
      //   }
      // }

      // val (explicitss, implicitss) = paramss.partition(_.exists(_.mods.hasFlag(IMPLICIT)))
      // val (bounds, implicits) = implicitss.flatten.partition(_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
      // tparams.map(tparam => {
      //   val contextBounds = bounds.flatMap(ContextBound.unapply).filter(_._1.name == tparam.name).map(_._2)
      //   tparam.appendMetadata("contextBounds" -> contextBounds)
      // })
      tparams
    }

    private def removeBounds(paramss: List[List[g.ValDef]]): List[List[g.ValDef]] = {
      // val init :+ last = paramss
      // val hasImplicits = last.exists(_.mods.hasFlag(IMPLICIT))
      // val explicitss = if (hasImplicits) init else init :+ last
      // val implicits = if (hasImplicits) last else Nil
      // val limplicits = implicits.filter(_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
      // if (limplicits.nonEmpty) explicitss :+ limplicits else explicitss
      paramss
    }

    private def removeSyntheticDefinitions(stats: List[g.Tree]): List[g.Tree] = {
      // TODO: relevant synthetics are described in:
      // * subclasses of MultiEnsugarer: DefaultGetter, VanillaAccessor, AbstractAccessor, LazyAccessor
      // * ToMtree.mstats: PatDef, InlinableHoistedTemporaryVal
      stats
    }
  }

  // NOTE: This is a mandatory piece of infrastructure that enriches scala.reflect trees
  // with information about parents and positions within their siblings.
  // As noted in ToMtree, it would be nice to figure out a mechanism to make this performance-neutral,
  // because currently we need two traversals (one to add stuff and one to remove stuff)
  // in order to transparently provide this functionality.
  implicit class RichFoundationNavigableTree[T <: g.Tree](tree: T) {
    def setType(tpe: dty.Type): T = tree.withType(tpe).asInstanceOf[T]
    def parent: g.Tree = tree.attachmentOrElse(ParentAttachment, g.EmptyTree)
    def parents: List[g.Tree] = {
      def loop(tree: g.Tree, acc: List[g.Tree]): List[g.Tree] = {
        val parent = tree.getAttachment(ParentAttachment)
        val recursion = parent.map(parent => {
          if (!parent.isEmpty) loop(parent, acc :+ parent)
          else acc
        })
        recursion.getOrElse(acc)
      }
      loop(tree, Nil)
    }
    def hierarchy: List[g.Tree] = tree +: parents
    def setParent(parent: g.Tree): T = {
      tree.removeAttachment(ParentAttachment)
      tree.pushAttachment(ParentAttachment, parent)
      tree
    }
    def index: Int = tree.attachmentOrElse(IndexAttachment, -1)
    def setIndex(index: Int): T = {
      tree.removeAttachment(IndexAttachment)
      tree.pushAttachment(IndexAttachment, index)
      tree
    }
    def installNavigationLinks(): Unit = {
      object installNavigationLinks extends g.TreeTraverser {
        private var parents = List[g.Tree]()
        override def traverse(tree: g.Tree)(implicit ctx: DottyContext): Unit = {
          if (tree.getAttachment(ParentAttachment).isDefined) return
          tree.setParent(parents.headOption.getOrElse(g.EmptyTree))
          parents = tree :: parents
          traverseChildren(tree)
          parents = parents.tail
        }
      }
      // NOTE: If you get a MatchError in xtraverse here,
      // it means that one of the custom logical trees declared below in this file
      // was created without its parent set (i.e. its tree.getAttachment(ParentAttachment).isDefined is false).
      // This should be fixed at the point where the logical tree is created, not here.
      installNavigationLinks.traverse(tree)
    }
    def removeNavigationLinks(): Unit = {
      object removeNavigationLinks extends g.TreeTraverser {
        override def traverse(tree: g.Tree)(implicit ctx: DottyContext): Unit = {
          if (!tree.getAttachment(ParentAttachment).isDefined
            && !tree.getAttachment(IndexAttachment).isDefined) return
          tree.removeAttachment(ParentAttachment)
          tree.removeAttachment(IndexAttachment)
          traverseChildren(tree)
        }
      }
      // NOTE: If you get a MatchError in xtraverse here,
      // it means that one of the custom logical trees declared below in this file
      // has escaped the confines of the scala.reflect > scala.meta converter
      // and tries to become part of the compilation pipeline.
      // This should be fixed at the point where the logical tree ends up in the output, not here
      removeNavigationLinks.traverse(tree)
    }
  }
}
