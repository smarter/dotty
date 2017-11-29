package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import Annotations._
import StdNames._
import NameOps._
import NameKinds.{ExpandedName, ImplMethName, TraitSetterName}
import ast.Trees._

/** This phase augments Scala2 traits with implementation classes and with additional members
 *  needed for mixin composition.
 *  These symbols would have been added between Unpickling and Mixin in the Scala2 pipeline.
 *  Specifcally, it adds
 *
 *   - an implementation class which defines a trait constructor and trait method implementations
 *   - trait setters for vals defined in traits
 *
 *  Furthermore, it expands the names of all private getters and setters as well as super accessors in the trait and makes
 *  them not-private.
 */
class AugmentScala2Traits extends MiniPhase with IdentityDenotTransformer with FullParameterization { thisPhase =>
  import ast.tpd._

  override def changesMembers = true

  override def phaseName: String = "augmentScala2Traits"

  override def rewiredTarget(referenced: Symbol, derived: Symbol)(implicit ctx: Context) = NoSymbol

  override def transformTemplate(impl: Template)(implicit ctx: Context) = {
    val cls = impl.symbol.owner.asClass
    for (mixin <- cls.mixins)
      if (mixin.is(Scala2x))
        augmentScala2Trait(mixin, cls)
    impl
  }

  private def augmentScala2Trait(mixin: ClassSymbol, cls: ClassSymbol)(implicit ctx: Context): Unit = {
      val ops = new MixinOps(cls, thisPhase)
      import ops._

      def info_2_12(tp: Type) = tp match {
        case mt @ MethodType(paramNames @ nme.SELF :: _) =>
          // 2.12 seems to always assume the enclsing mixin class as self type parameter,
          // whereas 2.11 used the self type of this class instead.
          val selfType :: otherParamTypes = mt.paramInfos
          MethodType(paramNames, mixin.typeRef :: otherParamTypes, mt.resType)
        case info => info
      }

      def implMethod(meth: TermSymbol): Symbol = {
        val mold =
          if (meth.isConstructor)
            meth.copySymDenotation(
              name = nme.TRAIT_CONSTRUCTOR,
              info = MethodType(Nil, defn.UnitType))
          else meth.ensureNotPrivate
        meth.copy(
          owner = mixin,
          name = if (meth.isConstructor) mold.name.asTermName else ImplMethName(mold.name.asTermName),
          flags = Method | JavaStatic,
          info = info_2_12(fullyParameterizedType(mold.info, mixin)))
      }

      def traitSetter(getter: TermSymbol) =
        getter.copy(
          name = getter.ensureNotPrivate.name
                  .expandedName(getter.owner, TraitSetterName)
                  .asTermName.setterName,
          flags = Method | Accessor,
          info = MethodType(getter.info.resultType :: Nil, defn.UnitType))

      for (sym <- mixin.info.decls) {
        if (needsForwarder(sym) || sym.isConstructor || sym.isGetter && sym.is(Lazy) || sym.is(Method, butNot = Deferred))
          mixin.enter(implMethod(sym.asTerm))
        if (sym.isGetter)
          if (sym.is(Lazy)) {
            if (!sym.hasAnnotation(defn.VolatileAnnot))
              sym.addAnnotation(Annotation(defn.VolatileAnnot, Nil))
          }
          else if (!sym.is(Deferred) && !sym.setter.exists &&
                   !sym.info.resultType.isInstanceOf[ConstantType])
            traitSetter(sym.asTerm).enteredAfter(thisPhase)
        if ((sym.is(PrivateAccessor) && !sym.name.is(ExpandedName) &&
          (sym.isGetter || sym.isSetter)) // strangely, Scala 2 fields are also methods that have Accessor set.
          || sym.isSuperAccessor) // scala2 superaccessors are pickled as private, but are compiled as public expanded
          sym.ensureNotPrivate.installAfter(thisPhase)
      }
      mixin.setFlag(Scala_2_12_Augmented)
  }
}
