/* sbt -- Simple Build Tool
 * Copyright 2008, 2009, 2010, 2011 Mark Harrah
 */
package dotty.tools.dotc
package sbt

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Flags._, Trees._, Types._, Names._, StdNames._, Symbols._
import transform.SymUtils._
import transform.ValueClasses.isDerivedValueClass
import Phases.Phase

import java.io.File

// import scala.tools.nsc._
import scala.tools.nsc.io.Path
// import symtab.Flags

import java.util.{Arrays, Comparator}
import scala.collection.mutable.{HashMap, HashSet, ListBuffer}

import xsbti.DependencyContext
import xsbti.api.{ClassLike, DefinitionType, PathComponent, SimpleType}
import xsbt.api.DefaultShowAPI

// THIS IS A QUICK HACK to experiment with integrating sbt's compiler interface into the compiler
// it consolidates sbt's incremental compilation traversal logic into one traversal
// it does not implement the logic that calls `callback.generatedClass` in the back-end (should be easy)
//
// to make this work, I just imported the needed xsbti.* sources from sbt,
// but we should really link against the sbt artifact that's provided on the classpath
// at compile time, we should resolve the sbt interface that this scala version is compatible with

object SbtApi {
  import tpd._

  class ApiPhase extends Phase {

    override def phaseName: String = "sbt-api"

    override def run(implicit ctx: Context): Unit = {
      val unit = ctx.compilationUnit
      if (!unit.isJava) {
        val sourceFile = unit.source.file.file
        ctx.log("Traversing " + sourceFile)
        val apiTraverser = new ExtractDependenciesTraverser(sourceFile)
        apiTraverser(unit.tpdTree)

        val source = new xsbti.api.SourceAPI(apiTraverser.packages.toArray.map(new xsbti.api.Package(_)), apiTraverser.defs.toArray)
        apiTraverser.forceStructures()

        val names   = apiTraverser.usedNames.map(_.toString).toArray[Object]
        val deps    = apiTraverser.topLevelDependencies.map(_.toString).toArray[Object]
        val inhDeps = apiTraverser.topLevelInheritanceDependencies.map(_.toString).toArray[Object]
        Arrays.sort(names)
        Arrays.sort(deps)
        Arrays.sort(inhDeps)

        val pw = Path(sourceFile).changeExtension("api").toFile.printWriter()
        try {
          pw.println(s"// usedNames: ${names.mkString(",")}")
          pw.println(s"// topLevelDependencies: ${deps.mkString(",")}")
          pw.println(s"// topLevelInheritanceDependencies: ${inhDeps.mkString(",")}")
          pw.println()
          pw.println(DefaultShowAPI(source))
        } finally pw.close()

        /*
        if (callback != null) {
          callback.api(sourceFile, source)
          apiTraverser.usedNames.foreach(callback.usedName(sourceFile, _))
          apiTraverser.topLevelDependencies foreach processDependency(sourceFile, DependencyContext.DependencyByMemberRef)
          apiTraverser.topLevelInheritanceDependencies foreach processDependency(sourceFile, DependencyContext.DependencyByInheritance)
        }
        */
      }
    }

    /**
      * Handles dependency on given symbol by trying to figure out if represents a term
      * that is coming from either source code (not necessarily compiled in this compilation
      * run) or from class file and calls respective callback method.
      */
    /*
    def processDependency(sourceFile: File, context: DependencyContext)(on: Symbol) = {
      def binaryDependency(file: File, className: String) = callback.binaryDependency(file, className, sourceFile, context)
      val onSource = on.sourceFile
      if (onSource == null) {
        classFile(on) match {
          case Some((f, className, inOutDir)) =>
            if (inOutDir && on.isJavaDefined) registerTopLevelSym(on)
            f match {
              case ze: ZipArchive#Entry => for (zip <- ze.underlyingSource; zipFile <- Option(zip.file)) binaryDependency(zipFile, className)
              case pf: PlainFile => binaryDependency(pf.file, className)
              case _ => ()
            }
          case None => ()
        }
      } else if (onSource.file != sourceFile)
        callback.sourceDependency(onSource.file, sourceFile, context)
    }

    private[this] final val classSeparator = '.'
    protected def classFile(sym: Symbol): Option[(AbstractFile, String, Boolean)] =
    // package can never have a corresponding class file; this test does not
    // catch package objects (that do not have this flag set)
      if (sym hasFlag scala.tools.nsc.symtab.Flags.PACKAGE) None
      else {
        val name = flatname(sym, classSeparator) + sym.moduleSuffix
        sbtGlobal.findClass(name).map { case (file, inOut) => (file, name, inOut) } orElse {
          if (isTopLevelModule(sym)) {
            val linked = sym.companionClass
            if (linked == NoSymbol)
              None
            else
              classFile(linked)
          } else
            None
        }
      }

    private def flatname(s: Symbol, separator: Char) = exitingFlatten { s fullName separator }
    protected def isTopLevelModule(sym: Symbol): Boolean = exitingPickler { sym.is(ModuleClass) && !sym.is(ImplClass) && !sym.isNestedClass }
    */
  }


  /**
    * Extracts full (including private members) API representation out of Symbols and Types.
    *
    * Each compilation unit should be processed by a fresh instance of this class.
    *
    * This class depends on instance of CallbackGlobal instead of regular Global because
    * it has a call to `addInheritedDependencies` method defined in CallbackGlobal. In the future
    * we should refactor this code so inherited dependencies are just accumulated in a buffer and
    * exposed to a client that can pass them to an instance of CallbackGlobal it holds.
    *
    * NOTE: This class extract *full* API representation. In most of other places in the incremental compiler,
    * only non-private (accessible from other compilation units) members are relevant. Other parts of the
    * incremental compiler filter out private definitions before processing API structures. Check SameAPI for
    * an example.
    *
    */
  private trait ExtractAPI {
    val sourceFile: File

    val packages = new HashSet[String]
    val defs     = new ListBuffer[xsbti.api.Definition]

    // this cache reduces duplicate work both here and when persisting
    //   caches on other structures had minimal effect on time and cache size
    //   (tried: Definition, Modifier, Path, Id, String)
    private[this] val typeCache      = new HashMap[(Symbol, Type), xsbti.api.Type]
    // these caches are necessary for correctness
    private[this] val structureCache = new HashMap[Symbol, xsbti.api.Structure]
    private[this] val classLikeCache = new HashMap[(Symbol, Symbol), xsbti.api.ClassLike]
    private[this] val pending        = new HashSet[xsbti.api.Lazy[_]]

    private[this] val emptyStringArray = new Array[String](0)

    /**
      * Force all lazy structures.  This is necessary so that we see the symbols/types at this phase and
      * so that we don't hold on to compiler objects and classes
      */
    def forceStructures(): Unit =
      if (pending.isEmpty)
        structureCache.clear()
      else {
        val toProcess = pending.toList
        pending.clear()
        toProcess foreach {_.get()}
        forceStructures()
      }

    // TODO: could we restrict ourselves to classes, ignoring the term symbol for modules,
    // since everything we need to track about a module is in the module's class (`moduleSym.moduleClass`)?
    def isClass(s: Symbol)(implicit ctx: Context) = s.isClass// || s.is(ModuleVal)

    // necessary to ensure a stable ordering of classes in the definitions list:
    //  modules and classes come first and are sorted by name
    // all other definitions come later and are not sorted
    def sortClasses(implicit ctx: Context) = new Comparator[Symbol] {
      def compare(a: Symbol, b: Symbol) = {
        val aIsClass = isClass(a)
        val bIsClass = isClass(b)
        if (aIsClass == bIsClass)
          if (aIsClass)
            if (a.is(Module) == b.is(Module))
              a.fullName.toString.compareTo(b.fullName.toString)
            else if (a.is(Module))
              -1
            else
              1
          else
            0 // substantial performance hit if fullNames are compared here
        else if (aIsClass)
          -1
        else
          1
      }
    }

    // call back to the xsbti.SafeLazy class in main sbt code to construct a SafeLazy instance
    //   we pass a thunk, whose class is loaded by the interface class loader (this class's loader)
    //   SafeLazy ensures that once the value is forced, the thunk is nulled out and so
    //   references to the thunk's classes are not retained.  Specifically, it allows the interface classes
    //   (those in this subproject) to be garbage collected after compilation.
    val safeLazy = Class.forName("xsbti.SafeLazy").getMethod("apply", classOf[xsbti.F0[_]])

    object Constants {
      val local          = new xsbti.api.ThisQualifier
      val public         = new xsbti.api.Public
      val privateLocal   = new xsbti.api.Private(local)
      val protectedLocal = new xsbti.api.Protected(local)
      val unqualified    = new xsbti.api.Unqualified
      val emptyPath      = new xsbti.api.Path(Array())
      val thisPath       = new xsbti.api.This
      val emptyType      = new xsbti.api.EmptyType
    }

    def extractAPI(tree: Tree)(implicit ctx: Context) = {
      tree match {
        //case (_: ClassDef | _: ModuleDef) if isTopLevel(tree.symbol) => defs += classLike(tree.symbol.owner, tree.symbol)
        case (_: TypeDef) if isTopLevel(tree.symbol) => defs += classLike(tree.symbol.owner, tree.symbol)
        case p: PackageDef => pkg(p.symbol)
        case _ =>
      }

    def isTopLevel(sym: Symbol): Boolean =
      (sym ne null) && (sym != NoSymbol) && !sym.is(ImplClass | Synthetic | JavaDefined) &&
      sym.isStatic && sym.owner.is(PackageClass)

    /** Record packages declared in the source file */
    def pkg(p: Symbol): Unit =
      if (!((p eq null) || p == NoSymbol || p.isEffectiveRoot || p.isEmptyPackage)) {
        packages += p.fullName.toString
        pkg(p.owner)
      }

    def lzy[S <: AnyRef](s: => S): xsbti.api.Lazy[S] = {
      val z = safeLazy.invoke(null, new xsbti.F0[S] {def apply() = s}).asInstanceOf[xsbti.api.Lazy[S]]
      pending += z
      z
    }

    def thisPath(sym: Symbol) = path(pathComponents(sym, Constants.thisPath :: Nil))
    def path(components: List[PathComponent]) = new xsbti.api.Path(components.toArray[PathComponent])
    def pathComponents(sym: Symbol, postfix: List[PathComponent]): List[PathComponent] =
      if (sym == NoSymbol || sym.isEffectiveRoot) postfix
      else pathComponents(sym.owner, new xsbti.api.Id(simpleName(sym)) :: postfix)
    def simpleType(in: Symbol, t: Type): SimpleType =
      processType(in, t) match {
        case s: SimpleType => s
        case x => ctx.log("Not a simple type:\n\tType: " + t + " (" + t.getClass + ")\n\tTransformed: " + x.getClass); Constants.emptyType
      }
    def types(in: Symbol, t: List[Type]): Array[xsbti.api.Type] = t.map(processType(in, _)).toArray
    def projectionType(in: Symbol, pre: Type, sym: Symbol) =
      new xsbti.api.Projection(simpleType(in, pre), simpleName(sym))

    def annotations(in: Symbol, s: Symbol): Array[xsbti.api.Annotation] = Array()

    def viewer(s: Symbol) = (if (s.is(Module)) s.moduleClass else s).thisType
    def printMember(label: String, in: Symbol, t: Type) = println(label + " in " + in + " : " + t + " (debug: " + t.show + " )")

    def defDef(in: Symbol, s: Symbol): List[xsbti.api.Def] = {
      // DOTTY: TODO: dealias?
      val hasValueClassAsParameter: Boolean = {
        s.info.paramTypess.flatten.exists(t => isDerivedValueClass(t.typeSymbol))
      }

      val hasValueClassAsReturnType: Boolean = {
        val tpe = viewer(in).memberInfo(s)
        isDerivedValueClass(tpe.finalResultType.typeSymbol)
      }

      val inspectPostErasure = hasValueClassAsParameter || hasValueClassAsReturnType

      def build(t: Type, typeParams: Array[xsbti.api.TypeParameter], valueParameters: List[xsbti.api.ParameterList]): List[xsbti.api.Def] = {
        def parameterList(mt: MethodType, erase: Boolean = false): xsbti.api.ParameterList = {
          val MethodType(pnames, ptypes) = if (erase) TypeErasure.erasure(mt) else mt
          val params = pnames.zip(ptypes).map{case (pname, ptype) => makeParameter(pname.toString, ptype, ptype.typeSymbol)}
          new xsbti.api.ParameterList(params.toArray, mt.isImplicit)
        }
        t match {
          case t: PolyType =>
            assert(typeParams.isEmpty)
            assert(valueParameters.isEmpty)
            build(t.resultType, typeParameters(in, t), Nil)
          case mt @ MethodType(_, params) =>
            // The types of a method's parameters change between phases: For instance, if a
            // parameter is a subtype of AnyVal, then it won't have the same type before and after
            // erasure. Therefore we record the type of parameters before AND after erasure to
            // make sure that we don't miss some API changes.
            //   class A(val x: Int) extends AnyVal
            //   def foo(a: A): Int = A.x <- has type (LA)I before erasure
            //                            <- has type (I)I after erasure
            // If we change A from value class to normal class, we need to recompile all clients
            // of def foo.
            val paramSyms = params.map(_.typeSymbol)
            val beforeErasure =
              build(mt.resultType, typeParams, parameterList(mt) :: valueParameters)
            val afterErasure =
              if (inspectPostErasure)
                build(mt.resultType, typeParams, parameterList(mt, erase = true) :: valueParameters)
              else
                Nil

            beforeErasure ++ afterErasure
          case ExprType(resultType) =>
            build(resultType, typeParams, valueParameters)
          case returnType =>
            def makeDef(retTpe: xsbti.api.Type): xsbti.api.Def =
              new xsbti.api.Def(
                valueParameters.reverse.toArray,
                retTpe,
                typeParams,
                simpleName(s),
                getAccess(s),
                getModifiers(s),
                annotations(in, s))

            // The return type of a method may change before and after erasure. Consider the
            // following method:
            //   class A(val x: Int) extends AnyVal
            //   def foo(x: Int): A = new A(x) <- has type (I)LA before erasure
            //                                 <- has type (I)I after erasure
            // If we change A from value class to normal class, we need to recompile all clients
            // of def foo.
            val beforeErasure = makeDef(processType(in, dropConst(returnType)))
            val afterErasure = Nil
            if (inspectPostErasure) {
                val erasedReturn = dropConst(TypeErasure.erasure(viewer(in).memberInfo(s)))/* map {
                  case MethodType(_, r) => r
                  case other => other
                }*/ // DOTTY: What is this map for?
                List(makeDef(processType(in, erasedReturn)))
              } else Nil

            beforeErasure :: afterErasure
        }
      }

      def makeParameter(name: String, tpe: Type, ts: Symbol): xsbti.api.MethodParameter = {
        import xsbti.api.ParameterModifier._
        val (t, special) =
          /*if (ts == defn.RepeatedParamClass) // || s == defn.JavaRepeatedParamClass)
            (tpe.typeArgs(0), Repeated)
          else if (ts == defn.ByNameParamClass)
            (tpe.typeArgs(0), ByName)
          else*/ // DOTTY: TODO
          (tpe, Plain)
        new xsbti.api.MethodParameter(name, processType(in, t), hasDefault(ts), special)
      }
      val t = viewer(in).memberInfo(s)
      build(t, Array(), Nil)
    }
    def hasDefault(s: Symbol) = s.is(DefaultParameter)
    def fieldDef[T](in: Symbol, s: Symbol, keepConst: Boolean, create: (xsbti.api.Type, String, xsbti.api.Access, xsbti.api.Modifiers, Array[xsbti.api.Annotation]) => T): T = {
      val t = dropNullary(viewer(in).memberInfo(s))
      val t2 = if (keepConst) t else dropConst(t)
      create(processType(in, t2), simpleName(s), getAccess(s), getModifiers(s), annotations(in, s))
    }
    def dropConst(t: Type): Type = t match {
      case ConstantType(constant) => constant.tpe
      case _ => t
    }
    def dropNullary(t: Type): Type = t match {
      case ExprType(un) => un
      case _ => t
    }

    def typeDef(in: Symbol, s: Symbol): xsbti.api.TypeMember = {
      val (typeParams, tpe) =
        viewer(in).memberInfo(s) match {
          case t: PolyType => (typeParameters(in, t), t.resultType)
          case t => (Array[xsbti.api.TypeParameter](), t)
        }
      val name = simpleName(s)
      val access = getAccess(s)
      val modifiers = getModifiers(s)
      val as = annotations(in, s)

      if (s.isAliasType)
        new xsbti.api.TypeAlias(processType(in, tpe), typeParams, name, access, modifiers, as)
      else if (s.isAbstractType) {
        val bounds = tpe.bounds
        new xsbti.api.TypeDeclaration(processType(in, bounds.lo), processType(in, bounds.hi), typeParams, name, access, modifiers, as)
      } else {
        assert(false, "Unknown type member" + s)
        ???
      }
    }

    def structure(info: Type, s: Symbol): xsbti.api.Structure = structureCache.getOrElseUpdate(s, mkStructure(info, s))
    def structureWithInherited(info: ClassInfo, s: Symbol): xsbti.api.Structure = structureCache.getOrElseUpdate(s, mkStructureWithInherited(info, s))

    def removeConstructors(ds: List[Symbol]): List[Symbol] = ds filter {!_.isConstructor}

    /**
      * Create structure as-is, without embedding ancestors
      *
      * (for refinement types, and ClassInfoTypes encountered outside of a definition???).
      */
    def mkStructure(info: Type, s: Symbol): xsbti.api.Structure = {
      // We're not interested in the full linearization, so we can just use `parents`,
      // which side steps issues with baseType when f-bounded existential types and refined types mix
      // (and we get cyclic types which cause a stack overflow in showAPI).
      //
      // The old algorithm's semantics for inherited dependencies include all types occurring as a parent anywhere in a type,
      // so that, in `class C { def foo: A  }; class A extends B`, C is considered to have an "inherited dependency" on `A` and `B`!!!
      val parentTypes = info.parents
      val decls = info.decls.toList
      val declsNoModuleCtor = if (s.is(ModuleClass)) removeConstructors(decls) else decls
      mkStructure0(s, parentTypes, declsNoModuleCtor, Nil)
    }

    /**
      * Track all ancestors and inherited members for a class's API.
      *
      * A class's hash does not include hashes for its parent classes -- only the symbolic names --
      * so we must ensure changes propagate somehow.
      *
      * TODO: can we include hashes for parent classes instead? This seems a bit messy.
      */
    def mkStructureWithInherited(info: ClassInfo, s: Symbol): xsbti.api.Structure = {
      val ancestorTypes = linearizedAncestorTypes(info)
      val decls = info.decls.toList
      val declsNoModuleCtor = if (s.is(ModuleClass)) removeConstructors(decls) else decls
      val declSet = decls.toSet
      val inherited = info.membersBasedOnFlags(requiredFlags = AllFlags, excludedFlags = Private).toList.map(_.symbol).filterNot(declSet) // private members are not inherited
      mkStructure0(s, ancestorTypes, declsNoModuleCtor, inherited)
    }

    // Note that the ordering of classes in `baseClasses` is important.
    def linearizedAncestorTypes(info: ClassInfo): List[Type] = {
      val ref = info.fullyAppliedRef
      info.baseClasses.tail.map(ref.baseTypeWithArgs)
    }

    // If true, this template is publicly visible and should be processed as a public inheritance dependency.
    // Local classes and local refinements will never be traversed by the api phase, so we don't need to check for that.
    def isPublicStructure(s: Symbol): Boolean =
      !(s.is(Private) && (s.privateWithin == NoSymbol || s.is(Local)))

    def mkStructure0(s: Symbol, bases: List[Type], declared: List[Symbol], inherited: List[Symbol]): xsbti.api.Structure = {
      if (isPublicStructure(s)) {
        // DOTTY: TODO
        //sbtGlobal.addInheritedDependencies(sourceFile, bases.map(_.dealias.typeSymbol))
      }
      new xsbti.api.Structure(lzy(types(s, bases)), lzy(processDefinitions(s, declared)), lzy(processDefinitions(s, inherited)))
    }
    def processDefinitions(in: Symbol, defs: List[Symbol]): Array[xsbti.api.Definition] =
      sort(defs.toArray).flatMap((d: Symbol) => definition(in, d))
    def sort(defs: Array[Symbol]): Array[Symbol] = {
      Arrays.sort(defs, sortClasses)
      defs
    }


   def definition(in: Symbol, sym: Symbol): List[xsbti.api.Definition] = {
      def mkVar = List(fieldDef(in, sym, false, new xsbti.api.Var(_, _, _, _, _)))
      def mkVal = List(fieldDef(in, sym, true, new xsbti.api.Val(_, _, _, _, _)))
      if (sym.isClass)
        if (ignoreClass(sym)) Nil else List(classLike(in, sym))
      else if (sym.isType)
        List(typeDef(in, sym))
      else if (sym.is(Mutable, butNot = Accessor))
        mkVar
      else if (sym.isStable)
        mkVal
      else if (!sym.is(Synthetic) && !sym.isSetter)
        if (sym.isGetter) {
          assert(false) // DOTTY: Test if we hit this code path
          mkVar
        } else defDef(in, sym)
      else
        Nil
    }
    def ignoreClass(sym: Symbol): Boolean =
      /*sym.isLocalClass ||*/ sym.isAnonymousClass

    def getModifiers(s: Symbol): xsbti.api.Modifiers = {
      val absOver = s.is(AbsOverride)
      val abs = s.is(Abstract) || s.is(Deferred) || absOver
      val over = s.is(Override) || absOver
      new xsbti.api.Modifiers(abs, over, s.is(Final), s.is(Sealed), s.is(Implicit), s.is(Lazy), s.is(Macro))
    }

    def getAccess(c: Symbol): xsbti.api.Access = {
      if (c.isPublic) Constants.public
      else if (c.is(PrivateLocal)) Constants.privateLocal
      else if (c.is(ProtectedLocal)) Constants.protectedLocal
      else {
        val within = c.privateWithin
        val qualifier = if (within == NoSymbol) Constants.unqualified else new xsbti.api.IdQualifier(within.fullName.toString)
        if (c.is(Protected)) new xsbti.api.Protected(qualifier)
        else new xsbti.api.Private(qualifier)
      }
    }

    /**
      * Replace all types that directly refer to the `forbidden` symbol by `NoType`.
      * (a specialized version of substThisAndSym)
      */
    class SuppressSymbolRef(forbidden: Symbol) extends TypeMap {
      def apply(tp: Type) =
        if (tp.typeSymbol == forbidden) NoType
        else mapOver(tp)
    }

    def processType(in: Symbol, t: Type): xsbti.api.Type = typeCache.getOrElseUpdate((in, t), makeType(in, t))
    def makeType(in: Symbol, t: Type): xsbti.api.Type = {

      val dealiased = t match {
        case t: TypeRef if t.symbol.isAliasType => t.dealias
        case _ => t
      }

      dealiased match {
        case NoPrefix => Constants.emptyType
        case t: ThisType => new xsbti.api.Singleton(thisPath(t.cls))
        case t: NamedType => projectionType(in, t.prefix, t.symbol)
        case ConstantType(constant) => new xsbti.api.Constant(processType(in, constant.tpe), constant.stringValue)

        /* explaining the special-casing of references to refinement classes (https://support.typesafe.com/tickets/1882)
                         *
                         * goal: a representation of type references to refinement classes that's stable across compilation runs
                         *       (and thus insensitive to typing from source or unpickling from bytecode)
                         *
                         * problem: the current representation, which corresponds to the owner chain of the refinement:
                         *   1. is affected by pickling, so typing from source or using unpickled symbols give different results (because the unpickler "localizes" owners -- this could be fixed in the compiler)
                         *   2. can't distinguish multiple refinements in the same owner (this is a limitation of SBT's internal representation and cannot be fixed in the compiler)
                         *
                         * potential solutions:
                         *   - simply drop the reference: won't work as collapsing all refinement types will cause recompilation to be skipped when a refinement is changed to another refinement
                         *   - represent the symbol in the api: can't think of a stable way of referring to an anonymous symbol whose owner changes when pickled
                         *   + expand the reference to the corresponding refinement type: doing that recursively may not terminate, but we can deal with that by approximating recursive references
                         *     (all we care about is being sound for recompilation: recompile iff a dependency changes, and this will happen as long as we have one unrolling of the reference to the refinement)
         */
        // DOTTY: TODO figure out if special case needed for references to refinement classes
        /*
        case TypeRef(pre, sym, Nil) if sym.isRefinementClass =>
          // Since we only care about detecting changes reliably, we unroll a reference to a refinement class once.
          // Recursive references are simply replaced by NoType -- changes to the type will be seen in the first unrolling.
          // The API need not be type correct, so this truncation is acceptable. Most of all, the API should be compact.
          val unrolling = pre.memberInfo(sym) // this is a refinement type

          // in case there are recursive references, suppress them -- does this ever happen?
          // we don't have a test case for this, so warn and hope we'll get a contribution for it :-)
          val withoutRecursiveRefs = new SuppressSymbolRef(sym).mapOver(unrolling)
          if (unrolling ne withoutRecursiveRefs)
            reporter.warning(sym.pos, "sbt-api: approximated refinement ref" + t + " (== " + unrolling + ") to " + withoutRecursiveRefs + "\nThis is currently untested, please report the code you were compiling.")

          structure(withoutRecursiveRefs, sym)
        case tr@TypeRef(pre, sym, args) =>
          val base = projectionType(in, pre, sym)
          if (args.isEmpty)
            if (isRawType(tr))
              processType(in, rawToExistential(tr))
            else
              base
          else
            new xsbti.api.Parameterized(base, types(in, args))
        */
        case SuperType(thistpe: Type, supertpe: Type) =>
          ctx.warning("sbt-api: Super type (not implemented): this=" + thistpe + ", super=" + supertpe); Constants.emptyType
        case AnnotatedType(tpe, annot) =>
          ctx.warning("sbt-api: skipped annotations in " + dealiased)
          processType(in, tpe)
          // new xsbti.api.Annotated(processType(in, tpe), mkAnnotations(in, annot))
        // DOTTY: Use xsbti.api.Parameterized for actual type parameters (see above)
        case rt: RefinedType => structure(rt, rt.typeSymbol)
        case rt: AndOrType => structure(rt, rt.typeSymbol)
        case NoType => Constants.emptyType // this can happen when there is an error that will be reported by a later phase
        case t: PolyType =>
          new xsbti.api.Polymorphic(processType(in, t.resultType), typeParameters(in, t))
        case t: PolyParam =>
          new xsbti.api.ParameterRef(t.paramName.toString)
        case ExprType(resultType) =>
          ctx.warning("sbt-api: Unexpected nullary method type " + in + " in " + in.owner); Constants.emptyType
        case _ => {
          ctx.warning("sbt-api: Unhandled type " + t.getClass + " : " + t); Constants.emptyType
        }
      }
    }

    def typeParametersCls(in: Symbol, cls: ClassSymbol): Array[xsbti.api.TypeParameter] =
      cls.typeParams.map(typeParameterCls(in, _)).toArray
    def typeParameterCls(in: Symbol, tparam: TypeSymbol): xsbti.api.TypeParameter = {
      val varianceInt = tparam.variance
      import xsbti.api.Variance._
      val variance = if (varianceInt < 0) Contravariant else if (varianceInt > 0) Covariant else Invariant
      val name = tparam.fullName.toString
      val bounds = tparam.info.bounds
      new xsbti.api.TypeParameter(name, Array(), Array(), variance,
        processType(in, bounds.lo), processType(in, bounds.hi))
    }
      
    def typeParameters(in: Symbol, p: PolyType): Array[xsbti.api.TypeParameter] =
      p.paramNames.zip(p.paramBounds).map{case (pname, pbound) => typeParameter(in, pname, pbound)}.toArray
    def typeParameter(in: Symbol, name: Name, bounds: TypeBounds): xsbti.api.TypeParameter = {
      new xsbti.api.TypeParameter(name.toString, Array(), Array(), xsbti.api.Variance.Invariant,
        processType(in, bounds.lo), processType(in, bounds.hi))
    }

    /* Representation for the self type of a class symbol `s`, or `emptyType` for an *unascribed* self variable (or no self variable at all).
       Only the self variable's explicitly ascribed type is relevant for incremental compilation. */
    def selfType(in: Symbol, s: Symbol): xsbti.api.Type =
      processType(in, s.asClass.classInfo.givenSelfType)

    def classLike(in: Symbol, c: Symbol): ClassLike = classLikeCache.getOrElseUpdate((in, c), mkClassLike(in, c))
    def mkClassLike(in: Symbol, sym: Symbol): ClassLike = {
      val defType =
        if (sym.is(Trait)) DefinitionType.Trait
        else if (sym.is(ModuleClass)) {
          if (sym.is(PackageClass)) DefinitionType.PackageModule
          else DefinitionType.Module
        } else DefinitionType.ClassDef

      new xsbti.api.ClassLike(
        defType, lzy(selfType(in, sym)), lzy(structureWithInherited(viewer(in).memberInfo(sym).asInstanceOf[ClassInfo], sym)), emptyStringArray, typeParametersCls(in, sym.asClass), // look at class symbol
        sym.fullName.toString, getAccess(sym), getModifiers(sym), annotations(in, sym))
    }

    def simpleName(s: Symbol): String = s.originalName.decode.toString
    }
  }
   


  private trait ExtractUsedNames {
    val usedNames = collection.mutable.HashSet.empty[String]

    val traverser = new TreeTraverser {
      def traverse(tree: Tree)(implicit ctx: Context) = {
        extractNames(tree)
        traverseChildren(tree)
      }
    }
    /**
      * Extracts simple names used in given compilation unit.
      *
      * Extracts simple (unqualified) names mentioned in given in non-definition position by collecting
      * all symbols associated with non-definition trees and extracting names from all collected symbols.
      *
      * If given symbol is mentioned both in definition and in non-definition position (e.g. in member
      * selection) then that symbol is collected. It means that names of symbols defined and used in the
      * same compilation unit are extracted. We've considered not extracting names of those symbols
      * as an optimization strategy. It turned out that this is not correct.  Check
      * https://github.com/gkossakowski/sbt/issues/3 for an example of scenario where it matters.
      *
      * All extracted names are returned in _decoded_ form. This way we stay consistent with the rest
      * of incremental compiler which works with names in decoded form.
      *
      * Names mentioned in Import nodes are handled properly but require some special logic for two
      * reasons:
      *
      * 1. import node itself has a term symbol associated with it with a name `<import`>.
      * I (gkossakowski) tried to track down what role this symbol serves but I couldn't.
      * It doesn't look like there are many places in Scala compiler that refer to
      * that kind of symbols explicitly.
      * 2. ImportSelector is not subtype of Tree therefore is not processed by `Tree.foreach`
      *
      * Another type of tree nodes that requires special handling is TypeTree. TypeTree nodes
      * has a little bit odd representation:
      *
      * 1. TypeTree.hasSymbol always returns false even when TypeTree.symbol
      * returns a symbol
      * 2. The original tree from which given TypeTree was derived is stored
      * in TypeTree.original but Tree.forech doesn't walk into original
      * tree so we missed it
      *
      * TODO: retronym says: this should also walk into the original trees of symbol annotations
      *       (via `sym.annotations.map(_.original)`.)
      *
      * The tree walking algorithm walks into TypeTree.original explicitly.
      *
      */
    def extractNames(tree: Tree)(implicit ctx: Context): Unit = {
      tree match {
        // // turns out that Import node has a TermSymbol associated with it
        // // I (Grzegorz) tried to understand why it's there and what does it represent but
        // // that logic was introduced in 2005 without any justification I'll just ignore the
        // // import node altogether and just process the selectors in the import node
        // case Import(_, selectors: List[ImportSelector]) =>
        //   def usedNameInImportSelector(name: Name): Unit =
        //     if ((name != null) && (name != nme.WILDCARD)) addUsedName(name)
        //   selectors foreach { selector =>
        //     usedNameInImportSelector(selector.name)
        //     usedNameInImportSelector(selector.rename)
        //   }
        // TODO: figure out whether we should process the original tree or walk the type
        // the argument for processing the original tree: we process what user wrote
        // the argument for processing the type: we catch all transformations that typer applies
        // to types but that might be a bad thing because it might expand aliases eagerly which
        // not what we need
        case t: TypeTree if !t.original.isEmpty =>
          traverser.traverse(t.original)
          if (eligibleAsUsedName(t.symbol)) addUsedName(t.symbol.name) // DOTTY: do both, why not?
        case _: DefTree | _: Template => // we only collect used names, not defined ones // DOTTY: But what about type A = Foo { type X = ... } <== X might be modified in Foo
        case t: NameTree =>
          addUsedName(t.name)
        // DOTTY: TODO: Test cases for these two.
        case t: DenotingTree =>
          if (eligibleAsUsedName(t.symbol)) addUsedName(t.symbol.name)
        case t: ProxyTree =>
          if (eligibleAsUsedName(t.symbol)) addUsedName(t.symbol.name)
        case _ =>
      }
    }

    private def addUsedName(name: Name) =
      if (name ne Names.EMPTY_PACKAGE)
        usedNames += name.decode.toString.trim

    private def eligibleAsUsedName(symbol: Symbol)(implicit ctx: Context): Boolean =
      (symbol != NoSymbol) && !symbol.is(Synthetic)
  }

  private class ExtractDependenciesTraverser(val sourceFile: File) extends TreeTraverser with ExtractAPI with ExtractUsedNames {
    private val _dependencies = collection.mutable.HashSet.empty[Symbol]
    protected def addDependency(dep: Symbol): Unit = if (dep ne NoSymbol) _dependencies += dep
    def dependencies: Iterator[Symbol] = _dependencies.iterator
    def topLevelDependencies(implicit ctx: Context): Iterator[Symbol] = _dependencies.map(_.topLevelClass).iterator

    private val _inheritanceDependencies = collection.mutable.HashSet.empty[Symbol]
    protected def addInheritanceDependency(dep: Symbol): Unit = if (dep ne NoSymbol) _inheritanceDependencies += dep
    def inheritanceDependencies: Iterator[Symbol] = _inheritanceDependencies.iterator
    def topLevelInheritanceDependencies(implicit ctx: Context): Iterator[Symbol] = _inheritanceDependencies.map(_.topLevelClass).iterator

    /*
     * Some macros appear to contain themselves as original tree.
     * We must check that we don't inspect the same tree over and over.
     * See https://issues.scala-lang.org/browse/SI-8486
     *     https://github.com/sbt/sbt/issues/1237
     *     https://github.com/sbt/sbt/issues/1544
     */
    private[this] val inspectedOriginalTrees = collection.mutable.Set.empty[Tree]

    def apply(tree: Tree)(implicit ctx: Context): Unit = traverse(tree)

    override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
      extractAPI(tree)
      // println("defs: " + defs)
      // println("tree: " + tree)
      extractNames(tree)
      // println("usedNames: " + usedNames)

      tree match {
        case Import(expr, selectors) =>
          def lookupImported(name: Name) = expr.symbol.info.member(name).symbol
          def addImported(name: Name) = {
            // importing a name means importing both a term and a type (if they exist)
            addDependency(lookupImported(name.toTermName))
            addDependency(lookupImported(name.toTypeName))
          }
          traverse(expr)
          selectors.foreach {
            case Ident(name) =>
              addImported(name)
            case Pair(Ident(name), _) =>
              addImported(name)
            case _ =>
              // in case of wildcard import we do not rely on any particular name being defined
              // on `expr`; all symbols that are being used will get caught through selections
          }

        case id: Ident => addDependency(id.symbol)
        case sel @ Select(qual, _) => traverse(qual); addDependency(sel.symbol)
        case sel @ SelectFromTypeTree(qual, _) => traverse(qual); addDependency(sel.symbol)

        case t @ Template(_, parents, self, _) =>
          // use typeSymbol to dealias type aliases -- we want to track the dependency on the real class in the alias's RHS
          def flattenTypeToSymbols(tp: Type): List[Symbol] = if (tp eq null) Nil
          else tp match {
            // rt.typeSymbol is redundant if we list out all parents, TODO: what about rt.decls?
            case rt: RefinedType => rt.parents.flatMap(flattenTypeToSymbols)
            case _ => List(tp.typeSymbol)
          }

          val inheritanceTypes = parents.map(_.tpe).toSet
          val inheritanceSymbols = inheritanceTypes.flatMap(flattenTypeToSymbols)

          ctx.log("Parent types for " + tree.symbol + " (self: " + self.tpt.tpe + "): " + inheritanceTypes + " with symbols " + inheritanceSymbols.map(_.fullName))

          inheritanceSymbols.foreach(addInheritanceDependency)

          val allSymbols = (inheritanceTypes + self.tpt.tpe).flatMap(symbolsInType)
          (allSymbols ++ inheritanceSymbols).foreach(addDependency)
          t.body.foreach(traverse)

        // In some cases (eg. macro annotations), `typeTree.tpe` may be null. See sbt/sbt#1593 and sbt/sbt#1655.
        // TODO: how about the original type tree? compare with treatment in ExtractUsedNames
        case typeTree: TypeTree if typeTree.tpe != null => symbolsInType(typeTree.tpe) foreach addDependency
        case other =>
          traverseChildren(other)
      }

      // tree.attachments.all.collect {
      //   case att: analyzer.MacroExpansionAttachment if inspectedOriginalTrees.add(att.expandee) => att.expandee
      // }.foreach(traverse)
    }

    private def symbolsInType(tp: Type)(implicit ctx: Context): Set[Symbol] = {
      val acc: TypeAccumulator[Set[Symbol]] = new TypeAccumulator[Set[Symbol]] {
        def apply(syms: Set[Symbol], tp: Type) =
          if (!tp.typeSymbol.is(Package))
            foldOver(syms + tp.typeSymbol, tp)
          else
            foldOver(syms, tp)
      }
      acc(collection.immutable.Set.empty[Symbol], tp)
    }
  }
}