package dotty.tools
package dotc
package core
package classfile

import Contexts._, Symbols._, Types._, Names._, StdNames._, NameOps._, Scopes._, Decorators._
import SymDenotations._, unpickleScala2.Scala2Unpickler._, Constants._, Annotations._, util.Positions._
import NameKinds.{ModuleClassName, DefaultGetterName}
import ast.tpd._
import java.io.{ ByteArrayInputStream, DataInputStream, File, IOException }
import java.nio
import java.lang.Integer.toHexString
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.{ ListBuffer, ArrayBuffer }
import scala.annotation.switch
import typer.Checking.checkNonCyclic
import io.{AbstractFile, PlainFile, Path, ZipArchive, JarArchive}
import scala.util.control.NonFatal

object ClassfileParser {
  /** Marker trait for unpicklers that can be embedded in classfiles. */
  trait Embedded

  /** Indicate that there is nothing to unpickle and the corresponding symbols can
    * be invalidated. */
  object NoEmbedded extends Embedded

  /** Replace raw types with wildcard applications */
  def cook(implicit ctx: Context) = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: TypeRef if tp.symbol.typeParams.nonEmpty =>
        AppliedType(tp, tp.symbol.typeParams.map(Function.const(TypeBounds.empty)))
      case tp @ AppliedType(tycon, args) =>
        // disregard tycon itself, but map over it to visit the prefix
        tp.derivedAppliedType(mapOver(tycon), args.mapConserve(this))
      case tp @ TempPolyType(_, tpe) =>
        val tpe1 = this(tpe)
        if (tpe1 eq tpe) tp else tp.copy(tpe = tpe1)
      case tp @ TempClassInfoType(parents, _, _) =>
        val parents1 = parents.mapConserve(this)
        if (parents eq parents1) tp else tp.copy(parentTypes = parents1)
      case _ =>
        mapOver(tp)
    }
  }
}

class ClassfileParser(
    classfile: AbstractFile,
    classRoot: ClassDenotation,
    moduleRoot: ClassDenotation)(ictx: Context) {

  //println(s"parsing ${classRoot.name.debugString} ${moduleRoot.name.debugString}")

  import ClassfileConstants._
  import ClassfileParser._

  protected val in = new AbstractFileReader(classfile)

  protected val staticModule: Symbol = moduleRoot.sourceModule(ictx)

  protected val instanceScope: MutableScope = newScope     // the scope of all instance definitions
  protected val staticScope: MutableScope = newScope       // the scope of all static definitions
  protected var pool: ConstantPool = _              // the classfile's constant pool

  protected var currentClassName: SimpleName = _      // JVM name of the current class
  protected var classTParams = Map[Name,Symbol]()

  private[this] var Scala2UnpicklingMode = Mode.Scala2Unpickling

  classRoot.info = (new NoCompleter).withDecls(instanceScope)
  moduleRoot.info = (new NoCompleter).withDecls(staticScope).withSourceModule(_ => staticModule)

  private def currentIsTopLevel(implicit ctx: Context) = classRoot.owner is Flags.PackageClass

  private def mismatchError(className: SimpleName) =
    throw new IOException(s"class file '${in.file.canonicalPath}' has location not matching its contents: contains class $className")

  def run()(implicit ctx: Context): Option[Embedded] = try {
    ctx.debuglog("[class] >> " + classRoot.fullName)
    parseHeader()
    this.pool = new ConstantPool
    parseClass()
  } catch {
    case e: RuntimeException =>
      if (ctx.debug) e.printStackTrace()
      throw new IOException(
        i"""class file ${classfile.canonicalPath} is broken, reading aborted with ${e.getClass}
           |${Option(e.getMessage).getOrElse("")}""")
  }

  private def parseHeader(): Unit = {
    val magic = in.nextInt
    if (magic != JAVA_MAGIC)
      throw new IOException(s"class file '${in.file}' has wrong magic number 0x${toHexString(magic)}, should be 0x${toHexString(JAVA_MAGIC)}")
    val minorVersion = in.nextChar.toInt
    val majorVersion = in.nextChar.toInt
    if (majorVersion >= JAVA8_MAJOR_VERSION)
      Scala2UnpicklingMode |= Mode.Java8Unpickling
    if ((majorVersion < JAVA_MAJOR_VERSION) ||
        ((majorVersion == JAVA_MAJOR_VERSION) &&
         (minorVersion < JAVA_MINOR_VERSION)))
      throw new IOException(
        s"class file '${in.file}' has unknown version $majorVersion.$minorVersion, should be at least $JAVA_MAJOR_VERSION.$JAVA_MINOR_VERSION")
  }

  /** Return the class symbol of the given name. */
  def classNameToSymbol(name: Name)(implicit ctx: Context): Symbol = innerClasses.get(name) match {
    case Some(entry) => innerClasses.classSymbol(entry.externalName)
    case None => ctx.requiredClass(name)
  }

  var sawPrivateConstructor = false

  def parseClass()(implicit ctx: Context): Option[Embedded] = {
    val jflags       = in.nextChar
    val isAnnotation = hasAnnotation(jflags)
    val sflags       = classTranslation.flags(jflags)
    val isEnum       = (jflags & JAVA_ACC_ENUM) != 0
    val nameIdx      = in.nextChar
    currentClassName = pool.getClassName(nameIdx)

    if (currentIsTopLevel &&
        currentClassName != classRoot.fullName.toSimpleName &&
        currentClassName != classRoot.fullName.encode.toSimpleName)
      mismatchError(currentClassName)

    addEnclosingTParams()

    /** Parse parents for Java classes. For Scala, return AnyRef, since the real type will be unpickled.
     *  Updates the read pointer of 'in'. */
    def parseParents: List[Type] = {
      val superType = if (isAnnotation) { in.nextChar; defn.AnnotationType }
                      else pool.getSuperClass(in.nextChar).typeRef
      val ifaceCount = in.nextChar
      var ifaces = for (i <- (0 until ifaceCount).toList) yield pool.getSuperClass(in.nextChar).typeRef
        // Dotty deviation: was
        //    var ifaces = for (i <- List.range(0 until ifaceCount)) ...
        // This does not typecheck because the type parameter of List is now lower-bounded by Int | Char.
        // Consequently, no best implicit for the "Integral" evidence parameter of "range"
        // is found. If we treat constant subtyping specially, we might be able
        // to do something there. But in any case, the until should be more efficient.

      if (isAnnotation) ifaces = defn.ClassfileAnnotationType :: ifaces
      superType :: ifaces
    }

    val result = unpickleOrParseInnerClasses()
    if (!result.isDefined) {
      var classInfo: Type = TempClassInfoType(parseParents, instanceScope, classRoot.symbol)
      // might be reassigned by later parseAttributes
      val staticInfo = TempClassInfoType(List(), staticScope, moduleRoot.symbol)

      enterOwnInnerClasses()

      classRoot.setFlag(sflags)
      moduleRoot.setFlag(Flags.JavaDefined | Flags.ModuleClassCreationFlags)
      setPrivateWithin(classRoot, jflags)
      setPrivateWithin(moduleRoot, jflags)
      setPrivateWithin(moduleRoot.sourceModule, jflags)

      for (i <- 0 until in.nextChar) parseMember(method = false)
      for (i <- 0 until in.nextChar) parseMember(method = true)
      classInfo = cook.apply(parseAttributes(classRoot.symbol, classInfo))
      if (isAnnotation) addAnnotationConstructor(classInfo)

      val companionClassMethod = ctx.synthesizeCompanionMethod(nme.COMPANION_CLASS_METHOD, classRoot, moduleRoot)
      if (companionClassMethod.exists) companionClassMethod.entered
      val companionModuleMethod = ctx.synthesizeCompanionMethod(nme.COMPANION_MODULE_METHOD, moduleRoot, classRoot)
      if (companionModuleMethod.exists) companionModuleMethod.entered

      setClassInfo(classRoot, classInfo)
      setClassInfo(moduleRoot, staticInfo)
    } else if (result == Some(NoEmbedded)) {
      for (sym <- List(moduleRoot.sourceModule.symbol, moduleRoot.symbol, classRoot.symbol)) {
        classRoot.owner.asClass.delete(sym)
        if (classRoot.owner == defn.ScalaShadowingPackageClass) {
          // Symbols in scalaShadowing are also added to scala
          defn.ScalaPackageClass.delete(sym)
        }
        sym.markAbsent()
      }
    }

    // eager load java enum definitions for exhaustivity check of pattern match
    if (isEnum) {
      instanceScope.toList.map(_.ensureCompleted())
      staticScope.toList.map(_.ensureCompleted())
      classRoot.setFlag(Flags.Enum)
      moduleRoot.setFlag(Flags.Enum)
    }

    result
  }

  /** Add type parameters of enclosing classes */
  def addEnclosingTParams()(implicit ctx: Context): Unit = {
    var sym = classRoot.owner
    while (sym.isClass && !(sym is Flags.ModuleClass)) {
      for (tparam <- sym.typeParams) {
        classTParams = classTParams.updated(tparam.name.unexpandedName, tparam)
      }
      sym = sym.owner
    }
  }

  def parseMember(method: Boolean)(implicit ctx: Context): Unit = {
    val start = indexCoord(in.bp)
    val jflags = in.nextChar
    val sflags =
      if (method) Flags.Method | methodTranslation.flags(jflags)
      else fieldTranslation.flags(jflags)
    val name = pool.getName(in.nextChar)
    if (!(sflags is Flags.Private) || name == nme.CONSTRUCTOR) {
      val member = ctx.newSymbol(
        getOwner(jflags), name, sflags, memberCompleter, coord = start)
      getScope(jflags).enter(member)
    }
    // skip rest of member for now
    in.nextChar // info
    skipAttributes()
  }

  val memberCompleter = new LazyType {

    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
      val oldbp = in.bp
      try {
        in.bp = denot.symbol.coord.toIndex
        val sym = denot.symbol
        val jflags = in.nextChar
        val isEnum = (jflags & JAVA_ACC_ENUM) != 0
        val name = pool.getName(in.nextChar)
        val isConstructor = name eq nme.CONSTRUCTOR

        /** Strip leading outer param from constructor and trailing access tag for
         *  private inner constructors.
         */
        def normalizeConstructorParams() = innerClasses.get(currentClassName) match {
          case Some(entry) if !isStatic(entry.jflags) =>
            val mt @ MethodTpe(paramNames, paramTypes, resultType) = denot.info
            var normalizedParamNames = paramNames.tail
            var normalizedParamTypes = paramTypes.tail
            if ((jflags & JAVA_ACC_SYNTHETIC) != 0) {
              // SI-7455 strip trailing dummy argument ("access constructor tag") from synthetic constructors which
              // are added when an inner class needs to access a private constructor.
              normalizedParamNames = paramNames.dropRight(1)
              normalizedParamTypes = paramTypes.dropRight(1)
            }
            denot.info = mt.derivedLambdaType(normalizedParamNames, normalizedParamTypes, resultType)
          case _ =>
        }

        /** Make return type of constructor be the enclosing class type,
         *  and make constructor type polymorphic in the type parameters of the class
         */
        def normalizeConstructorInfo() = {
          val mt @ MethodType(paramNames) = denot.info
          val rt = classRoot.typeRef appliedTo (classRoot.typeParams map (_.typeRef))
          denot.info = mt.derivedLambdaType(paramNames, mt.paramInfos, rt)
          addConstructorTypeParams(denot)
        }

        denot.info = cook.apply(pool.getType(in.nextChar))
        if (isEnum) denot.info = ConstantType(Constant(sym))
        if (isConstructor) normalizeConstructorParams()
        setPrivateWithin(denot, jflags)
        denot.info = translateTempPoly(parseAttributes(sym, denot.info))
        if (isConstructor) normalizeConstructorInfo()

        if ((denot is Flags.Method) && (jflags & JAVA_ACC_VARARGS) != 0)
          denot.info = arrayToRepeated(denot.info)

        // seal java enums
        if (isEnum) {
          val enumClass = sym.owner.linkedClass
          if (!enumClass.exists)
            ctx.warning(s"no linked class for java enum $sym in ${sym.owner}. A referencing class file might be missing an InnerClasses entry.")
          else {
            if (!(enumClass is Flags.Sealed)) enumClass.setFlag(Flags.AbstractSealed)
            enumClass.addAnnotation(Annotation.Child(sym))
          }
        }
      } finally {
        in.bp = oldbp
      }
    }
  }

  /** Map direct references to Object to references to Any */
  final def objToAny(tp: Type)(implicit ctx: Context) =
    if (tp.isDirectRef(defn.ObjectClass) && !ctx.phase.erasedTypes) defn.AnyType else tp

  private def sigToType(sig: SimpleName, owner: Symbol = null)(implicit ctx: Context): Type = {
    var index = 0
    val end = sig.length
    def accept(ch: Char): Unit = {
      assert(sig(index) == ch, (sig(index), ch))
      index += 1
    }
    def subName(isDelimiter: Char => Boolean): SimpleName = {
      val start = index
      while (!isDelimiter(sig(index))) { index += 1 }
      sig.slice(start, index)
    }
    // Warning: sigToType contains nested completers which might be forced in a later run!
    // So local methods need their own ctx parameters.
    def sig2type(tparams: immutable.Map[Name, Symbol], skiptvs: Boolean)(implicit ctx: Context): Type = {
      val tag = sig(index); index += 1
      (tag: @switch) match {
        case BYTE_TAG   => defn.ByteType
        case CHAR_TAG   => defn.CharType
        case DOUBLE_TAG => defn.DoubleType
        case FLOAT_TAG  => defn.FloatType
        case INT_TAG    => defn.IntType
        case LONG_TAG   => defn.LongType
        case SHORT_TAG  => defn.ShortType
        case VOID_TAG   => defn.UnitType
        case BOOL_TAG   => defn.BooleanType
        case 'L' =>
          def processInner(tp: Type): Type = tp match {
            case tp: TypeRef if !(tp.symbol.owner is Flags.ModuleClass) =>
              if (config.Config.newScheme)
                TypeRef.withSym(processInner(tp.prefix.widen), tp.symbol.asType)
              else
                TypeRef.withSymOLD(processInner(tp.prefix.widen), tp.symbol.asType, tp.name)
            case _ =>
              tp
          }
          def processClassType(tp: Type): Type = tp match {
            case tp: TypeRef =>
              if (sig(index) == '<') {
                accept('<')
                val argsBuf = if (skiptvs) null else new ListBuffer[Type]
                while (sig(index) != '>') {
                  val arg = sig(index) match {
                    case variance @ ('+' | '-' | '*') =>
                      index += 1
                      variance match {
                        case '+' => objToAny(TypeBounds.upper(sig2type(tparams, skiptvs)))
                        case '-' =>
                          val tp = sig2type(tparams, skiptvs)
                          // sig2type seems to return AnyClass regardless of the situation:
                          // we don't want Any as a LOWER bound.
                          if (tp.isDirectRef(defn.AnyClass)) TypeBounds.empty
                          else TypeBounds.lower(tp)
                        case '*' => TypeBounds.empty
                      }
                    case _ => sig2type(tparams, skiptvs)
                  }
                  if (argsBuf != null) argsBuf += arg
                }
                accept('>')
                if (skiptvs) tp else tp.appliedTo(argsBuf.toList)
              } else tp
            case tp =>
              assert(sig(index) != '<', tp)
              tp
          }

          val classSym = classNameToSymbol(subName(c => c == ';' || c == '<'))
          var tpe = processClassType(processInner(classSym.typeRef))
          while (sig(index) == '.') {
            accept('.')
            val name = subName(c => c == ';' || c == '<' || c == '.').toTypeName
            val clazz = tpe.member(name).symbol
            tpe = processClassType(processInner(clazz.typeRef))
          }
          accept(';')
          tpe
        case ARRAY_TAG =>
          while ('0' <= sig(index) && sig(index) <= '9') index += 1
          var elemtp = sig2type(tparams, skiptvs)
          // make unbounded Array[T] where T is a type variable into Ar ray[T with Object]
          // (this is necessary because such arrays have a representation which is incompatible
          // with arrays of primitive types.
          // NOTE that the comparison to Object only works for abstract types bounded by classes that are strict subclasses of Object
          // if the bound is exactly Object, it will have been converted to Any, and the comparison will fail
          // see also RestrictJavaArraysMap (when compiling java sources directly)
          if (elemtp.typeSymbol.isAbstractType && !(elemtp.derivesFrom(defn.ObjectClass))) {
            elemtp = AndType(elemtp, defn.ObjectType)
          }
          defn.ArrayOf(elemtp)
        case '(' =>
          // we need a method symbol. given in line 486 by calling getType(methodSym, ..)
          val paramtypes = new ListBuffer[Type]()
          var paramnames = new ListBuffer[TermName]()
          while (sig(index) != ')') {
            paramnames += nme.syntheticParamName(paramtypes.length)
            paramtypes += objToAny(sig2type(tparams, skiptvs))
          }
          index += 1
          val restype = sig2type(tparams, skiptvs)
          JavaMethodType(paramnames.toList, paramtypes.toList, restype)
        case 'T' =>
          val n = subName(';'.==).toTypeName
          index += 1
          //assert(tparams contains n, s"classTparams = $classTParams, tparams = $tparams, key = $n")
          if (skiptvs) defn.AnyType else tparams(n).typeRef
      }
    } // sig2type(tparams, skiptvs)

    def sig2typeBounds(tparams: immutable.Map[Name, Symbol], skiptvs: Boolean)(implicit ctx: Context): Type = {
      val ts = new ListBuffer[Type]
      while (sig(index) == ':') {
        index += 1
        if (sig(index) != ':') // guard against empty class bound
          ts += objToAny(sig2type(tparams, skiptvs))
      }
      TypeBounds.upper(((NoType: Type) /: ts)(_ & _) orElse defn.AnyType)
    }

    var tparams = classTParams

    def typeParamCompleter(start: Int) = new LazyType {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        val savedIndex = index
        try {
          index = start
          denot.info =
            checkNonCyclic( // we need the checkNonCyclic call to insert LazyRefs for F-bounded cycles
                denot.symbol,
                sig2typeBounds(tparams, skiptvs = false),
                reportErrors = false)
        } finally {
          index = savedIndex
        }
      }
    }

    val newTParams = new ListBuffer[Symbol]()
    if (sig(index) == '<') {
      assert(owner != null)
      index += 1
      val start = index
      while (sig(index) != '>') {
        val tpname = subName(':'.==).toTypeName
        val s = ctx.newSymbol(
          owner, tpname, owner.typeParamCreationFlags,
          typeParamCompleter(index), coord = indexCoord(index))
        if (owner.isClass) owner.asClass.enter(s)
        tparams = tparams + (tpname -> s)
        sig2typeBounds(tparams, skiptvs = true)
        newTParams += s
      }
      index += 1
    }
    val ownTypeParams = newTParams.toList.asInstanceOf[List[TypeSymbol]]
    val tpe =
      if ((owner == null) || !owner.isClass)
        sig2type(tparams, skiptvs = false)
      else {
        classTParams = tparams
        val parents = new ListBuffer[Type]()
        while (index < end) {
          parents += sig2type(tparams, skiptvs = false)  // here the variance doesnt'matter
        }
        TempClassInfoType(parents.toList, instanceScope, owner)
      }
    if (ownTypeParams.isEmpty) tpe else TempPolyType(ownTypeParams, tpe)
  } // sigToType

  def parseAnnotArg(skip: Boolean = false)(implicit ctx: Context): Option[Tree] = {
    val tag = in.nextByte.toChar
    val index = in.nextChar
    tag match {
      case STRING_TAG =>
        if (skip) None else Some(Literal(Constant(pool.getName(index).toString)))
      case BOOL_TAG | BYTE_TAG | CHAR_TAG | SHORT_TAG =>
        if (skip) None else Some(Literal(pool.getConstant(index, tag)))
      case INT_TAG | LONG_TAG | FLOAT_TAG | DOUBLE_TAG =>
        if (skip) None else Some(Literal(pool.getConstant(index)))
      case CLASS_TAG =>
        if (skip) None else Some(Literal(Constant(pool.getType(index))))
      case ENUM_TAG =>
        val t = pool.getType(index)
        val n = pool.getName(in.nextChar)
        val module = t.typeSymbol.companionModule
        val s = module.info.decls.lookup(n)
        if (skip) {
          None
        } else if (s != NoSymbol) {
          Some(Literal(Constant(s)))
        } else {
          ctx.warning(s"""While parsing annotations in ${in.file}, could not find $n in enum $module.\nThis is likely due to an implementation restriction: an annotation argument cannot refer to a member of the annotated class (SI-7014).""")
          None
        }
      case ARRAY_TAG =>
        val arr = new ArrayBuffer[Tree]()
        var hasError = false
        for (i <- 0 until index)
          parseAnnotArg(skip) match {
            case Some(c) => arr += c
            case None => hasError = true
          }
        if (hasError) None
        else if (skip) None
        else {
          val elems = arr.toList
          val elemType =
            if (elems.isEmpty) defn.ObjectType
            else ctx.typeComparer.lub(elems.tpes).widen
          Some(JavaSeqLiteral(elems, TypeTree(elemType)))
        }
      case ANNOTATION_TAG =>
        parseAnnotation(index, skip) map (_.tree)
    }
  }

  /** Parse and return a single annotation.  If it is malformed,
   *  return None.
   */
  def parseAnnotation(attrNameIndex: Char, skip: Boolean = false)(implicit ctx: Context): Option[Annotation] = try {
    val attrType = pool.getType(attrNameIndex)
    val nargs = in.nextChar
    val argbuf = new ListBuffer[Tree]
    var hasError = false
    for (i <- 0 until nargs) {
      val name = pool.getName(in.nextChar)
      parseAnnotArg(skip) match {
        case Some(arg) => argbuf += NamedArg(name, arg)
        case None => hasError = !skip
      }
    }
    if (hasError || skip) None
    else Some(Annotation.deferredResolve(attrType, argbuf.toList))
  } catch {
    case f: FatalError => throw f // don't eat fatal errors, they mean a class was not found
    case NonFatal(ex) =>
      // We want to be robust when annotations are unavailable, so the very least
      // we can do is warn the user about the exception
      // There was a reference to ticket 1135, but that is outdated: a reference to a class not on
      // the classpath would *not* end up here. A class not found is signaled
      // with a `FatalError` exception, handled above. Here you'd end up after a NPE (for example),
      // and that should never be swallowed silently.
      ctx.warning("Caught: " + ex + " while parsing annotations in " + in.file)
      if (ctx.debug) ex.printStackTrace()

      None // ignore malformed annotations
  }

  def parseAttributes(sym: Symbol, symtype: Type)(implicit ctx: Context): Type = {
    def convertTo(c: Constant, pt: Type): Constant = {
      if (pt == defn.BooleanType && c.tag == IntTag)
        Constant(c.value != 0)
      else
        c convertTo pt
    }
    var newType = symtype

    def parseAttribute(): Unit = {
      val attrName = pool.getName(in.nextChar).toTypeName
      val attrLen = in.nextInt
      val end = in.bp + attrLen
      attrName match {
        case tpnme.SignatureATTR =>
          val sig = pool.getExternalName(in.nextChar)
          newType = sigToType(sig, sym)
          if (ctx.debug && ctx.verbose)
            println("" + sym + "; signature = " + sig + " type = " + newType)
        case tpnme.SyntheticATTR =>
          sym.setFlag(Flags.SyntheticArtifact)
        case tpnme.BridgeATTR =>
          sym.setFlag(Flags.Bridge)
        case tpnme.DeprecatedATTR =>
          val msg = Literal(Constant("see corresponding Javadoc for more information."))
          val since = Literal(Constant(""))
          sym.addAnnotation(Annotation(defn.DeprecatedAnnot, msg, since))
        case tpnme.ConstantValueATTR =>
          val c = pool.getConstant(in.nextChar)
          val c1 = convertTo(c, symtype)
          if (c1 ne null) newType = ConstantType(c1)
          else println("failure to convert " + c + " to " + symtype); //debug
        case tpnme.AnnotationDefaultATTR =>
          sym.addAnnotation(Annotation(defn.AnnotationDefaultAnnot, Nil))
        // Java annotations on classes / methods / fields with RetentionPolicy.RUNTIME
        case tpnme.RuntimeAnnotationATTR =>
          parseAnnotations(attrLen)

        // TODO 1: parse runtime visible annotations on parameters
        // case tpnme.RuntimeParamAnnotationATTR

        // TODO 2: also parse RuntimeInvisibleAnnotation / RuntimeInvisibleParamAnnotation,
        // i.e. java annotations with RetentionPolicy.CLASS?

        case tpnme.ExceptionsATTR =>
          parseExceptions(attrLen)

        case tpnme.CodeATTR =>
          if (sym.owner is Flags.JavaTrait) {
            sym.resetFlag(Flags.Deferred)
            sym.owner.resetFlag(Flags.PureInterface)
            ctx.log(s"$sym in ${sym.owner} is a java8+ default method.")
          }
          in.skip(attrLen)

        case _ =>
      }
      in.bp = end
    }

    /**
     * Parse the "Exceptions" attribute which denotes the exceptions
     * thrown by a method.
     */
    def parseExceptions(len: Int): Unit = {
      val nClasses = in.nextChar
      for (n <- 0 until nClasses) {
        // FIXME: this performs an equivalent of getExceptionTypes instead of getGenericExceptionTypes (SI-7065)
        val cls = pool.getClassSymbol(in.nextChar.toInt)
        sym.addAnnotation(ThrowsAnnotation(cls.asClass))
      }
    }

    /** Parse a sequence of annotations and attaches them to the
     *  current symbol sym, except for the ScalaSignature annotation that it returns, if it is available. */
    def parseAnnotations(len: Int): Unit =  {
      val nAttr = in.nextChar
      for (n <- 0 until nAttr)
        parseAnnotation(in.nextChar) match {
          case Some(annot) =>
            sym.addAnnotation(annot)
          case None =>
        }
    }

    // begin parseAttributes
    for (i <- 0 until in.nextChar) {
      parseAttribute()
    }
    newType
  }

  /** Add synthetic constructor(s) and potentially also default getters which
   *  reflects the fields of the annotation with given `classInfo`.
   *  Annotations in Scala are assumed to get all their arguments as constructor
   *  parameters. For Java annotations we need to fake it by making up the constructor.
   *  Note that default getters have type Nothing. That's OK because we need
   *  them only to signal that the corresponding parameter is optional.
   */
  def addAnnotationConstructor(classInfo: Type, tparams: List[TypeSymbol] = Nil)(implicit ctx: Context): Unit = {
    def addDefaultGetter(attr: Symbol, n: Int) =
      ctx.newSymbol(
        owner = moduleRoot.symbol,
        name = DefaultGetterName(nme.CONSTRUCTOR, n),
        flags = attr.flags & Flags.AccessFlags,
        info = defn.NothingType).entered

    classInfo match {
      case classInfo @ TempPolyType(tparams, restpe) if tparams.isEmpty =>
        addAnnotationConstructor(restpe, tparams)
      case classInfo: TempClassInfoType =>
        val attrs = classInfo.decls.toList.filter(_.isTerm)
        val targs = tparams.map(_.typeRef)
        val paramNames = attrs.map(_.name.asTermName)
        val paramTypes = attrs.map(_.info.resultType)

        def addConstr(ptypes: List[Type]) = {
          val mtype = MethodType(paramNames, ptypes, classRoot.typeRef.appliedTo(targs))
          val constrType = if (tparams.isEmpty) mtype else TempPolyType(tparams, mtype)
          val constr = ctx.newSymbol(
            owner = classRoot.symbol,
            name = nme.CONSTRUCTOR,
            flags = Flags.Synthetic | Flags.JavaDefined,
            info = constrType
          ).entered
          for ((attr, i) <- attrs.zipWithIndex)
            if (attr.hasAnnotation(defn.AnnotationDefaultAnnot)) {
              constr.setFlag(Flags.HasDefaultParams)
              addDefaultGetter(attr, i)
            }
        }

        addConstr(paramTypes)

        // The code below added an extra constructor to annotations where the
        // last parameter of the constructor is an Array[X] for some X, the
        // array was replaced by a vararg argument. Unfortunately this breaks
        // inference when doing:
        //   @Annot(Array())
        // The constructor is overloaded so the expected type of `Array()` is
        // WildcardType, and the type parameter of the Array apply method gets
        // instantiated to `Nothing` instead of `X`.
        // I'm leaving this commented out in case we improve inference to make this work.
        // Note that if this is reenabled then JavaParser will also need to be modified
        // to add the extra constructor (this was not implemented before).
        /*
        if (paramTypes.nonEmpty)
          paramTypes.last match {
            case defn.ArrayOf(elemtp) =>
              addConstr(paramTypes.init :+ defn.RepeatedParamType.appliedTo(elemtp))
            case _ =>
        }
        */
    }
  }

  /** Enter own inner classes in the right scope. It needs the scopes to be set up,
   *  and implicitly current class' superclasses.
   */
  private def enterOwnInnerClasses()(implicit ctx: Context): Unit = {
    def className(name: Name): Name = {
      val name1 = name.toSimpleName
      name1.drop(name1.lastIndexOf('.') + 1)
    }

    def enterClassAndModule(entry: InnerClassEntry, file: AbstractFile, jflags: Int) = {
      ctx.base.loaders.enterClassAndModule(
          getOwner(jflags),
          entry.originalName,
          new ClassfileLoader(file),
          classTranslation.flags(jflags),
          getScope(jflags))
    }

    for (entry <- innerClasses.values) {
      // create a new class member for immediate inner classes
      if (entry.outerName == currentClassName) {
        val file = ctx.platform.classPath.findClassFile(entry.externalName.mangledString) getOrElse {
          throw new AssertionError(entry.externalName)
        }
        enterClassAndModule(entry, file, entry.jflags)
      }
    }
  }

  // Nothing$ and Null$ were incorrectly emitted with a Scala attribute
  // instead of ScalaSignature before 2.13.0-M2, see https://github.com/scala/scala/pull/5952
  private[this] val scalaUnpickleWhitelist = List(tpnme.nothingClass, tpnme.nullClass)

  /** Parse inner classes. Expects `in.bp` to point to the superclass entry.
   *  Restores the old `bp`.
   *  @return true iff classfile is from Scala, so no Java info needs to be read.
   */
  def unpickleOrParseInnerClasses()(implicit ctx: Context): Option[Embedded] = {
    val oldbp = in.bp
    try {
      skipSuperclasses()
      skipMembers() // fields
      skipMembers() // methods
      val attrs = in.nextChar
      val attrbp = in.bp

      def scan(target: TypeName): Boolean = {
        in.bp = attrbp
        var i = 0
        while (i < attrs && pool.getName(in.nextChar).toTypeName != target) {
          val attrLen = in.nextInt
          in.skip(attrLen)
          i += 1
        }
        i < attrs
      }

      def unpickleScala(bytes: Array[Byte]): Some[Embedded] = {
        val unpickler = new unpickleScala2.Scala2Unpickler(bytes, classRoot, moduleRoot)(ctx)
        unpickler.run()(ctx.addMode(Scala2UnpicklingMode))
        Some(unpickler)
      }

      def unpickleTASTY(bytes: Array[Byte]): Some[Embedded]  = {
        val unpickler = new tasty.DottyUnpickler(bytes)
        unpickler.enter(roots = Set(classRoot, moduleRoot, moduleRoot.sourceModule))
        Some(unpickler)
      }

      def parseScalaSigBytes: Array[Byte] = {
        val tag = in.nextByte.toChar
        assert(tag == STRING_TAG, tag)
        pool getBytes in.nextChar
      }

      def parseScalaLongSigBytes: Array[Byte] = {
        val tag = in.nextByte.toChar
        assert(tag == ARRAY_TAG, tag)
        val stringCount = in.nextChar
        val entries =
          for (i <- 0 until stringCount) yield {
            val stag = in.nextByte.toChar
            assert(stag == STRING_TAG, stag)
            in.nextChar.toInt
          }
        pool.getBytes(entries.toList)
      }

      if (scan(tpnme.TASTYATTR)) {
        val attrLen = in.nextInt
        if (attrLen == 0) { // A tasty attribute implies the existence of the .tasty file
          def readTastyForClass(jpath: nio.file.Path): Array[Byte] = {
            val plainFile = new PlainFile(io.File(jpath).changeExtension("tasty"))
            if (plainFile.exists) plainFile.toByteArray
            else {
              ctx.error("Could not find " + plainFile)
              Array.empty
            }
          }
          val tastyBytes = classfile.underlyingSource match { // TODO: simplify when #3552 is fixed
            case None =>
              ctx.error("Could not load TASTY from .tasty for virtual file " + classfile)
              Array.empty[Byte]
            case Some(jar: ZipArchive) => // We are in a jar
              val jarFile = JarArchive.open(io.File(jar.jpath))
              try readTastyForClass(jarFile.jpath.resolve(classfile.path))
              finally jarFile.close()
            case _ =>
              readTastyForClass(classfile.jpath)
          }
          if (tastyBytes.nonEmpty)
            return unpickleTASTY(tastyBytes)
        }
        else return unpickleTASTY(in.nextBytes(attrLen))
      }

      if (scan(tpnme.ScalaATTR) && !scalaUnpickleWhitelist.contains(classRoot.name)) {
        // To understand the situation, it's helpful to know that:
        // - Scalac emits the `ScalaSig` attribute for classfiles with pickled information
        // and the `Scala` attribute for everything else.
        // - Dotty emits the `TASTY` attribute for classfiles with pickled information
        // and the `Scala` attribute for _every_ classfile.
        //
        // Therefore, if the `Scala` attribute is present but the `TASTY`
        // attribute isn't, this classfile is a compilation artifact.
        return Some(NoEmbedded)
      }

      if (scan(tpnme.RuntimeAnnotationATTR)) {
        val attrLen = in.nextInt
        val nAnnots = in.nextChar
        var i = 0
        while (i < nAnnots) {
          val attrClass = pool.getType(in.nextChar).typeSymbol
          val nArgs = in.nextChar
          var j = 0
          while (j < nArgs) {
            val argName = pool.getName(in.nextChar)
            if (argName == nme.bytes)
              if (attrClass == defn.ScalaSignatureAnnot)
                return unpickleScala(parseScalaSigBytes)
              else if (attrClass == defn.ScalaLongSignatureAnnot)
                return unpickleScala(parseScalaLongSigBytes)
              else if (attrClass == defn.TASTYSignatureAnnot)
                return unpickleTASTY(parseScalaSigBytes)
              else if (attrClass == defn.TASTYLongSignatureAnnot)
                return unpickleTASTY(parseScalaLongSigBytes)
            parseAnnotArg(skip = true)
            j += 1
          }
          i += 1
        }
      }

      if (scan(tpnme.InnerClassesATTR)) {
        val attrLen = in.nextInt
        val entries = in.nextChar.toInt
        for (i <- 0 until entries) {
          val innerIndex = in.nextChar
          val outerIndex = in.nextChar
          val nameIndex = in.nextChar
          val jflags = in.nextChar
          if (innerIndex != 0 && outerIndex != 0 && nameIndex != 0) {
            val entry = InnerClassEntry(innerIndex, outerIndex, nameIndex, jflags)
            innerClasses(pool.getClassName(innerIndex)) = entry
          }
        }
      }
      None
    } finally in.bp = oldbp
  }

  /** An entry in the InnerClasses attribute of this class file. */
  case class InnerClassEntry(external: Int, outer: Int, name: Int, jflags: Int) {
    def externalName = pool.getClassName(external)
    def outerName    = pool.getClassName(outer)
    def originalName = pool.getName(name)

    override def toString =
      originalName + " in " + outerName + "(" + externalName + ")"
  }

  object innerClasses extends scala.collection.mutable.HashMap[Name, InnerClassEntry] {
    /** Return the Symbol of the top level class enclosing `name`,
     *  or 'name's symbol if no entry found for `name`.
     */
    def topLevelClass(name: Name)(implicit ctx: Context): Symbol = {
      val tlName = if (isDefinedAt(name)) {
        var entry = this(name)
        while (isDefinedAt(entry.outerName))
          entry = this(entry.outerName)
        entry.outerName
      } else
        name
      classNameToSymbol(tlName)
    }

    /** Return the class symbol for `externalName`. It looks it up in its outer class.
     *  Forces all outer class symbols to be completed.
     *
     *  If the given name is not an inner class, it returns the symbol found in `defn`.
     */
    def classSymbol(externalName: Name)(implicit ctx: Context): Symbol = {
      /** Return the symbol of `innerName`, having the given `externalName`. */
      def innerSymbol(externalName: Name, innerName: Name, static: Boolean): Symbol = {
        def getMember(sym: Symbol, name: Name)(implicit ctx: Context): Symbol =
          if (static)
            if (sym == classRoot.symbol) staticScope.lookup(name)
            else {
              var module = sym.companionModule
              if (!module.exists && sym.isAbsent)
                module = sym.scalacLinkedClass
              module.info.member(name).symbol
            }
          else
            if (sym == classRoot.symbol) instanceScope.lookup(name)
            else sym.info.member(name).symbol

        innerClasses.get(externalName) match {
          case Some(entry) =>
            val outerName = entry.outerName.stripModuleClassSuffix
            val owner = classSymbol(outerName)
            val result = ctx.atPhaseNotLaterThan(ctx.typerPhase) { implicit ctx =>
              getMember(owner, innerName.toTypeName)
            }
            assert(result ne NoSymbol,
              i"""failure to resolve inner class:
                 |externalName = $externalName,
                 |outerName = $outerName,
                 |innerName = $innerName
                 |owner.fullName = ${owner.showFullName}
                 |while parsing ${classfile}""")
            result

          case None =>
            classNameToSymbol(externalName)
        }
      }

      get(externalName) match {
        case Some(entry) =>
          innerSymbol(entry.externalName, entry.originalName, isStatic(entry.jflags))
        case None =>
          classNameToSymbol(externalName)
      }
    }
  }

 def skipAttributes(): Unit = {
    val attrCount = in.nextChar
    for (i <- 0 until attrCount) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  def skipMembers(): Unit = {
    val memberCount = in.nextChar
    for (i <- 0 until memberCount) {
      in.skip(6); skipAttributes()
    }
  }

  def skipSuperclasses(): Unit = {
    in.skip(2) // superclass
    val ifaces = in.nextChar
    in.skip(2 * ifaces)
  }

  protected def getOwner(flags: Int): Symbol =
    if (isStatic(flags)) moduleRoot.symbol else classRoot.symbol

  protected def getScope(flags: Int): MutableScope =
    if (isStatic(flags)) staticScope else instanceScope

  private def setPrivateWithin(denot: SymDenotation, jflags: Int)(implicit ctx: Context): Unit = {
    if ((jflags & (JAVA_ACC_PRIVATE | JAVA_ACC_PUBLIC)) == 0)
      denot.privateWithin = denot.enclosingPackageClass
  }

  private def isPrivate(flags: Int)     = (flags & JAVA_ACC_PRIVATE) != 0
  private def isStatic(flags: Int)      = (flags & JAVA_ACC_STATIC) != 0
  private def hasAnnotation(flags: Int) = (flags & JAVA_ACC_ANNOTATION) != 0

  class ConstantPool {
    private val len = in.nextChar
    private val starts = new Array[Int](len)
    private val values = new Array[AnyRef](len)
    private val internalized = new Array[SimpleName](len)

    { var i = 1
      while (i < starts.length) {
        starts(i) = in.bp
        i += 1
        (in.nextByte.toInt: @switch) match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE =>
            in.skip(in.nextChar)
          case CONSTANT_CLASS | CONSTANT_STRING | CONSTANT_METHODTYPE =>
            in.skip(2)
          case CONSTANT_METHODHANDLE =>
            in.skip(3)
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF
             | CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT
             | CONSTANT_INVOKEDYNAMIC =>
            in.skip(4)
          case CONSTANT_LONG | CONSTANT_DOUBLE =>
            in.skip(8)
            i += 1
          case _ =>
            errorBadTag(in.bp - 1)
        }
      }
    }

    /** Return the name found at given index. */
    def getName(index: Int): SimpleName = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      values(index) match {
        case name: SimpleName => name
        case null   =>
          val start = starts(index)
          if (in.buf(start).toInt != CONSTANT_UTF8) errorBadTag(start)
          val len   = in.getChar(start + 1).toInt
          val name = termName(fromMUTF8(in.buf, start + 1, len + 2))
          values(index) = name
          name
      }
    }

    private def fromMUTF8(bytes: Array[Byte], offset: Int, len: Int): String =
      new DataInputStream(new ByteArrayInputStream(bytes, offset, len)).readUTF

    /** Return the name found at given index in the constant pool, with '/' replaced by '.'. */
    def getExternalName(index: Int): SimpleName = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      if (internalized(index) == null)
        internalized(index) = getName(index).replace('/', '.')

      internalized(index)
    }

    def getClassSymbol(index: Int)(implicit ctx: Context): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var c = values(index).asInstanceOf[Symbol]
      if (c eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        if (name.endsWith("$") && (name ne nme.nothingRuntimeClass) && (name ne nme.nullRuntimeClass))
          // Null$ and Nothing$ ARE classes
          c = ctx.requiredModule(name.dropRight(1))
        else c = classNameToSymbol(name)
        values(index) = c
      }
      c
    }

    /** Return the external name of the class info structure found at 'index'.
     *  Use 'getClassSymbol' if the class is sure to be a top-level class.
     */
    def getClassName(index: Int): SimpleName = {
      val start = starts(index)
      if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
      getExternalName(in.getChar(start + 1))
    }

    /** Return a name and a type at the given index.
     */
    private def getNameAndType(index: Int, ownerTpe: Type)(implicit ctx: Context): (Name, Type) = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var p = values(index).asInstanceOf[(Name, Type)]
      if (p eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_NAMEANDTYPE) errorBadTag(start)
        val name = getName(in.getChar(start + 1).toInt)
        var tpe  = getType(in.getChar(start + 3).toInt)
        // fix the return type, which is blindly set to the class currently parsed
        if (name == nme.CONSTRUCTOR)
          tpe match {
            case tp: MethodType =>
              tp.derivedLambdaType(tp.paramNames, tp.paramInfos, ownerTpe)
          }
        p = (name, tpe)
        values(index) = p
      }
      p
    }

    /** Return the type of a class constant entry. Since
     *  arrays are considered to be class types, they might
     *  appear as entries in 'newarray' or 'cast' opcodes.
     */
    def getClassOrArrayType(index: Int)(implicit ctx: Context): Type = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      val value = values(index)
      var c: Type = null
      if (value eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        if (name.firstPart(0) == ARRAY_TAG) {
          c = sigToType(name)
          values(index) = c
        } else {
          val sym = classNameToSymbol(name)
          values(index) = sym
          c = sym.typeRef
        }
      } else c = value match {
          case tp: Type => tp
          case cls: Symbol => cls.typeRef
      }
      c
    }

    def getType(index: Int)(implicit ctx: Context): Type =
      sigToType(getExternalName(index))

    def getSuperClass(index: Int)(implicit ctx: Context): Symbol = {
      assert(index != 0, "attempt to parse java.lang.Object from classfile")
      getClassSymbol(index)
    }

    def getConstant(index: Int, tag: Int = -1)(implicit ctx: Context): Constant = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var value = values(index)
      if (value eq null) {
        val start = starts(index)
        value = (in.buf(start).toInt: @switch) match {
          case CONSTANT_STRING =>
            Constant(getName(in.getChar(start + 1).toInt).toString)
          case CONSTANT_INTEGER if tag != -1 =>
            val value = in.getInt(start + 1)
            (tag: @switch) match {
              case BOOL_TAG =>
                Constant(value != 0)
              case BYTE_TAG =>
                Constant(value.toByte)
              case CHAR_TAG =>
                Constant(value.toChar)
              case SHORT_TAG =>
                Constant(value.toShort)
              case _ =>
                errorBadTag(tag)
            }
          case CONSTANT_INTEGER =>
            Constant(in.getInt(start + 1))
          case CONSTANT_FLOAT =>
            Constant(in.getFloat(start + 1))
          case CONSTANT_LONG =>
            Constant(in.getLong(start + 1))
          case CONSTANT_DOUBLE =>
            Constant(in.getDouble(start + 1))
          case CONSTANT_CLASS =>
            getClassOrArrayType(index).typeSymbol
          case _ =>
            errorBadTag(start)
        }
        values(index) = value
      }
      value match {
        case  ct: Constant => ct
        case cls: Symbol   => Constant(cls.typeRef)
        case arr: Type     => Constant(arr)
      }
    }

    private def getSubArray(bytes: Array[Byte]): Array[Byte] = {
      val decodedLength = ByteCodecs.decode(bytes)
      val arr           = new Array[Byte](decodedLength)
      System.arraycopy(bytes, 0, arr, 0, decodedLength)
      arr
    }

    def getBytes(index: Int): Array[Byte] = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var value = values(index).asInstanceOf[Array[Byte]]
      if (value eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_UTF8) errorBadTag(start)
        val len   = in.getChar(start + 1)
        val bytes = new Array[Byte](len)
        System.arraycopy(in.buf, start + 3, bytes, 0, len)
        value = getSubArray(bytes)
        values(index) = value
      }
      value
    }

    def getBytes(indices: List[Int]): Array[Byte] = {
      assert(!indices.isEmpty, indices)
      var value = values(indices.head).asInstanceOf[Array[Byte]]
      if (value eq null) {
        val bytesBuffer = ArrayBuffer.empty[Byte]
        for (index <- indices) {
          if (index <= 0 || ConstantPool.this.len <= index) errorBadIndex(index)
          val start = starts(index)
          if (in.buf(start).toInt != CONSTANT_UTF8) errorBadTag(start)
          val len = in.getChar(start + 1)
          bytesBuffer ++= in.buf.view(start + 3, start + 3 + len)
        }
        value = getSubArray(bytesBuffer.toArray)
        values(indices.head) = value
      }
      value
    }

    /** Throws an exception signaling a bad constant index. */
    private def errorBadIndex(index: Int) =
      throw new RuntimeException("bad constant pool index: " + index + " at pos: " + in.bp)

    /** Throws an exception signaling a bad tag at given address. */
    private def errorBadTag(start: Int) =
      throw new RuntimeException("bad constant pool tag " + in.buf(start) + " at byte " + start)
  }
}

