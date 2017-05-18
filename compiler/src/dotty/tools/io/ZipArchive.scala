/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package dotty.tools.io

import java.net.URL
import java.io.{ IOException, InputStream, ByteArrayInputStream, FilterInputStream }
import java.util.zip.{ ZipEntry, ZipFile, ZipInputStream }
import java.util.jar.Manifest
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.annotation.tailrec

/** An abstraction for zip files and streams.  Everything is written the way
 *  it is for performance: we come through here a lot on every run.  Be careful
 *  about changing it.
 *
 *  @author  Philippe Altherr (original version)
 *  @author  Paul Phillips (this one)
 *  @version 2.0,
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object ZipArchive {
  private[io] val closeZipFile = sys.props.get("scala.classpath.closeZip").map(_.toBoolean).getOrElse(false)

  /**
   * @param   file  a File
   * @return  A ZipArchive if `file` is a readable zip file, otherwise null.
   */
  def fromFile(file: File): FileZipArchive = fromFile(file.jfile)
  def fromFile(file: JFile): FileZipArchive =
    try   { new FileZipArchive(file) }
    catch { case _: IOException => null }

  def fromManifestURL(url: URL): AbstractFile = new ManifestResources(url)

  def dirName(path: String)  = splitPath(path, front = true)
  private def baseName(path: String) = splitPath(path, front = false)
  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path  = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx   = path.lastIndexOf('/')

    if (idx < 0)
      if (front) "/"
      else path
    else
      if (front) path.substring(0, idx + 1)
      else path.substring(idx + 1)
  }
}
import ZipArchive._
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
abstract class ZipArchive(override val file: JFile) extends AbstractFile with Equals {
  self =>

  override def underlyingSource = Some(this)
  def isDirectory = true
  def lookupName(name: String, directory: Boolean) = unsupported()
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported()
  def create()  = unsupported()
  def delete()  = unsupported()
  def output    = unsupported()
  def container = unsupported()
  def absolute  = unsupported()

  /** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
  sealed abstract class Entry(path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this name for compat with sbt's compiler-interface
    def getArchive: ZipFile = null
    override def underlyingSource = Some(self)
    override def toString = self.path + "(" + path + ")"
  }

  /** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
  class DirEntry(path: String) extends Entry(path) {
    val entries = mutable.HashMap[String, Entry]()

    override def isDirectory = true
    override def iterator: Iterator[Entry] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry = {
      if (directory) entries(name + "/")
      else entries(name)
    }
  }

  private def ensureDir(dirs: mutable.Map[String, DirEntry], path: String, zipEntry: ZipEntry): DirEntry =
    //OPT inlined from getOrElseUpdate; saves ~50K closures on test run.
    // was:
    // dirs.getOrElseUpdate(path, {
    //   val parent = ensureDir(dirs, dirName(path), null)
    //   val dir    = new DirEntry(path)
    //   parent.entries(baseName(path)) = dir
    //   dir
    // })
    dirs get path match {
      case Some(v) => v
      case None =>
        val parent = ensureDir(dirs, dirName(path), null)
        val dir    = new DirEntry(path)
        parent.entries(baseName(path)) = dir
        dirs(path) = dir
        dir
    }

  protected def getDir(dirs: mutable.Map[String, DirEntry], entry: ZipEntry): DirEntry = {
    if (entry.isDirectory) ensureDir(dirs, entry.getName, entry)
    else ensureDir(dirs, dirName(entry.getName), null)
  }
}
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
final class FileZipArchive(file: JFile) extends ZipArchive(file) {
  private[this] def openZipFile(): ZipFile = try {
    new ZipFile(file)
  } catch {
    case ioe: IOException => throw new IOException("Error accessing " + file.getPath, ioe)
  }

  private[this] class LazyEntry(
    name: String,
    time: Long,
    size: Int
  ) extends Entry(name) {
    override def lastModified: Long = time // could be stale
    override def input: InputStream = {
      val zipFile  = openZipFile()
      val entry    = zipFile.getEntry(name)
      val delegate = zipFile.getInputStream(entry)
      new FilterInputStream(delegate) {
        override def close(): Unit = { zipFile.close() }
      }
    }
    override def sizeOption: Option[Int] = Some(size) // could be stale
  }

  // keeps a file handle open to ZipFile, which forbids file mutation
  // on Windows, and leaks memory on all OS (typically by stopping
  // classloaders from being garbage collected). But is slightly
  // faster than LazyEntry.
  private[this] class LeakyEntry(
    zipFile: ZipFile,
    zipEntry: ZipEntry
  ) extends Entry(zipEntry.getName) {
    override def lastModified: Long = zipEntry.getTime
    override def input: InputStream = zipFile.getInputStream(zipEntry)
    override def sizeOption: Option[Int] = Some(zipEntry.getSize.toInt)
  }

  val (root, allDirs) = {
    val root = new DirEntry("/")
    val dirs = mutable.HashMap[String, DirEntry]("/" -> root)
    val zipFile = openZipFile()
    val entries = zipFile.entries()

    try {
      while (entries.hasMoreElements) {
        val zipEntry = entries.nextElement
        val dir = getDir(dirs, zipEntry)
        if (zipEntry.isDirectory) dir
        else {
          val f =
            if (ZipArchive.closeZipFile)
              new LazyEntry(
                zipEntry.getName(),
                zipEntry.getTime(),
                zipEntry.getSize().toInt
              )
            else
              new LeakyEntry(zipFile, zipEntry)

          dir.entries(f.name) = f
        }
      }
    } finally {
      if (ZipArchive.closeZipFile) zipFile.close()
    }
    (root, dirs)
  }

  def iterator: Iterator[Entry] = root.iterator

  def name         = file.getName
  def path         = file.getPath
  def input        = File(file).inputStream()
  def lastModified = file.lastModified

  override def sizeOption = Some(file.length.toInt)
  override def canEqual(other: Any) = other.isInstanceOf[FileZipArchive]
  override def hashCode() = file.hashCode
  override def equals(that: Any) = that match {
    case x: FileZipArchive => file.getAbsoluteFile == x.file.getAbsoluteFile
    case _                 => false
  }
}

final class ManifestResources(val url: URL) extends ZipArchive(null) {
  def iterator = {
    val root     = new DirEntry("/")
    val dirs     = mutable.HashMap[String, DirEntry]("/" -> root)
    val manifest = new Manifest(input)
    val iter     = manifest.getEntries().keySet().iterator().asScala.filter(_.endsWith(".class")).map(new ZipEntry(_))

    for (zipEntry <- iter) {
      val dir = getDir(dirs, zipEntry)
      if (!zipEntry.isDirectory) {
        val f = new Entry(zipEntry.getName) {
          override def lastModified = zipEntry.getTime()
          override def input        = resourceInputStream(path)
          override def sizeOption   = None
        }
        dir.entries(f.name) = f
      }
    }

    try root.iterator
    finally dirs.clear()
  }

  def name  = path
  def path: String = {
    val s = url.getPath
    val n = s.lastIndexOf('!')
    s.substring(0, n)
  }
  def input = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }

  override def canEqual(other: Any) = other.isInstanceOf[ManifestResources]
  override def hashCode() = url.hashCode
  override def equals(that: Any) = that match {
    case x: ManifestResources => url == x.url
    case _                => false
  }

  private def resourceInputStream(path: String): InputStream = {
    new FilterInputStream(null) {
      override def read(): Int = {
        if(in == null) in = Thread.currentThread().getContextClassLoader().getResourceAsStream(path)
        if(in == null) throw new RuntimeException(path + " not found")
        super.read()
      }

      override def close(): Unit = {
        super.close()
        in = null
      }
    }
  }
}
