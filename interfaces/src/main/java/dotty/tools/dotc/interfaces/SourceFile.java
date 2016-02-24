package dotty.tools.dotc.interfaces;

import java.io.File;

/** A source file that may correspond to a file on disk but may also be virtual.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface SourceFile {
  /** The name of this file, note that two files may have the same name. */
  String name();

  /** The path of this file, this might be a virtual path of an unspecified format. */
  String path();

  /** The content of this file as seen by the compiler. */
  char[] content();
}
