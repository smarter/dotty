/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;
import dotty.tools.dotc.util.SourcePosition;
import dotty.tools.io.AbstractFile;
import xsbti.Position;
import xsbti.TextEdit;

import java.io.File;
import java.util.Optional;

public class TextEditBridge implements TextEdit {
  private final Position position;
  private final String newText;

  public TextEditBridge(Position position, String newText) {
    this.position = position;
    this.newText = newText;
  }

  @Override
  public Position position() {
    return position;
  }

  @Override
  public String newText() {
    return newText;
  }
}
