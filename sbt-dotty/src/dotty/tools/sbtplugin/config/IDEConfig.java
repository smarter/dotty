package dotty.tools.sbtplugin.config;

import java.io.File;

public class IDEConfig {
  public String id;
  public String scalaVersion;
  public File[] sources;
  public String[] scalacArgs;
  public File[] depCp;
  public File target;

  // Constructor used by Jackson
  public IDEConfig() {
  }

  public IDEConfig(String id, String scalaVersion, File[] sources, String[] scalacArgs, File[] depCp, File target) {
     this.id = id;
     this.scalaVersion = scalaVersion;
     this.sources = sources;
     this.scalacArgs = scalacArgs;
     this.depCp = depCp;
     this.target =target;
  }
}
