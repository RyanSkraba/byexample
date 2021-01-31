package com.skraba.byexample.scala;

import java.util.Map;
import org.docopt.Docopt;

/** A Java application that can use scala code. */
public class JavaScalaGo {

  public static final String DOC =
      "A simple test driver for running Scala examples.\n" //
          + "\n" //
          + "Usage:\n" //
          + "  JavaScalaGo [--scala] [--name=NAME] [--count=COUNT]\n" //
          + "\n" //
          + "Options:\n" //
          + "  -h --help       Show this screen.\n" //
          + "  --version       Show version.\n" //
          + "  --scala         If present, uses the scala code to store the name\n"
          + "                  and count.\n" //
          + "  --name=NAME     The user's name.\n" //
          + "  --count=COUNT   The number of The times to say hello [default: 10]\n" //
          + "\n" //
          + "Says hello."; //

  /** The java bean being used for the greeting. */
  private final GreeterBean javaGreeter;

  /** The scala case class used for the greeting. */
  private final ScalaCaseClass scalaGreeter;

  public JavaScalaGo(ScalaCaseClass scalaGreeter) {
    this.javaGreeter = null;
    this.scalaGreeter = scalaGreeter;
  }

  public JavaScalaGo(GreeterBean javaGreeter) {
    this.javaGreeter = javaGreeter;
    this.scalaGreeter = null;
  }

  public int getCount() {
    return javaGreeter != null ? javaGreeter.getCount() : scalaGreeter.count();
  }

  public String getName() {
    return javaGreeter != null ? javaGreeter.getName() : scalaGreeter.name();
  }

  private void go() {
    for (int i = 0; i < getCount(); i++) {
      System.out.println("Hello, " + getName() + "!");
    }
  }

  /** Main entry point to the application. */
  public static void main(String[] args) {
    Docopt docopt = new Docopt(DOC).withVersion("0.0.1-SNAPSHOT");
    // .withExit(false)
    Map<String, Object> opts = docopt.parse(args);
    // System.out.println(opts);

    try {
      String name = (String) opts.get("--name");
      if (name == null) {
        name = JavaScalaGo.class.getSimpleName();
      }
      String count = (String) opts.get("--count");

      // whether to store the name and count in a Scala case class or Java bean
      JavaScalaGo go =
          (Boolean) opts.get("--scala")
              ? new JavaScalaGo(ScalaCaseClass.apply(Integer.parseInt(count), name))
              : new JavaScalaGo(new GreeterBean(Integer.parseInt(count), name));

      go.go();
    } catch (Exception ex) {
      System.err.println(DOC);
      ex.printStackTrace();
      System.exit(1);
    }
  }
}
