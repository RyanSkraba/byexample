package com.skraba.byexample.scala;

import static scala.Predef.println;

import com.tinfoiled.docopt4s.shaded.docoptjava.Docopt;
import com.tinfoiled.docopt4s.shaded.docoptjava.DocoptExitException;
import java.util.Map;

/**
 * A Java application that can use scala code.
 *
 * <p>Note for testability, we use the scala.Predef.println for output. This wouldn't be necessary
 * for a pure-Java application.
 */
public class JavaScalaGo {

  public static final String VERSION = "0.0.1-SNAPSHOT";

  public static final String DOC =
      String.join(
          "\n",
          "A simple test driver for running Scala examples.",
          "",
          "Usage:",
          "  JavaScalaGo [--scala] [--name=NAME] [--count=COUNT]",
          "",
          "Options:",
          "  -h --help       Show this screen.",
          "  --version       Show version.",
          "  --scala         If present, uses the scala code to store the name",
          "                  and count.",
          "  --name=NAME     The user's name.",
          "  --count=COUNT   The number of times to say hello [default: 10]",
          "",
          "Says hello.");

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
      println("Hello, " + getName() + "!");
    }
  }

  /**
   * Runs the tool. This does not handle any docopt exception automatically while parsing the
   * command line.
   *
   * @param args command-line arguments as described in DOC . * @throws DocoptExitException
   * @throws InternalDocoptException
   */
  public static void go(String... args) throws DocoptExitException, InternalDocoptException {
    Map<String, Object> opts = new Docopt(DOC).withVersion(VERSION).withExit(false).parse(args);

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
  }

  /** Main entry point to the application. */
  public static void main(String[] args) {
    // All of the command is executed in the go method, and this wraps DocOpt and exceptions for
    // console feedback.
    try {
      go(args);
    } catch (DocoptExitException ex) {
      if (ex.getMessage() != null)
        (ex.getExitCode() == 0 ? System.out : System.err).println(ex.getMessage());
      System.exit(ex.getExitCode());
    } catch (InternalDocoptException ex) {
      System.out.println(ex.getDocopt());
      if (ex.getMessage() != null) {
        System.out.println();
        System.out.println(ex.getMessage());
      }
      System.exit(1);
    } catch (Exception ex) {
      System.err.println(DOC);
      System.err.println();
      ex.printStackTrace();
      System.exit(1);
    }
  }

  /**
   * {@link com.tinfoiled.docopt4s.shaded.docoptjava.DocoptExitException} constructors are
   * protected.
   */
  public static class InternalDocoptException extends RuntimeException {

    private final String docopt;

    public InternalDocoptException(String message, Throwable cause, String docopt) {
      super(message, cause);
      this.docopt = docopt;
    }

    public String getDocopt() {
      return docopt;
    }
  }
}
