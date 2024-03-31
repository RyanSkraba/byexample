package com.skraba.byexample.scala.markd

import org.docopt.{Docopt, DocoptExitException}

import scala.jdk.CollectionConverters._
import scala.reflect.io.{Directory, File, Path}

/** A driver for the various utilities that use the [[Markd]] model.
  */
object MarkdGo {

  val Version: String = "0.0.1-SNAPSHOT"

  /** The subcommands that this driver supports.
    * @param doc
    *   The [[Docopt]] for the subcommand.
    * @param cmd
    *   The subcommand token.
    * @param description
    *   The short description for the subcommand.
    * @param go
    *   The method to call with the argument map from the subcommand docopts.
    */
  case class Task(
      doc: String,
      cmd: String,
      description: String,
      go: java.util.Map[String, AnyRef] => Unit
  )

  /** [[DocoptExitException]] constructors are protected. */
  class InternalDocoptException(
      msg: String,
      ex: Throwable = None.orNull,
      val docopt: String = Doc
  ) extends RuntimeException(msg, ex)

  val Tasks: Seq[Task] = Seq(BeautifyTask.Task, DateCountdownTask.Task)

  val Doc: String =
    """A driver for the various markdown utilities.
      |
      |Usage:
      |  MarkdGo [--debug] <command> [args...]
      |
      |Options:
      |  -h --help  Show this screen.
      |  --version  Show version.
      |  --debug    Log extra information to the console while executing.
      |
      |Commands:
      |%s
      |
      |""".stripMargin.format {
      // Align the task subcommands and descriptions to the longest subcommand.
      val col = (0 +: Tasks.map(_.cmd.length)).max
      Tasks
        .map(task => s"%${col + 2}s  %s".format(task.cmd, task.description))
        .mkString("\n")
    }.trim

  /** Runs the tool. This does not handle any docopt exception automatically while parsing the command line.
    *
    * @param args
    *   command-line arguments as described in [[Doc]]
    */
  @throws[DocoptExitException]
  @throws[InternalDocoptException]
  def go(args: String*): Unit = {
    // Java docopts doesn't support ignoring options after the command, so strip them first.
    val mainArgs: Seq[String] = if (args.nonEmpty) {
      val (options, cmd) = args.span(_.startsWith("-"))
      if (cmd.isEmpty) options :+ "???" else options :+ cmd.head
    } else Seq("--help")

    // Get the command, but throw exceptions for --help and --version
    val cmd = new Docopt(Doc)
      .withVersion(Version)
      .withOptionsFirst(true)
      .withExit(false)
      .parse(mainArgs.asJava)
      .get("<command>")
      .asInstanceOf[String]

    // This is only here to rewrap any internal docopt exception with the current docopt
    if (cmd == "???")
      throw new InternalDocoptException("Missing command", docopt = Doc)

    // Reparse with the specific command.
    val task = Tasks
      .find(_.cmd == cmd)
      .getOrElse(throw new InternalDocoptException(s"Unknown command: $cmd"))

    try {
      val opts = new Docopt(task.doc)
        .withVersion(Version)
        .withExit(false)
        .parse(args.toList.asJava)
      task.go(opts)
    } catch {
      // This is only here to rewrap any internal docopt exception with the current docopt
      case ex: InternalDocoptException =>
        throw new InternalDocoptException(ex.getMessage, ex, task.doc)
      case ex: DocoptExitException if ex.getMessage == null =>
        throw new InternalDocoptException(
          null,
          ex,
          task.doc
        )
    }
  }

  def main(args: Array[String]): Unit = {
    // All of the command is executed in the go method, and this wraps DocOpt and exceptions for
    // console feedback.
    try {
      go(args: _*)
    } catch {
      case ex: DocoptExitException =>
        Option(if (ex.getExitCode == 0) System.out else System.err)
          .foreach(ps => {
            if (ex.getMessage != null) ps.println(ex.getMessage)
            else ps.println(MarkdGo.Doc)
          })
        System.exit(ex.getExitCode)
      case ex: InternalDocoptException =>
        println(ex.docopt)
        if (ex.getMessage != null) {
          println()
          println(ex.getMessage)
        }
        System.exit(1)
      case ex: Exception =>
        println(Doc)
        println()
        ex.printStackTrace()
        System.exit(1)
    }
  }

  /** A helper method to process a list of files supplied on the command line.
    * @param files
    *   Files and directories relative to the current directory as string arguments. Markdown files are discovered
    *   recursively in the directory.
    * @param fn
    *   A method to call on each discovered file.
    */
  def processMd(files: Seq[String])(fn: File => Unit): Unit = files
    .map(Path(_).toAbsolute)
    .flatMap {
      case f: File if f.exists => Some(f)
      case d: Directory if d.exists =>
        d.walkFilter(p => p.isDirectory || """.*\.md$""".r.findFirstIn(p.name).isDefined).map(_.toFile)
      case p =>
        throw new InternalDocoptException(
          s"The file ${p.name} doesn't exist."
        )
    }
    .foreach(fn)
}
