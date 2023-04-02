package com.skraba.byexample.scala.markd

import org.docopt.Docopt
import org.docopt.DocoptExitException
import scala.collection.JavaConverters._
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
  class InternalDocoptException(msg: String, val docopt: String = Doc)
      extends RuntimeException(msg)

  val Tasks: Seq[Task] = Seq(BeautifyTask.Task)

  val Doc: String =
    """A driver for the various markdown utilities.
      |
      |Usage:
      |  MarkdGo [--debug] <command> [<ARGS>...]
      |
      |Options:
      |  -h --help    Show this screen.
      |  --version    Show version.
      |  --debug      Log extra information to the console while executing.
      |
      |Commands:
      |%s
      |
      |""".stripMargin.format {
      // Align the task subcommands and descriptions to the longest subcommand.
      val col = Tasks.map(_.cmd.length).max
      Tasks
        .map(task => s"%${col + 2}s  %s".format(task.cmd, task.description))
        .mkString("\n")
    }.trim

  /** Runs the tool. This does not handle any docopt exception automatically
    * while parsing the command line.
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
      throw new InternalDocoptException("Missing command", Doc)

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
        throw new InternalDocoptException(ex.getMessage, task.doc)
      case ex: DocoptExitException if ex.getMessage == null =>
        throw new InternalDocoptException(
          null,
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

  /** Command-line driver for beautifying a markdown file.
    */
  object BeautifyTask {

    val Doc: String =
      """Beautify a markdown file.
        |
        |Usage:
        |  MarkdGo beautify [--sortLinkRefs] FILE...
        |
        |Options:
        |  -h --help       Show this screen.
        |  --version       Show version.
        |  --sortLinkRefs  Sort the link references in the file (off by default)
        |  FILE            File(s) to beautify.
        |""".stripMargin.trim

    val Cmd = "beautify"

    val Description = "Reformat a markdown file."

    def go(opts: java.util.Map[String, AnyRef]): Unit = {

      val files: Seq[String] =
        opts
          .get("FILE")
          .asInstanceOf[java.lang.Iterable[String]]
          .asScala
          .toSeq

      val cfg: ParserCfg = new ParserCfg(
        sortLinkRefs = opts.get("--sortLinkRefs").toString.toBoolean
      )

      files
        .map(Path(_).toAbsolute)
        .flatMap {
          case f: File if f.exists => Some(f)
          case d: Directory if d.exists =>
            d.walkFilter(p =>
              p.isDirectory || """.*\.md$""".r.findFirstIn(p.name).isDefined
            ).map(_.toFile)
          case p =>
            throw new InternalDocoptException(
              s"The file ${p.name} doesn't exist."
            )
        }
        .foreach(f => {
          val md = Header.parse(f.slurp(), cfg)
          f.writeAll(md.build().toString)
        })
    }

    val Task: MarkdGo.Task = MarkdGo.Task(Doc, Cmd, Description, go)
  }

}
