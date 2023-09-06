package com.skraba.byexample.scala.markd

import org.docopt.{Docopt, DocoptExitException}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters._
import scala.collection.immutable.Seq
import scala.reflect.io.{Directory, File, Path}
import scala.util.Try
import scala.util.matching.Regex

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

  val Tasks: Seq[Task] = Seq(BeautifyTask.Task, DateCountdownTask.Task)

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

  /** A helper method to process a list of files supplied on the command line.
    * @param files
    *   Files and directories relative to the current directory as string
    *   arguments. Markdown files are discovered recursively in the directory.
    * @param fn
    *   A method to call on each discovered file.
    */
  def processMd(files: Seq[String])(fn: File => Unit): Unit = files
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
    .foreach(fn)

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

      MarkdGo.processMd(files) { f =>
        {
          val md = Header.parse(f.slurp(), cfg)
          f.writeAll(md.build().toString)
        }
      }
    }

    val Task: MarkdGo.Task = MarkdGo.Task(Doc, Cmd, Description, go)
  }

  object DateCountdownTask {

    val Doc: String =
      """Look for tables with dates in a countdown format and fix them.
        |
        |Usage:
        |  MarkdGo datecount FILE...
        |
        |Options:
        |  -h --help       Show this screen.
        |  --version       Show version.
        |  FILE            File(s) to find and add date countdowns.
        |
        |If a table has a cell with a value "T 2024/06/03" (for example), all of the
        |cells in the same column with the format "T-100 2023/08/22" will have the date
        |adjusted to fit the countdown.  In this case, the value will be modified to
        |"T-100 2024/02/20".
        |
        |All of the cells in the same column with the format "T-XX 2023/08/22" will have
        |the countdown modified to fit the date.  In this case, the value would be
        |modified to "T-286 2023/08/22".
        |
        |Other cells will not be modified.
        |""".stripMargin.trim

    val Cmd = "datecount"

    val Description = "Adjust countdown cells in tables."

    def go(opts: java.util.Map[String, AnyRef]): Unit = {

      val files: Seq[String] =
        opts
          .get("FILE")
          .asInstanceOf[java.lang.Iterable[String]]
          .asScala
          .toSeq

      MarkdGo.processMd(files) { f =>
        f.writeAll(
          Header
            .parse(f.slurp())
            .replaceRecursively { case tbl: Table =>
              process(tbl)
            }
            .build()
            .toString
        )
      }
    }

    val YyyyMmDd = DateTimeFormatter.ofPattern("yyyy/MM/dd")

    val DateRegex: Regex =
      raw"(.*?)\b([*_(\[]*)T([-+]?[\d+X]+)?([*_)\]]*)\s+([*_(\[]*)(\d\d\d\d/\d\d/\d\d)([*_)\]]*)(.*)".r

    /** Given any table, update any rows with date information. */
    def process(tbl: Table): Table = {

      // A two dimensional ragged array of all the parsed table cells.
      val parsed = tbl.mds.map(row =>
        row.cells.map {
          case DateRegex(before, preT, t, postT, pre, date, post, after) =>
            Some(
              (
                Try(if (t == null) 0 else t.toLong).getOrElse(Long.MaxValue),
                before,
                preT,
                t,
                postT,
                pre,
                date,
                post,
                after
              )
            )
          case _ => None
        }
      )

      // Find the T0 date for each column, or None if there isn't any.  The T0 date is the measured relative
      // to the smallest magnitude T number discovered in the parsed cells for that column
      val t0s: Seq[Option[LocalDate]] = Seq.tabulate(tbl.colSize) { col =>
        parsed
          .filter(_.isDefinedAt(col))
          .flatMap(_(col))
          .sortBy(_._1.abs)
          .headOption
          .map(x => LocalDate.parse(x._7, YyyyMmDd).minusDays(x._1))
      }

      // For every single cell, if it has been parsed, then rewrite either the t number or the date according to the T0 value
      val mds = tbl.mds.zipWithIndex.map { row =>
        TableRow.from(row._1.cells.zipWithIndex.map {
          case (cell, col)
              if col < tbl.colSize && parsed
                .isDefinedAt(row._2) && parsed(row._2).isDefinedAt(col) =>
            parsed(row._2)(col)
              .map { x =>
                val base = t0s(col).getOrElse(LocalDate.now)

                // Resolve either the t number or the date according to the base.
                val (t, date) = if (x._1 == Long.MaxValue) {
                  val date = LocalDate.parse(x._7, YyyyMmDd)
                  (ChronoUnit.DAYS.between(base, date), date)
                } else {
                  (x._1, base.plusDays(x._1))
                }

                val number =
                  if (t == 0) "T"
                  else if (t > 0) s"T+$t"
                  else s"T$t"

                s"${x._2}${x._3}$number${x._5} ${x._6}${YyyyMmDd
                    .format(date)}${x._8}${x._9}"

              }
              .getOrElse(cell)
          case (cell, _) => cell
        }: _*)
      }
      tbl.copy(mds = mds)
    }

    val Task: MarkdGo.Task = MarkdGo.Task(Doc, Cmd, Description, go)
  }
}
