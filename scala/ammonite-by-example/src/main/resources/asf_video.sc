#!/usr/bin/env amm

import $ivy.`org.apache.commons:commons-csv:1.11.0`
import mainargs.{Flag, arg, main}
import org.apache.commons.csv._

import scala.jdk.CollectionConverters._
import scala.reflect.io.Streamable

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util
local_import_util.load("scala-by-example")
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.{ConsoleCfg, FileMaker}
import com.skraba.byexample.scala.ammonite.video.Ffmpeg
import com.skraba.byexample.scala.ammonite.video.Card

// ==========================================================================
// Top level variables available to the script

object Files {

  /** Recording information. */
  val SrcRecordings: os.Path = os.pwd / "coceu-2024-recordings.csv"

  /** Speaker information. */
  val SrcSpeakers: os.Path = os.pwd / "coceu-2024-speakers.csv"

  /** Completed videos can be manually moved here, and subsequently ignored in future processing. */
  val DstUploaded: os.Path = os.pwd / "uploaded"

  /** Files that can be saved between runs. */
  val TargetDirTitleCardSvg: os.Path = os.pwd / "title-cards"
  val TargetDirDescription: os.Path = os.pwd / "descriptions"

  /** Temporary files to be regenerated. */
  val TargetDir: os.Path = os.pwd / "target"
  val TargetDirTitleCardPng: os.Path = TargetDir / "title-cards-png"
  val TargetDirTitleCardMp4: os.Path = TargetDir / "title-cards-mp4"
  val TargetDirSessionTrimmedMp4: os.Path = TargetDir / "session-trimmed"
  val TargetDirSessionNormalizedMp4: os.Path = TargetDir / "session-trimmed-normalized"
  val TargetDirSessionFadedMp4: os.Path = TargetDir / "session-trimmed-faded"
  val TargetDirSessionFinalMp4: os.Path = TargetDir / "session-final"
}

val FadeStart: Double = 0.5d
val FadeEnd: Double = 0.5d

// ==========================================================================
// Top level Initialization

case class Context(
    templateMaker: FileMaker = FileMaker(out = ConsoleCfg()),
    templateFf: Ffmpeg = Ffmpeg(out = ConsoleCfg()),
    reencodeGlobal: Boolean = true
)
var ctx = Context()
private def updateCtx(out: ConsoleCfg, overwrite: Flag, cmdLog: Option[os.Path] = None): Context = {
  ctx.copy(
    templateFf = cmdLog
      .map(f => ctx.templateFf.copy(out = out, cmdLog = Ffmpeg.cmdLogToFile(f)))
      .getOrElse(ctx.templateFf.copy(out = out)),
    templateMaker = ctx.templateMaker.copy(out = out, overwrite = overwrite.value)
  )
}

/** A template card used to create the others in this script. */
lazy val templateCard = Card(
  genSvg = _ => "",
  filename = "",
  maker = ctx.templateMaker,
  dirSvg = Files.TargetDirTitleCardSvg,
  dirPng = Files.TargetDirTitleCardPng,
  dirMp4 = Files.TargetDirTitleCardMp4
)

/** if a new recordings sheet as made available, then move it from Downloads to this directory. */
if (os.exists(os.home / os.RelPath("Downloads/20240603 Community over Code Recordings - Sheet1.csv")))
  os.move.over(
    os.home / os.RelPath("Downloads/20240603 Community over Code Recordings - Sheet1.csv"),
    Files.SrcRecordings
  )

// ==========================================================================
// Helper classes

case class Session(
    id: String,
    title: String,
    speakers: String,
    track: String,
    startFile: Option[os.Path],
    startTime: Option[String],
    endFile: Option[os.Path],
    endTime: Option[String]
) {
  private lazy val titleLines: Seq[String] = title.replaceAll("&", "&amp;").split("\n").toSeq.map(_.trim)

  private lazy val speakerLines: Seq[String] = speakers.split(",").toSeq.map(_.trim)

  lazy val filename: String = Seq("0" * (3 - id.length) + id, titleLines.head, speakerLines.head)
    .map(_.replaceAll("[^a-zA-Z0-9]+", "_"))
    .mkString("__")

  lazy val titleCard: Card = templateCard.copy(
    genSvg = out => {
      val withTitle = Card.multiReplaceInTemplate(Session.TitleCardTemplate, "TITLE", titleLines)
      out.vPrint(out.blue("."))
      val withAuthor = Card.multiReplaceInTemplate(withTitle, "NAME", speakerLines)
      out.vPrint(out.blue("."))
      withAuthor
    },
    filename = filename
  )

  private val trimMp4StartPath: os.Path = Files.TargetDirSessionTrimmedMp4 / (filename + ".1.mp4")
  private val trimMp4EndPath: os.Path = Files.TargetDirSessionTrimmedMp4 / (filename + ".2.mp4")
  private val trimMp4Path: os.Path = Files.TargetDirSessionTrimmedMp4 / (filename + ".mp4")
  private val normalizedMp4Path: os.Path = Files.TargetDirSessionNormalizedMp4 / (filename + ".mp4")
  private val fadedMp4Path: os.Path = Files.TargetDirSessionFadedMp4 / (filename + ".mp4")
  private val finalMp4Path: os.Path = Files.TargetDirSessionFinalMp4 / (filename + ".mp4")

  private val trimMp4Start: FileMaker =
    if (startFile.isEmpty) FileMaker(None)
    else
      (
        if (endFile.isEmpty) ctx.templateMaker.yellow(trimMp4Path, "trimMp4Start") _
        else ctx.templateMaker.yellow(trimMp4StartPath, "trimMp4Start") _
      ) { out =>
        ctx.templateFf
          .mp4(startFile.get)
          .trim(out, startTime, if (endFile.nonEmpty) None else endTime, reencode = ctx.reencodeGlobal)
      }

  private val trimMp4End: FileMaker =
    if (endFile.isEmpty) FileMaker(None)
    else
      ctx.templateMaker.yellow(trimMp4EndPath, "trimMp4End") { out =>
        ctx.templateFf.mp4(endFile.get).trim(out, None, endTime, reencode = ctx.reencodeGlobal)
      }

  val trimMp4: FileMaker =
    if (trimMp4Start.isEmpty) FileMaker(None)
    else if (trimMp4End.isEmpty) trimMp4Start
    else {
      ctx.templateMaker.yellow(trimMp4Path, "trimMp4") {
        ctx.templateFf
          .mp4(_)
          .concat(
            concatTxt = Files.TargetDirSessionTrimmedMp4 / (filename + ".concat.txt"),
            srcMp4s = Seq(trimMp4Start.dst.get, trimMp4End.dst.get)
          )
      }
    }

  val normalizedMp4: FileMaker =
    if (trimMp4.isEmpty) FileMaker(None)
    else
      ctx.templateMaker.cyan(normalizedMp4Path, "normalizedMp4") { ctx.templateFf.mp4(trimMp4.dst.get).normalizeMp4(_) }

  val fadedMp4: FileMaker =
    if (trimMp4.isEmpty) FileMaker(None)
    else
      ctx.templateMaker.cyan(fadedMp4Path, "fadedMp4") { out =>
        trimMp4.dst
        val src = Seq(trimMp4, normalizedMp4).flatMap(_.dstPath).filter(os.exists).maxBy(os.mtime)
        ctx.templateFf.mp4(src).fade(out, FadeStart, FadeEnd)
      }

  val finalMp4: FileMaker =
    if (trimMp4.isEmpty) FileMaker(None)
    else
      ctx.templateMaker.red(finalMp4Path, "finalMp4") { out =>
        trimMp4.dst
        val src = Seq(trimMp4, normalizedMp4, fadedMp4).flatMap(_.dstPath).filter(os.exists).maxBy(os.mtime)
        val frameRate = ctx.templateFf.mp4(src).frameRate.toInt
        ctx.templateFf
          .mp4(out)
          .concat(
            concatTxt = Files.TargetDirSessionTrimmedMp4 / (filename + ".concat.txt"),
            srcMp4s = Seq(
              titleCard.mp4(ctx.templateFf, frameRate).dst.get,
              Session.Sponsors(id.hashCode % Session.Sponsors.size).mp4(ctx.templateFf, frameRate).dst.get,
              src,
              Session.NextStop(id.hashCode % Session.NextStop.size).mp4(ctx.templateFf, frameRate).dst.get,
              Session.Sponsors((1 + id.hashCode) % Session.Sponsors.size).mp4(ctx.templateFf, frameRate).dst.get
            )
          )
      }
}

object Session {

  import scala.xml.{Attribute, Elem, Node, Null, XML}
  import scala.xml.transform.{RewriteRule, RuleTransformer}

  val InkNs: String = "http://www.inkscape.org/namespaces/inkscape"

  /** The template file containing the SVG resources. */
  private lazy val Template: Node = XML.loadString(os.read(os.pwd / "title-card-template.svg"))

  /** All of the inkscape layers in the template. */
  private lazy val TemplateLayers: Seq[Node] = {
    def findLayers(in: Node): Seq[Node] = {
      in match {
        case elem: Elem if (elem \@ s"{$InkNs}groupmode") == "layer" => Seq(elem) ++ elem.child.flatMap(findLayers)
        case elem: Elem                                              => elem.child.flatMap(findLayers)
        case other                                                   => Seq.empty
      }
    }
    findLayers(Template)
  }

  /** The template file containing only the title card information. */
  lazy val TitleCardTemplate: String =
    stripLayers("Title card", "Logo", "Conference title", "Stars", "Background").toString

  /** The sponsor cards in MP4 format. */
  lazy val Sponsors: Seq[Card] =
    TemplateLayers
      .map(_ \@ s"{$InkNs}label")
      .filter(_ startsWith "Sponsor mix")
      .map(layer =>
        templateCard.copy(
          genSvg = _ => stripLayers("Sponsor background", layer, "Stars", "Background").toString,
          filename = layer.replaceAll("[^a-zA-Z0-9]+", "_")
        )
      )

  /** The sponsor cards in MP4 format. */
  lazy val NextStop: Seq[Card] =
    TemplateLayers
      .map(_ \@ s"{$InkNs}label")
      .filter(_ startsWith "Next stop")
      .map(layer =>
        templateCard.copy(
          genSvg = _ => stripLayers(layer, "Stars", "Background").toString,
          filename = layer.replaceAll("[^a-zA-Z0-9]+", "_")
        )
      )

  private def stripLayers(keep: String*): Node = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if (elem \@ s"{$InkNs}groupmode") == "layer" =>
          // Remove it, if it isn't in the keep list, or set it visible.
          if (!keep.contains(n \@ s"{$InkNs}label")) Seq.empty
          else elem.copy(attributes = elem.attributes.append(Attribute(null, "style", "display:inline", Null)))
        case other => other
      }
    }
    new RuleTransformer(rule).transform(Template).head
  }

  /** Read the session information from a CSV file (absolute or relative to the current directory).
    *
    * @param file
    *   The file name to read from.
    * @param ids
    *   If present, a list of ids to include (excluding all others). The ID is the row number where the first row is
    *   "1".
    * @return
    *   The sessions read from the file.
    */
  def readSessions(
      file: String = Files.SrcRecordings.toString,
      ids: Option[Set[String]] = None
  ): Seq[Session] = {

    // Read the input file entirely as a CSV file, and convert it to Session
    val csvFormat: CSVFormat = CSVFormat.DEFAULT.builder.setHeader().build()
    val csvLines = Streamable.closing(new java.io.FileReader(file)) { new CSVParser(_, csvFormat).getRecords.asScala }
    val sessions = csvLines.zipWithIndex.map { case (csv, i) =>
      Session(
        id = (i + 1).toString,
        title = csv.get("Title"),
        speakers = csv.get("Speakers"),
        track = csv.get("Track"),
        startFile = Some(csv.get("Start file")).filter(_.nonEmpty).map(os.pwd / _),
        startTime = Some(csv.get("Start time")).filter(_.nonEmpty),
        endFile = Some(csv.get("End file")).filter(_.nonEmpty).map(os.pwd / _),
        endTime = Some(csv.get("End time")).filter(_.nonEmpty)
      )
    }.toSeq

    // Limit it to the specified IDs
    ids
      .map(idSet => sessions.filter(session => idSet(session.id)))
      .getOrElse(sessions)
      .filterNot(_.track.contains("Poster"))
  }
}

case class Speaker(firstName: String, lastName: String, tagLine: String, bio: String)

case class SessionDescription(id: String, title: String, summary: String, speakers: Seq[Speaker]) {
  lazy val filename: String = Seq(title, speakers.head.firstName + "_" + speakers.head.lastName)
    .map(_.replaceAll("[^a-zA-Z0-9]+", "_"))
    .mkString("__")

  private val descriptionTxtPath: os.Path = Files.TargetDirDescription / (filename + ".txt")

  lazy val txt: FileMaker = ctx.templateMaker
    .green(descriptionTxtPath, "txt") {
      os.write.over(
        _,
        s"""${title.trim}
           |
           |${summary.trim}
           |
           |""".stripMargin + speakers
          .map(speaker => s"""*${speaker.firstName.trim} ${speaker.lastName.trim}*
               |${speaker.tagLine.trim}
               |
               |${speaker.bio.trim}""".stripMargin)
          .mkString("\n\n")
      )
    }
}

object SessionDescription {

  /** Read the session information from a CSV file (absolute or relative to the current directory).
    *
    * @param file
    *   The file name to read from.
    * @param ids
    *   If present, a list of session ids to include (excluding all others).
    * @return
    *   The session descriptions read from the file.
    */
  def readFromCsv(
      file: String = Files.SrcSpeakers.toString,
      ids: Option[Set[String]] = None
  ): Seq[SessionDescription] = {

    // Read the input file entirely as a CSV file, and extract the interesting information
    val csvFormat: CSVFormat = CSVFormat.DEFAULT.builder.setHeader().build()
    val csvLines = Streamable.closing(new java.io.FileReader(file)) { new CSVParser(_, csvFormat).getRecords.asScala }
    val descriptions = csvLines
      .map { csv =>
        SessionDescription(
          id = csv.get("Session Id"),
          title = csv.get("Title"),
          summary = csv.get("Description"),
          speakers = Seq(
            Speaker(
              firstName = csv.get("FirstName"),
              lastName = csv.get("LastName"),
              tagLine = csv.get("TagLine"),
              bio = csv.get("Bio")
            )
          )
        )
      }
      .toSeq
      .groupBy(_.id)
      .values
      .map { onePer: Seq[SessionDescription] =>
        SessionDescription(
          id = onePer.map(_.id).filterNot(_.isBlank).head,
          title = onePer.map(_.title).filterNot(_.isBlank).head,
          summary = onePer.map(_.summary).head,
          speakers = onePer.flatMap(_.speakers)
        )
      }
      .toSeq

    // Limit it to the specified IDs
    ids
      .map(idSet => descriptions.filter(desc => idSet(desc.id)))
      .getOrElse(descriptions)
  }
}

// ==========================================================================
// Main tasks

@arg(doc = "Print help to the console.")
@main
def help(cfg: ConsoleCfg): Unit = {
  // The help header includes all of the subcommands
  val cli = "asf_video.sc"
  println(
    cfg.helpHeader(
      cli,
      "Generate files for ASF video presentations",
      "description" -> "Generate the description pages for the videos",
      "titleCard" -> "Generate the title card SVG and PNG",
      "video" -> "Generate the finished videos",
      "normalize" -> "Audio normalization for all MP4 in the current directory"
    )
  )

  println(cfg.helpUse(cli, "description", "--verbose", "--overwrite", "--ids", "1"))
  println(cfg.helpUse(cli, "titleCard", "--verbose", "--overwrite", "--ids", "1"))
  println(cfg.helpUse(cli, "video", "--verbose", "--overwrite", "--noSkip"))
  println(cfg.helpUse(cli, "normalize", "--verbose", "--overwrite"))
}

@arg(doc = "Generate the description pages for the videos")
@main
def description(
    overwrite: Flag,
    ids: Option[String] = None,
    cmdLog: Option[os.Path] = None,
    out: ConsoleCfg
): Unit = {

  updateCtx(out, overwrite, cmdLog)

  // Read all of the sessions, but only include the numeric ids specified (if any)
  val descriptions = SessionDescription.readFromCsv(ids = ids.map(_.split(",").toSet))

  out.vPrintln(out.green("Generating descriptions"))

  descriptions.foreach { desc =>
    out.vPrint(out.bold(desc.filename) + ".")
    desc.txt
    out.vPrintln(".")
  }
}

@arg(doc = "Generate the title card SVG and PNG")
@main
def titleCard(
    overwrite: Flag,
    ids: Option[String] = None,
    cmdLog: Option[os.Path] = None,
    out: ConsoleCfg
): Unit = {

  updateCtx(out, overwrite, cmdLog)

  // Read all of the sessions, but only include the numeric ids specified (if any)
  val sessions = Session.readSessions(ids = ids.map(_.split(",").toSet)).filter(_.startFile.nonEmpty)

  out.vPrintln(out.green("Generating title cards"))

  (Session.Sponsors ++ Session.NextStop ++ sessions.map(_.titleCard)).foreach { card =>
    out.vPrint(out.bold(card.filename) + ".")
    card.png.dst
    out.vPrintln(".")
  }
}

@arg(doc = "Generate the finished videos")
@main
def video(
    overwrite: Flag,
    normalize: Flag,
    fade: Flag,
    reencode: Flag,
    noSkip: Flag,
    printCmd: Flag,
    ids: Option[String] = None,
    cmdLog: Option[os.Path] = None,
    out: ConsoleCfg
): Unit = {

  ctx = updateCtx(out, overwrite, cmdLog).copy(reencodeGlobal = reencode.value)

  // Read all of the sessions, but only include the numeric ids specified (if any)
  val sessions = Session.readSessions(ids = ids.map(_.split(",").toSet)).filter(_.startFile.nonEmpty)
  // By default, skip all of the sessions that were already uploaded
  val skip: Set[String] = if (noSkip.value) Set() else os.list(Files.DstUploaded).map(_.baseName).toSet

  out.vPrintln(out.green("Trimming videos"))

  for (session <- sessions if !skip(session.filename)) {
    out.vPrint(out.bold(session.filename) + ".")
    if (normalize.value) session.normalizedMp4.dst
    if (fade.value) session.fadedMp4.dst
    session.finalMp4.dst
    out.vPrintln(".")
  }
}

@arg(doc = "Audio normalization for all MP4 in the current directory")
@main
def normalize(
    overwrite: Flag,
    cmdLog: Option[os.Path] = None,
    out: ConsoleCfg
): Unit = {

  for (mp4 <- os.list(os.pwd).filter(_.ext.toLowerCase == "mp4")) {

    out.vPrint(out.bold(mp4) + ".")

    val normalizedMp4 = Files.TargetDirSessionNormalizedMp4 / mp4.last
    os.makeDir.all(Files.TargetDirSessionNormalizedMp4)

    if (!os.exists(normalizedMp4) || overwrite.value) {
      out.vPrint(out.blue("(pre"))
      if (os.exists(normalizedMp4)) out.vPrint(out.blue("*"))
      ctx.templateFf.mp4(mp4).normalizeMp4(normalizedMp4)
      out.vPrint(out.blue(".)"))
    }
  }
}
