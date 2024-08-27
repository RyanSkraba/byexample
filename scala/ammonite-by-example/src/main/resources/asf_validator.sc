#!/usr/bin/env amm

/** Validating a release for the ASF. */

import mainargs.{ParserForClass, arg, main}

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util
local_import_util.load("scala-by-example")
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ConsoleCfg
import com.skraba.byexample.scala.ammonite.validator.AsfReleaseCfg

// ==========================================================================
// Top level variables available to the script

implicit def asfReleaseConfigParser: ParserForClass[AsfReleaseCfg] = ParserForClass[AsfReleaseCfg]

// ==========================================================================
// Help

@arg(doc = "Print help to the console.")
@main
def help(asf: AsfReleaseCfg, out: ConsoleCfg): Unit = {
  // The help header includes all of the subcommands
  val cli = "asf_validator.sc"
  println(
    out.helpHeader(
      cli,
      "Validating a release for the ASF",
      "help" -> "Help, usage and release validation resources."
    )
  )

  // Usage examples
  println(out.helpUse(cli, "help", "[--verbose]"))
  println()

  println(asf.properties(out));
}

/** Print out some useful information about the environment variables used to validate ASF releases.
  *
  * @param out
  *   Colour configuration for the output
  */
@main
def svn(asf: AsfReleaseCfg, out: ConsoleCfg): Unit = {
  out.vPrintln(asf.properties(out))
  if (!os.exists(asf.SvnDir.get)) {
    out.vPrintln(
      out.warn("Creating subversion directory:", bold = true) + out.warn(
        asf.SvnDir
      )
    )
    os.makeDir.all(asf.SvnDir.get / os.up)
    val cmd =
      os.proc("svn", "checkout", asf.SvnUrl.get, asf.SvnBaseDir.get)
        .call(asf.SvnBaseDir.get / os.up)
    println(cmd.out.lines().mkString("\n"))
  }
  println(
    os.proc("svn", "update").call(asf.SvnBaseDir.get).out.lines().mkString("\n")
  )
  println(os.proc("svn", "info").call(asf.SvnDir.get).out.lines().mkString("\n"))
}
