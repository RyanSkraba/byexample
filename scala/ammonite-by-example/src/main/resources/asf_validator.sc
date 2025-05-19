#!/usr/bin/env amm

/** Validating a release for the ASF. */

import mainargs.{ParserForClass, arg, main}

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ConsoleCfg
import com.skraba.byexample.scala.ammonite.validator.{AsfReleaseCfg, SvnCheck}

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
      "help" -> "Help, usage and release validation resources.",
      "svn" -> "Updating, downloading and checking the SVN repository."
    )
  )

  // Usage examples
  println(out.helpUse(cli, "help", "[--verbose]"))
  println(out.helpUse(cli, "svn", "[--verbose]"))
  println()

  println(asf.properties(out));
}

/** SVN management for the release: checking out, updating and fetching information.
  *
  * @param asf
  *   All of the parameters necessary to run the release.
  * @param out
  *   Colour configuration for the output.
  */
@main
def svn(asf: AsfReleaseCfg, out: ConsoleCfg): Unit = {
  SvnCheck(asf, out).check()
}
