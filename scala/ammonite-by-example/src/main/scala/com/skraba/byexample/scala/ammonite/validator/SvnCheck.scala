package com.skraba.byexample.scala.ammonite.validator

import com.skraba.docoptcli.AnsiConsole
import os.Path

import scala.xml.XML

/** SVN management for the release: checking out, updating and fetching information.
  *
  * @param asf
  *   All of the parameters necessary to run the release.
  * @param out
  *   Colour configuration for the output.
  */
case class SvnCheck(asf: AsfReleaseCfg, out: AnsiConsole) {

  /** Evaluate the default value once. */

  def check(check: Validator = Validator()): Validator = {
    out.vPrintln(asf.properties(out))

    // Checkout the project directory if it doesn't already exist
    if (!os.exists(asf.SvnBaseDir.get)) {
      out.ask(s"Create ${asf.SvnBaseDir.get}") {
        check.doStep(out, "Creating subversion directory ", asf.SvnBaseDir.get) {
          os.makeDir.all(asf.SvnBaseDir.get)
          os.proc("svn", "checkout", asf.SvnUrl.get, asf.SvnBaseDir.get).call(asf.SvnBaseDir.get / os.up)
        }
      }
    }

    // Update the SVN directory on request
    out.ask(s"Update ${asf.SvnBaseDir}?") {
      check.doStep(out, s"cd ${asf.SvnBaseDir} && svn update") { os.proc("svn", "update").call(asf.SvnBaseDir.get) }
    }

    if (!os.exists(asf.SvnDir.get)) {
      println(out.error(s"The project directory ${asf.SvnDir} doesn't exist after updating."))
      System.exit(1);
    }

    check
  }
}
