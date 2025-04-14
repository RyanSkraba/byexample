package com.skraba.byexample.webclient

import com.skraba.docoptcli.DocoptCliGo
import com.skraba.docoptcli.DocoptCliGo.Task

/** A driver for running a Scalatra web server. */
object WebClientGo extends DocoptCliGo {
  override lazy val Cli: String = "WebClientGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(GetTask)
  override lazy val Doc: String = "A driver to demonstrate http calls.\n\n" + SimpleDoc
}
