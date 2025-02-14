package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGo
import com.skraba.docoptcli.DocoptCliGo.Task

/** A driver for running a Scalatra web server.
  */
object ScalatraGo extends DocoptCliGo {
  override lazy val Cli: String = "ScalatraGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(HelloWorldTask, ServeJarResourceTask)
  override lazy val Doc: String = "A driver to launch a web server.\n\n" + SimpleDoc
}
