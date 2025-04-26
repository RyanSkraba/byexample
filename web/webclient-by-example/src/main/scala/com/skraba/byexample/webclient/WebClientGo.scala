package com.skraba.byexample.webclient

import com.skraba.docoptcli.DocoptCliGo
import com.skraba.docoptcli.DocoptCliGo.Task

/** A driver for running a Scalatra web server. */
object WebClientGo extends DocoptCliGo {

  case class SimpleResponse(code: Int, body: String)

  trait SimpleClient {

    /** Make a GET request to the server. */
    def get(path: String): SimpleResponse

    /** Make a POST request to the server. */
    def post(path: String, payload: String): SimpleResponse

    /** Make a PUT request to the server. */
    def put(path: String, payload: String): SimpleResponse

    /** Make a DELETE request to the server. */
    def delete(path: String): SimpleResponse
  }

  override lazy val Cli: String = "WebClientGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(GetTask, PostTask)
  override lazy val Doc: String = "A driver to demonstrate http calls.\n\n" + SimpleDoc
}
