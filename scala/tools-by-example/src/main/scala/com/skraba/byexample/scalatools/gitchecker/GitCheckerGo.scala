package com.skraba.byexample.scalatools.gitchecker

import com.tinfoiled.docopt4s.{MultiTaskMain, Task}

/** Do some analysis on git repositories */
object GitCheckerGo extends MultiTaskMain {
  override lazy val Name: String = "GitCheckerGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(RewriteDateTask)
  override lazy val Doc: String = "Do some analysis on git repositories.\n\n" + SimpleDoc
}
