package com.skraba.byexample.scalatools

import java.nio.file.Path
import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}

package object gitchecker {

  case class GitException(exitCode: Int, stdout: String, stderr: String) extends Exception()

  /** Runs git as a process with arguments, erroring if the exist code is non-zero
    *
    * @param args
    *   The git arguments ("add", ".")
    * @param repo
    *   The directory of the repository to run in
    * @return
    *   The output of the command
    */
  @throws[GitException]
  def git(repo: Path, args: String*): String = git(repo, args)

  /** Runs git as a process with arguments and environment, erroring if the exist code is non-zero
    *
    * @param repo
    *   The directory of the repository to run in
    * @param args
    *   The git arguments ("add", ".")
    * @param extraEnv
    *   Environment variables to add to the process
    * @return
    *   The output of the command
    */
  @throws[GitException]
  def git(repo: Path, args: Seq[String], extraEnv: (String, String)*): String = {
    val stdout = mutable.ListBuffer[String]()
    val stderr = mutable.ListBuffer[String]()
    val logger = ProcessLogger(stdout += _, stderr += _)
    val exitCode = Process("git" +: args, repo.toFile, extraEnv: _*).!(logger)

    if (exitCode != 0) throw new GitException(exitCode, stdout.mkString("\n"), stderr.mkString("\n"))
    stdout.mkString("\n")
  }
}
