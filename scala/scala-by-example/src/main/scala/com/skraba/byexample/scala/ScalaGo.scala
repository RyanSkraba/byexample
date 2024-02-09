package com.skraba.byexample.scala

import org.docopt.{Docopt, DocoptExitException}
import scala.jdk.CollectionConverters._

class ScalaGo(params: Either[GreeterBean, ScalaCaseClass]) {

  def count(): Int = params.fold(_.getCount, _.count)

  def name(): String = params.fold(_.getName, _.name)

  def go(): Unit = {
    for (_ <- 1 to count())
      println("Hello, " + name() + "!")
  }
}

/** The same application as [[JavaScalaGo]] written in Scala.
  */
object ScalaGo {

  val Version: String = "0.0.1-SNAPSHOT"

  /** Recycle the doc with this class name. */
  val Doc: String = JavaScalaGo.DOC.replaceAll(
    classOf[JavaScalaGo].getSimpleName,
    ScalaGo.getClass.getSimpleName.replaceAll("\\$", "")
  )

  /** [[DocoptExitException]] constructors are protected. */
  class InternalDocoptException(
      msg: String,
      ex: Throwable = None.orNull,
      val docopt: String = Doc
  ) extends RuntimeException(msg, ex)

  /** Runs the tool. This does not handle any docopt exception automatically while parsing the command line.
    *
    * @param args
    *   command-line arguments as described in [[Doc]]
    */
  @throws[DocoptExitException]
  @throws[InternalDocoptException]
  def go(args: String*): Unit = {
    val opts = new Docopt(Doc)
      .withVersion(Version)
      .withExit(false)
      .parse(args.toList.asJava)

    var name = opts.get("--name")
    if (name == null) name = classOf[JavaScalaGo].getSimpleName
    val count = opts.get("--count")

    new ScalaGo(
      if (opts.get("--scala").toString.toBoolean)
        Left(new GreeterBean(count.toString.toInt, name.toString))
      else
        Right(ScalaCaseClass(count.toString.toInt, name.toString))
    ).go()
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
}
