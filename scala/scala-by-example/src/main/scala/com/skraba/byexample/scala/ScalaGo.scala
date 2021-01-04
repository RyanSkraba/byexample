package com.skraba.byexample.scala

import org.docopt.Docopt

import scala.collection.JavaConverters._

class ScalaGo(params: Either[GreeterBean, ScalaCaseClass]) {

  def count(): Int = params.fold(_.getCount, _.count)

  def name(): String = params.fold(_.getName, _.name)

  def go() = {
    for (x <- 1 to count()) {
      // Using the System.out.println for unit test captures.  Scala differs slightly.
      System.out.println("Hello, " + name() + "!")
    }
  }
}

/**
  * The same application as [[JavaScalaGo]] written in Scala.
  */
object ScalaGo {

  def main(args: Array[String]) {

    /** Recycle the doc with this class name. */
    val DOC = JavaScalaGo.DOC.replaceAll(
      classOf[JavaScalaGo].getSimpleName,
      ScalaGo.getClass.getSimpleName.replaceAll("\\$", "")
    )

    val docopt = new Docopt(DOC).withVersion("0.0.1-SNAPSHOT")

    val opts = docopt.parse(args.toList.asJava)

    try {
      var name = opts.get("--name")
      if (name == null) name = classOf[JavaScalaGo].getSimpleName
      val count = opts.get("--count")

      new ScalaGo(
        if (opts.get("--scala").toString.toBoolean)
          Left(new GreeterBean(count.toString.toInt, name.toString))
        else
          Right(ScalaCaseClass(count.toString.toInt, name.toString))
      ).go()

    } catch {
      case ex: Exception =>
        System.out.println(JavaScalaGo.DOC)
        ex.printStackTrace()
        System.exit(1)
    }
  }
}
