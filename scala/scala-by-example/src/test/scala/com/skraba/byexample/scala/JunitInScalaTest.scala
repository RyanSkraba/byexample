package com.skraba.byexample.scala

import java.io.IOException

import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers.is
import org.junit.jupiter.api.Test

/**
  * Basic JUnit test written in scala.
  */
class JunitInScalaTest {
  @Test
  @throws[IOException]
  def testBasic() {
    {
      val p: GreeterBean = new GreeterBean(100, "Joe")
      assertThat(p.getName, is("Joe"))
      assertThat(p.getCount, is(100))
    }
    {
      val p: ScalaCaseClass = ScalaCaseClass(100, "Joe")
      assertThat(p.name, is("Joe"))
      assertThat(p.count, is(100))
    }
  }
}
