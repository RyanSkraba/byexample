package com.skraba.byexample.scala

/**
  * Examples for using Scaladoc.
  *
  * =Markup=
  *  - [[https://docs.scala-lang.org/overviews/scaladoc/overview.html Scaladoc overview]]
  *  - [[com.skraba.byexample.scala.ScalaDocExample]] Internal link to scaladoc
  *  - `monospace`
  *  -  ''italic text''
  *  -  '''bold text'''
  *  -  __underline__
  *  -  ^superscript^
  *  -  ,,subscript,,
  *
  * {{{
  * // Code block
  * }}}
  *
  * ==Bullet/ unordered list==
  *
  *  - Apple
  *  - Banana
  *    - Big Mike
  *    - Cavendish
  *  - Carrot
  *
  * ==Ordered list==
  *
  *  1. One
  *  1. Two
  *    i. Aie
  *    i. Aiyaie
  *    i. Aiyaieyai
  *  1. Three
  *    a. eh?
  *
  * =Tags=
  * @todo [[https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#tags]]
  */
class ScalaDocExample {

  /** Find the next integer.
    *
    * @param i the starting point
    * @return the result of adding one to `i`
    */
  def increment(i: Int): Int = i + 1

}
