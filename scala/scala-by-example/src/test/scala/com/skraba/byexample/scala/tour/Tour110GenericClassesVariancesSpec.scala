package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see
  *   https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour110GenericClassesVariancesSpec extends AnyFunSpecLike with Matchers {

  /** An example hierarchy of types. */
  abstract class Animal {
    def name: String
  }
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  describe("Generic classes") {

    it("can be simple") {

      class Stack[A] {
        private var elements: List[A] = Nil

        def push(x: A): Unit = {
          elements = x :: elements
        }

        def peek: A = elements.head

        def pop(): A = {
          val currentTop = peek
          elements = elements.tail
          currentTop
        }
      }

      // As a collection, it can can take subtypes without futzing with variance.
      val stack = new Stack[Int]
      stack.push(1)
      stack.push('a')
      stack.peek shouldBe 97
      stack.pop() shouldBe 'a'
      stack.pop() shouldBe 1

      val charStack = new Stack[Char]
      charStack.push('a')
      charStack.push(98)
      charStack.peek shouldBe 'b'
      charStack.pop() shouldBe 98
      charStack.pop() shouldBe 'a'

      // But a Stack[Char] can't be assigned to a Stack[Int], even though every element
      // in the Stack[Char] could go into that stack.
      "val check: Stack[Char] = stack" shouldNot typeCheck
      "val check: Stack[Int] = charStack" shouldNot typeCheck

      class Fruit
      class Apple extends Fruit
      class Banana extends Fruit

      val fruitStack = new Stack[Fruit]
      val apple = new Apple
      val banana = new Banana

      fruitStack.push(apple)
      fruitStack.push(banana)

      fruitStack.pop shouldBe a[Banana]
      fruitStack.pop shouldBe an[Apple]
      "val check: Stack[Fruit] = new Stack[Apple]" shouldNot typeCheck
      "val check: Stack[Apple] = fruitStack" shouldNot typeCheck
    }
  }

  describe("Variances") {
    // The Stack[A] above is *invariant*, which means that a Stack[Dog] is NOT a Stack[Animal].

    // This makes sense -- if a Stack[Apple] were a Stack[Fruit], then you could pass it around
    // and add a Banana to it later.

    class Foo[+A] // A covariant class
    class Bar[-A] // A contravariant class
    class Baz[A] // An invariant class

    it("such as covariance") {
      // Covariant like Foo[+A] implies that for two types X and Y
      // - where X is a subtype of Y,
      // - then Foo[X] is a subtype of Foo[Y]

      // Unlike Stack, "sealed abstract class List[+A]" has a covariant type parameter A
      val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
      val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))

      // So these are true
      "val check: List[Animal] = cats" should compile
      "val check: List[Animal] = dogs" should compile

      // And any List[subtype of Animal] can be passed to this method.
      def animalNamesLength(animals: List[Animal]): Int =
        animals.map(_.name.length).sum

      // This compiles because List[Cat] is a List[Animal], etc.
      animalNamesLength(cats) shouldBe 11
      animalNamesLength(dogs) shouldBe 7
    }

    it("such as contravariance") {
      // Contravariant like Bar[-A] implies that for two types X and Y
      // - where X is a subtype of Y,
      // - then Bar[Y] is a subtype of Bar[X]

      abstract class NameCalculator[-A] {
        def nameLength(value: A): Int
      }
      class AnimalNameCalculator extends NameCalculator[Animal] {
        def nameLength(animal: Animal): Int = animal.name.length
      }
      class CatNameCalculator extends NameCalculator[Cat] {
        // Takes into account that cats have three different names.
        def nameLength(animal: Cat): Int = animal.name.length * 3
      }

      // A method that requires the argument to be NameCalculator of a specific type.
      def catNameLength(calc: NameCalculator[Cat], cat: Cat): Int =
        calc.nameLength(cat)

      // This compiles because NameCalculator[Cat] is a NameCalculator[Animal].
      catNameLength(new AnimalNameCalculator, Cat("Whiskers")) shouldBe 8
      catNameLength(new CatNameCalculator, Cat("Whiskers")) shouldBe 24

      // In java collections, the rule for wildcard types is PECS:
      // a producer -> ? extends T, a consumer -> ? super T
      // If you are reading from a list, it should ? extend Animal so you are guaranteed to have am
      // Animal when you take an element, even if it was really a cat.
      // If you are writing to a list, it should ? super Animal so can put anything in it as long
      // as it's an Animal.

      // In Java, variance annotations are given by clients when a class abstraction is used
      // (use-site variance).

    }

    it("has examples like Function") {
      // trait Function1[-T, +R] from the Scala standard library with argument type and return type.
      // A Function1 is contravariant over its argument type, and covariant over its return type.

      val identity: Function[Animal, Animal] = a => a

      // This is acceptable because the argument type is contravariant.
      // Cat is an Animal, Therefore Function[Animal, _] is a Function[Cat, _]
      val catIdentity: Function[Cat, Animal] = identity
      catIdentity(Cat("Whiskers")).name shouldBe "Whiskers"

      // This is not acceptable, because Any is not a subtype of Animal.
      // val anyIdentity: Function[Any, Animal] = identity

      // This is acceptable because the return type is covariant.
      // Animal is an Any, Therefore Function[_, Animal] is a Function[_, Any]
      val anyIdentity: Function[Cat, Any] = identity
      catIdentity(Cat("Whiskers")) shouldBe a[Cat]

      // This is not acceptable, because Animal is not a subtype of Cat.
      "val check: Function[Animal, Cat] = identity" shouldNot typeCheck
    }
  }
}
