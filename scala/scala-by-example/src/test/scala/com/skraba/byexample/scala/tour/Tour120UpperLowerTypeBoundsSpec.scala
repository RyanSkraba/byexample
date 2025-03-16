package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour120UpperLowerTypeBoundsSpec extends AnyFunSpecLike with Matchers {

  describe("Upper type bounds") {
    it("is simple") {
      // An example hierarchy of Animals.
      abstract class Animal {
        def name: String
      }

      abstract class Pet extends Animal {}

      class Cat extends Pet {
        override def name: String = "Cat"
      }

      class Dog extends Pet {
        override def name: String = "Dog"
      }

      class Lion extends Animal {
        override def name: String = "Lion"
      }

      // Upper type bounds, we can be guaranteed that this container will only have Pets.
      class PetContainer[P <: Pet](p: P) {
        def pet: P = p
      }

      val dogContainer = new PetContainer[Dog](new Dog)
      dogContainer.pet shouldBe a[Dog]
      dogContainer.pet.name shouldBe "Dog"
      val catContainer = new PetContainer[Cat](new Cat)
      catContainer.pet.name shouldBe "Cat"
      catContainer.pet shouldBe a[Cat]
      // This is unacceptable, a Lion is not a Pet
      // val lionContainer = new PetContainer[Lion](new Lion)
    }
  }

  describe("Lower type bounds") {

    it("are required to respect a covariant contract") {
      // "This doesn't work because functions are contravariant in their parameter
      // types and covariant in their result types."

      // In other words: Type parameter A is covariant.
      // Therefore, if Dog is an Animal any List[Dog] is a List[Animal]
      // If we had a class DogList that extends List[Dog] with custom prepend behaviour that relied on
      // the B type parameter being a Dog, it would also be a List[Animal].
      // But since the above prepend uses A, when treating it as a List[Animal], you *could* ask
      // it to prepend a Cat... which would break the covariant contract.
      """\
      trait List[+A] {
        def prepend(elem: A): NonEmptyList[A] = NonEmptyList(elem, this)
      }
      case class NonEmptyList[+A](head: A, tail: List[A]) extends List[A]
      object Nil extends List[Nothing]
      """ shouldNot compile
    }

    it("is so simple") {

      // This form below respects the covariant contract, because now the subtypes of List
      // specify that prepend must be of a type it can handle (anything below B in the contract.
      object Working {
        trait List[+A] {
          def prepend[B >: A](elem: B): NonEmptyList[B] =
            NonEmptyList(elem, this)
        }

        case class NonEmptyList[+A](head: A, tail: List[A]) extends List[A]

        object Nil extends List[Nothing]

        // Using the above abstract data type with classes.
        trait Bird

        case class AfricanSwallow() extends Bird

        case class EuropeanSwallow() extends Bird
      }
      import Working._

      val africanSwallows: List[AfricanSwallow] = Nil.prepend(AfricanSwallow())
      val swallowsFromAntarctica: List[Bird] = Nil
      val someBird: Bird = EuropeanSwallow()

      // The List is covariant
      val birds: List[Bird] = africanSwallows

      // The list can change it's type depending what is put into it, while respecting the covariant contract.
      africanSwallows.prepend(AfricanSwallow()) shouldBe a[List[AfricanSwallow]]
      africanSwallows.prepend(EuropeanSwallow()) shouldBe a[List[Bird]]

      // This is a mistake! We now have a List(bird, bird, List) where the type is Object or Any, which is probably not what we want.
      val error = birds.prepend(EuropeanSwallow()).prepend(swallowsFromAntarctica)
    }
  }
}
