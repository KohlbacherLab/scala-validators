package de.ekut.tbi.validation



import java.util.UUID.randomUUID
import java.time._

import scala.util.Try

import org.scalatest.flatspec.AnyFlatSpec

import cats.data.Validated
import cats.instances.list._
import cats.instances.option._

import de.ekut.tbi.validation.dsl._



class Tests extends AnyFlatSpec
{

  import scala.language.implicitConversions


  private def assertValid[E,A](v: Validated[E,A]) = assert(v.isValid)

  private def assertInvalid[E,A](v: Validated[E,A]) = assert(v.isInvalid)


  val even = Validator[String,Int](n => n%2 == 0)(n => s"$n is not even", n => s"$n is even")

  val odd = not (even)



  "Option validations" must "work as expected" in {

    assertValid(Option(42) must be (a [Some[_]]))

    assertValid(Option(42) must not (be (a [None.type])))

    assertValid(Some(42) must be (defined))
    
    assertValid(Some(42) must contain (42))

    assertValid(Some(42) must (contain (42) or contain(43)))

    assertValid(Option(42) must (be (defined) and contain(42)))

    assertValid(None must be (undefined))

    assertValid(None must be (empty))


  }

  "Try validations" must "work as expected" in {

    assertValid(Try(42) must be (success))
  }


  "Range validations" must "work as expected" in {

    val oneToTen = 1 to 10

    assertValid(oneToTen must have (size (10)))

    assertValid(oneToTen must not (contain (11)))

    assertValid(11 must not (be (in (oneToTen))))

    assertValid(oneToTen must contain (anyOf (2,20,200)))

    assertValid(oneToTen must contain (allOf (2,5,7)))

    assertValid(oneToTen must (contain (allOf (2,5,7)) and not (contain (anyOf(20,21,22)))))

    assertValid(oneToTen must not (contain (8) and (contain (11))))

    assertValid(oneToTen must (contain (11) or (have (size (lessThan(11))))))
    assertValid(oneToTen must (have (size (lessThan(11))) or (contain (11))))

    assertValid(oneToTen must not (contain (11) or (contain (12))))

    assert(
      (oneToTen must not (contain (8) and (contain (11)))) ==
      (oneToTen must (not (contain (8)) or (not (contain (11)))))
    )

    assertValid(4 must be (in (oneToTen)))

    assertValid(all(oneToTen.toList) must be (positive))
    
    assertValid(all(oneToTen.toList) must not (be (negative)))

    assertValid(all(Range(0,10,2).toList) must be (even))
    assertValid(all(Range(0,10,2).toList) must not (be (odd)))
    
    assertValid(all(Range(1,11,2).toList) must be (odd))
    assertValid(all(Range(1,11,2).toList) must not (be (even)))

  }


  "Numeric validations" must "work as expected" in {

    assertValid(4 must be (positive))

    assertValid(-4 must not (be (positive)))

    assertValid(-4 must (be (positive) or (be (even))))

    assertValid(3 must (be (negative) or (be (positive) and be (odd))))

    assertValid(3 must (be (even) or (be (positive))))

    assertValid(-3 must not (be (even) or (be (positive))))

    assertValid(4 must (be (positive) and (be (even))))


    assertValid(4 must be (equalTo (4)))

    assertValid(4 must be (4))
    

  }
  

  "DateTime validations" must "work as expected" in {

    assertValid(LocalDate.now.minusDays(1) must be (before (LocalDate.now)))
    
    assertValid(LocalDate.now.plusDays(1) must be (after (LocalDate.now)))

  }


  "String validations" must "work as expected" in {

    val testString = "Test String"


    assertValid(all(List(testString)) must have (length (11)))

    assertValid(all(Option(testString)) must contain ('e'))

    assertValid(all(List(testString)) must contain (anyOf('a','e','i','o','u')))

    assertValid(all(List(testString)) must contain (allOf('e','i')))

    assertValid(all(List(testString)) must contain ("est"))

    assertValid(Subject("foo") must not (be (in (List(testString)))))

  }



  "Patient Validation" must "work as expected" in {

     val patient =
       Patient(randomUUID,Some(Gender.Other),Some(LocalDate.now.minusYears(42)),"Max Mustermensch")

     assertValid(patient must be (valid))

     assertValid(Patient(randomUUID,None,Some(LocalDate.now),"") must not (be (valid)))

     val patients = List(patient,patient,patient)

     assertValid(all(patients) must be (valid))

  }


}
