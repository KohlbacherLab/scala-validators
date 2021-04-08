package de.ekut.tbi.validation



import java.util.UUID.randomUUID
import java.time._

import org.scalatest.flatspec.AnyFlatSpec

import cats.data.Validated
import cats.instances.list._

import de.ekut.tbi.validation.dsl._



class Tests extends AnyFlatSpec
{

  import scala.language.implicitConversions

  implicit def validatedToBoolean[E,T](v: Validated[E,T]): Boolean = v.isValid


  val even = Validator[String,Int](n => n%2 == 0)(n => s"$n is not even", n => s"$n is even")

  val odd = not (even)


  "Validation DSL" must "work as expected" in {

    assert(Option(42) must be (a [Some[Int]]))

    assert(Option(42) must not (be (a [None.type])))

    assert(Some(42) must be (defined))
    
    assert(Some(42) must contain (42))

    assert(Some(42) must (contain (42) or contain(43)))

    assert(None must be (undefined))

    assert(None must be (empty))


    val oneToTen = 1 to 10

    assert(oneToTen must contain (anyOf (2,20,200)))

    assert(oneToTen must contain (allOf (2,5,7)))

    assert(oneToTen must have (size (10)))

    assert(4 must be (in (oneToTen)))

    assert(4 must be (positive))

    assert(-4 must not (be (positive)))

    assert(4 must be (equalTo (4)))

    assert(4 must be (4))
    
    assert(all(oneToTen.toList) must be (positive))
    
    assert(all(oneToTen.toList) must not (be (negative)))

    assert(all(Range(0,10,2).toList) must be (even))
    assert(all(Range(0,10,2).toList) must not (be (odd)))
    
    assert(all(Range(1,11,2).toList) must be (odd))
    assert(all(Range(1,11,2).toList) must not (be (even)))

  }


  "DateTime validations" must "work as expected" in {

    assert(LocalDate.now.minusDays(1) must be (before (LocalDate.now)))
    
    assert(LocalDate.now.plusDays(1) must be (after (LocalDate.now)))

  }


  "String validations" must "work as expected" in {

    val testString = "Test String"


    assert(all(List(testString)) must have (length (11)))

    assert(all(List(testString)) must contain ('e'))

    assert(all(List(testString)) must contain (anyOf('a','e','i','o','u')))

    assert(all(List(testString)) must contain (allOf('e','i')))

    assert(all(List(testString)) must contain ("est"))
  }



  "Patient Validation" must "work as expected" in {

     val patient =
       Patient(randomUUID,Some(Gender.Other),Some(LocalDate.now.minusYears(42)),"Max Mustermensch")

     assert(patient must be (valid))


     val validation2 =
       validate(Patient(randomUUID,None,Some(LocalDate.now),""))

     assert(validation2.isInvalid)

     assert(Patient(randomUUID,None,Some(LocalDate.now),"") must not (be (valid)))

     val patients = List(patient,patient,patient)

     assert(all(patients) must be (valid))

     assert(all(patients) must not (be (invalid)))


  }


}
