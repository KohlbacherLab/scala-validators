package de.ekut.tbi.validation



import java.util.UUID.randomUUID
import java.time.LocalDate

import org.scalatest.flatspec.AnyFlatSpec

import cats.data.Validated
import cats.instances.list._

import de.ekut.tbi.validation.dsl._



class Tests extends AnyFlatSpec
{

  import scala.language.implicitConversions

  implicit def validatedToBoolean[E,T](v: Validated[E,T]): Boolean = v.isValid


  val even = Validator((n: Int) => n%2 == 0, (n: Int) => s"$n is not even")


  "Validation DSL" must "work as expected" in {

    assert(Some(42) must be (defined))
    
    assert(Some(42) must contain (42))

    assert(Some(42) must (contain (42) or contain(43)))

    assert(None must be (undefined))

    assert(None must be (empty))


    val oneToTen = 1 to 10

    assert(oneToTen must contain (anyOf (2,20,200)))

    assert(oneToTen must contain (allOf (2,5,7)))

    assert(4 must be (in (oneToTen)))

    assert(4 must be (positive))

    assert(4 must be (equalTo (4)))

    assert(4 must be (4))
    
    assert(all(oneToTen.toList) must be (positive))
    
    assert(all(Range(0,10,2).toList) must be (even))
    
  }


  "Patient Validation" must "work as expected" in {

     val patient =
       Patient(randomUUID,Some(Gender.Other),Some(LocalDate.now.minusYears(42)),"Max Mustermensch")

     assert(validate(patient).isValid)



     val validation2 =
       validate(Patient(randomUUID,None,Some(LocalDate.now),""))

     assert(validation2.isInvalid)

  }


}
