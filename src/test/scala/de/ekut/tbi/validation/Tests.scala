package de.ekut.tbi.validation



import java.util.UUID.randomUUID
import java.time.LocalDate

import org.scalatest.flatspec.AnyFlatSpec

import de.ekut.tbi.validation.dsl._



class Tests extends AnyFlatSpec
{

  "Validation DSL" must "work as expected" in {

    assert((Some(42) must be (defined)).isValid)
    
    assert((Some(42) must contain (42)).isValid)

    assert((Some(42) must (contain (42) or contain(43))).isValid)

    assert((None must be (undefined)).isValid)

    assert((None must be (empty)).isValid)


    val oneToTen = 1 to 10

    assert((oneToTen must contain (anyOf (2,20,200))).isValid)

    assert((oneToTen must contain (allOf (2,5,7))).isValid)

    assert((4 must be (in (oneToTen))).isValid)

    assert((4 must be (positive)).isValid)

    assert((4 must be (equalTo (4))).isValid)

    assert((4 must be (4)).isValid)
  }


  "Validation Ops" must "work as expected" in {

     import cats.instances.list._

     implicit val intValidator = positive[Int]


     assert(validate(42).isValid)


     assert((1 to 10).toList.validateEach.isValid)

  }


  "Patient Validation" must "work as expected" in {

     val patient =
       Patient(randomUUID,Some(Gender.Other),Some(LocalDate.now.minusYears(42)),"Max Mustermensch")

     assert(validate(patient).isValid)



     val validation2 =
       validate(Patient(randomUUID,None,Some(LocalDate.now),""))


     validation2.leftMap(_.toList.tapEach(println))

     assert(validation2.isInvalid)

  }


}
