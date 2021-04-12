package de.ekut.tbi.validation



import java.time.LocalDate
import java.util.UUID

import cats.syntax.apply._
import cats.instances.list._



object Gender extends Enumeration
{
  val Male, Female, Other, Unknown = Value
}


final case class Patient
(
  id: UUID,
  gender: Option[Gender.Value],
  birthDate: Option[LocalDate],
  name: String 
)


final case class Issue(message: String)


object Patient
{

  import de.ekut.tbi.validation.dsl._


  implicit val validator: Validator[Issue,Patient] =
    patient =>
      (
        patient.gender must be (defined) otherwise (
          Issue("gender not defined")
        ),
        patient.birthDate must be (defined) otherwise (
          Issue("birthDate not defined")
        ) andThen (
          _.get must be (before (LocalDate.now)) otherwise (Issue("Invalid birthDate in the future"))
        ),
        patient.name must not (be (empty)) otherwise (
          Issue("Empty name")
        ),
      )
      .mapN { case _: Product => patient }
    

/*
  implicit val validator: Validator[String,Patient] =
    patient =>
      (
        patient.gender must be (defined) otherwise ("gender not defined"),
        patient.birthDate must be (defined) andThen (_.get must be (before (LocalDate.now))),
        patient.name must not (be (empty)),
      )
      .mapN { case _: Product => patient }
    
  implicit val validator: Validator[String,Patient] =
    {
      case pat @ Patient(id,gender,birthDate,name) =>
        (
          gender must be (defined) otherwise ("gender not defined"),
          birthDate must be (defined) andThen (_.get must be (before (LocalDate.now))),
          name must not (be (empty)),
        )
        .mapN { case _: Product => pat }
    }
*/

}
