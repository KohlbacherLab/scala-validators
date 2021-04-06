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


object Patient
{

  import de.ekut.tbi.validation.dsl._


  implicit val validator =
    Validator[String,Patient]{

      case pat @ Patient(id,gender,birthDate,name) =>
        (
          gender must be (defined),
          birthDate must be (defined) andThen (_.get must be (before (LocalDate.now))),
          name must be (nonEmpty),

        )
        .mapN { case _: Product => pat }

    }


}
