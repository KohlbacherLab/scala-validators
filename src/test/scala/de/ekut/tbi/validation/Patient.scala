package de.ekut.tbi.validation



import java.time.LocalDate
import java.util.UUID



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


  implicit val issueBuilder: String => Issue =
    Issue(_)


  implicit val validator: NegatableValidator[Issue,Patient] =
    patient =>
      (
        patient.gender must be (defined) otherwise (Issue("gender not defined")),
        patient.birthDate must be (defined) otherwise (Issue("birthDate not defined")) andThen (
          _.get must be (before (LocalDate.now)) otherwise (Issue("Invalid birthDate in the future"))
        ),
        patient.name must not (be (empty)) otherwise (Issue("Empty name")),
      )
      .errorsOr(patient)


/*
  implicit val validator: NegatableValidator[Issue,Patient] =
    NegatableValidator.from(
      (patient: Patient) =>
        (
          patient.gender must be (defined) otherwise (Issue("gender not defined")),
          patient.birthDate must be (defined) otherwise (Issue("birthDate not defined")) andThen (
            _.get must be (before (LocalDate.now)) otherwise (Issue("Invalid birthDate in the future"))
          ),
          patient.name must not (be (empty)) otherwise (Issue("Empty name")),
        )
        .errorsOr(patient)
    )(
      Issue(_)
    )
*/
}
