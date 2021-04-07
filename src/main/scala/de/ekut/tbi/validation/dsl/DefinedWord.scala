package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder
}



sealed trait DefinedWord extends ValidatorBuilder[String,CanBeDefined]


final case object defined extends DefinedWord
{
  def apply[T](implicit cbd: CanBeDefined[T]): Validator[String,T] =
    Validator[String,T](
      cbd.isDefined(_)
    )(
      t => s"$t is not defined",
      t => s"$t is defined"
    )
//    Validator(
//      t => condNel(cbd.isDefined(t), t , s"$t is not defined")
//    )
}


final case object undefined extends DefinedWord
{
  def apply[T](implicit cbd: CanBeDefined[T]): Validator[String,T] =
    defined.apply[T].negated
//    Validator(
//      t => condNel(!cbd.isDefined(t), t , s"$t is defined")
//    )
}




