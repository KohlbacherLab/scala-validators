package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder
}



sealed trait DefinedWord extends ValidatorBuilder[String,CanBeDefined]
{
  type Type = DefinedWord

}

final case object defined extends DefinedWord
{
  def apply[T](implicit cbd: CanBeDefined[T]): Validator[String,T] =
    Validator[String,T](
      cbd.isDefined(_)
    )(
      t => s"$t is not defined",
      t => s"$t is defined"
    )
  def negated = undefined
}


final case object undefined extends DefinedWord
{
  def apply[T](implicit cbd: CanBeDefined[T]): Validator[String,T] =
    defined.apply[T].negated
  
  def negated = defined
}




