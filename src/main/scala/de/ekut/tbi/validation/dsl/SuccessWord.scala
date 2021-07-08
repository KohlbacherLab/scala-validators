package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanBeSuccess,
  Validator,
  ValidatorBuilder,
  NegatableValidator,
  NegatableValidatorBuilder
}



sealed trait SuccessWord extends NegatableValidatorBuilder[String,CanBeSuccess]
{
  self =>

  type Type = SuccessWord

  def apply[T](implicit cbd: CanBeSuccess[T]): NegatableValidator[String,T] =
    Validator[String,T](
      cbd.isSuccess(_)
    )(
      t => s"$t is not success",
      t => s"$t is success"
    )

  def negated = 
    new SuccessWord {
      override def apply[T: Constraint]: NegatableValidator[String,T] = self.apply[T].negated
      override def negated = self
    } 

}

final case object success extends SuccessWord

