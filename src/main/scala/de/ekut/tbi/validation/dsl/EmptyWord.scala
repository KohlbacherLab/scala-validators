package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanBeEmpty,
  Validator,
  ValidatorBuilder,
  NegatableValidator,
  NegatableValidatorBuilder
}



sealed trait EmptyWord extends NegatableValidatorBuilder[String,CanBeEmpty]
{
  self =>

  type Type = EmptyWord

  def apply[T](implicit cbe: CanBeEmpty[T]): NegatableValidator[String,T] =
    Validator[String,T](
      cbe(_)
    )(
      t => s"$t is not empty",
      t => s"$t is empty"
    )

  def negated = 
    new EmptyWord {
      override def apply[T: Constraint]: NegatableValidator[String,T] = self.apply[T].negated
      override def negated = self
    }

}

final case object empty extends EmptyWord

