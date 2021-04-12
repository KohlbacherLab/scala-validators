package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder,
  NegatableValidator,
  NegatableValidatorBuilder
}



sealed trait DefinedWord extends NegatableValidatorBuilder[String,CanBeDefined]
{
  self =>

  type Type = DefinedWord

  def apply[T](implicit cbd: CanBeDefined[T]): NegatableValidator[String,T] =
    Validator[String,T](
      cbd.isDefined(_)
    )(
      t => s"$t is not defined",
      t => s"$t is defined"
    )

  def negated = 
    new DefinedWord {
      override def apply[T: Constraint]: NegatableValidator[String,T] = self.apply[T].negated
      override def negated = self
    } 

  def or(other: => Type) =
    new DefinedWord {
      override def apply[T: Constraint] =
        t => self.apply[T].apply(t) orElse other.apply[T].apply(t)
    }

  def and(other: Type) =
    new DefinedWord {
      override def apply[T: Constraint] =
        t => (self.apply[T].apply(t), other.apply[T].apply(t)).mapN((_,_) => t)
    }

}

final case object defined extends DefinedWord

