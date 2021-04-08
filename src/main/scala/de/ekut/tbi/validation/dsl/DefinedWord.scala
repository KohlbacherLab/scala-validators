package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder
}



sealed trait DefinedWord extends ValidatorBuilder[String,CanBeDefined]
{
  self =>

  type Type = DefinedWord

  def apply[T](implicit cbd: CanBeDefined[T]): Validator[String,T] =
    Validator[String,T](
      cbd.isDefined(_)
    )(
      t => s"$t is not defined",
      t => s"$t is defined"
    )

  def negated = 
    new DefinedWord {
      override def apply[T: Constraint]: Validator[String,T] = self.apply[T].negated
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

/*
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
*/



