package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeEmpty,
  Validator,
  ValidatorBuilder
}



sealed trait EmptyWord extends ValidatorBuilder[String,CanBeEmpty]
{
  self =>

  type Type = EmptyWord

  def apply[T](implicit cbe: CanBeEmpty[T]): Validator[String,T] =
    Validator[String,T](
      cbe.isEmpty(_)
    )(
      t => s"$t is not empty",
      t => s"$t is empty"
    )

  def negated = 
    new EmptyWord {
      override def apply[T: Constraint]: Validator[String,T] = self.apply[T].negated
      override def negated = self
    }

  def or(other: => Type) =
    new EmptyWord {
      override def apply[T: Constraint] =
        t => self.apply[T].apply(t) orElse other.apply[T].apply(t)
    }
    

}

final case object empty extends EmptyWord


/*
final case object empty extends EmptyWord
{
  def apply[T](implicit cbe: CanBeEmpty[T]): Validator[String,T] =
    Validator[String,T](
      cbe.isEmpty(_)
    )(
      t => s"$t is not empty",
      t => s"$t is empty"
    )

  def negated = nonEmpty
}


final case object nonEmpty extends EmptyWord
{
  def apply[T](implicit cbe: CanBeEmpty[T]): Validator[String,T] =
    empty.apply[T].negated

  def negated = empty
}
*/
