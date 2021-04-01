package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeEmpty,
  Validator,
  ValidatorBuilder
}



sealed trait EmptyWord extends ValidatorBuilder[String,CanBeEmpty]

final case object empty extends EmptyWord
{
  def apply[T](implicit cbe: CanBeEmpty[T]): Validator[String,T] =
    Validator(
      t => condNel(cbe.isEmpty(t), t , s"$t is not empty")
    )
}


final case object nonEmpty extends EmptyWord
{
  def apply[T](implicit cbe: CanBeEmpty[T]): Validator[String,T] =
    Validator(
      t => condNel(!cbe.isEmpty(t), t , s"$t is empty")
    )
}

