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
    Validator[String,T](
      cbe.isEmpty(_)
    )(
      t => s"$t is not empty",
      t => s"$t is empty"
    )
//    Validator(
//      t => condNel(cbe.isEmpty(t), t , s"$t is not empty")
//    )
}


final case object nonEmpty extends EmptyWord
{
  def apply[T](implicit cbe: CanBeEmpty[T]): Validator[String,T] =
    empty.apply[T].negated
//    Validator(
//      t => condNel(!cbe.isEmpty(t), t , s"$t is empty")
//    )
}

