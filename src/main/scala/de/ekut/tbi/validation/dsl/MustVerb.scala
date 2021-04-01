package de.ekut.tbi.validation.dsl


import cats.data.ValidatedNel
import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  ValidatorBuilder
}


sealed trait MustVerb[T]

/*
final class MustVerb[T](val t: T) extends AnyVal
{

  def apply[Constraint[_]](
    be: BeClause[Constraint]
  )(
    implicit constraint: Constraint[T]
  ): ValidatedNel[String,T] = {
    be.apply[T].apply(t)
  }


  def apply[Constraint[_]](
    validatorBuilder: ValidatorBuilder[Constraint]
  )(
    implicit constraint: Constraint[T]
  ): ValidatedNel[String,T] = {

    validatorBuilder.apply[T].apply(t)

  }

}
*/



