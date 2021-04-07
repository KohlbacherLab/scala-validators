package de.ekut.tbi.validation


import cats.data.ValidatedNel

import cats.syntax.apply._
import cats.instances.list._


trait ValidatorBuilder[E,C[_]]
{
  self =>

  type Constraint[x] = C[x]

  type Type <: ValidatorBuilder[E,C]


  def apply[T: Constraint]: Validator[E,T]


/*
  def and(other: ValidatorBuilder[E,C]): ValidatorBuilder[E,C] =
    new ValidatorBuilder[E,C]{
      def apply[T: Constraint]: Validator[E,T] =
        Validator(
          t => (self.apply[T].apply(t),other.apply[T].apply(t)).mapN((_,_) => t)
        )
    }


  def or(other: => ValidatorBuilder[E,C]): ValidatorBuilder[E,C] =
    new ValidatorBuilder[E,C]{
      def apply[T: Constraint]: Validator[E,T] =
        Validator(t => self.apply[T].apply(t) orElse (other.apply[T].apply(t)))
    }
*/

  def negated: Type

}
