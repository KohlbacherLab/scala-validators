package de.ekut.tbi.validation


import cats.data.ValidatedNel


trait ValidatorBuilder[+E,C[_]]
{
  self =>

  type Constraint[x] = C[x]

  type Type <: ValidatorBuilder[E,C]


  def apply[T: Constraint]: Validator[E,T]

  def or(other: => Type): Type

  def and(other: Type): Type

  def negated: Type

}
