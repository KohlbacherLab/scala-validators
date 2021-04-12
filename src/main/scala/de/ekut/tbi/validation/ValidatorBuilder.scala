package de.ekut.tbi.validation


import cats.data.ValidatedNel


trait ValidatorBuilder[+E,C[_]]
{

  type Constraint[x] = C[x]

  type Type <: ValidatorBuilder[E,C]

  def apply[T: Constraint]: Validator[E,T]

}


trait NegatableValidatorBuilder[+E,C[_]] extends ValidatorBuilder[E,C]
{

  type Type <: NegatableValidatorBuilder[E,C]

  def apply[T: Constraint]: NegatableValidator[E,T]

  def negated: Type

}

