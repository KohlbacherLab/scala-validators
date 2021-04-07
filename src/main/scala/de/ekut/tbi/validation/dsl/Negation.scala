package de.ekut.tbi.validation.dsl


import de.ekut.tbi.validation.{
  Validator,
  ValidatorBuilder
}


sealed trait Negation
{
/*
  def apply[E,T,V](v: V)(implicit isv: V <:< Validator[E,T]) = v.negated
//  def apply[E,T, V <: Validator[E,T]](v: V): V#Sub = v.negated

  def apply[E,Constraint[_]](vb: ValidatorBuilder[E,Constraint]): ValidatorBuilder[E,Constraint] =
    vb.negated
*/

  def apply[E,T](v: Validator[E,T]): Validator[E,T] =
    v.negated

  def apply[E,Constraint[_]](vb: ValidatorBuilder[E,Constraint]): ValidatorBuilder[E,Constraint] =
    vb.negated

}

final object not extends Negation



