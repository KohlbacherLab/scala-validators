package de.ekut.tbi.validation.dsl


import de.ekut.tbi.validation.{
  Validator,
  ValidatorBuilder
}


sealed trait Negation
{

  def apply[E,T](validator: Validator[E,T]): validator.Type = validator.negated


  def apply[E,Constraint[_]](vb: ValidatorBuilder[E,Constraint]): vb.Type =
    vb.negated


  /*
  def apply[E,T](v: Validator[E,T]): Validator[E,T] =
    v.negated

  def apply[E,Constraint[_]](vb: ValidatorBuilder[E,Constraint]): ValidatorBuilder[E,Constraint] =
    vb.negated
*/

}

final object not extends Negation



