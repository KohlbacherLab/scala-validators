package de.ekut.tbi.validation.dsl


import de.ekut.tbi.validation.{
  NegatableValidator,
  NegatableValidatorBuilder
}


sealed trait Negation
{

  def apply[E,T](validator: NegatableValidator[E,T]): validator.Type = validator.negated


  def apply[E,Constraint[_]](vb: NegatableValidatorBuilder[E,Constraint]): vb.Type =
    vb.negated

}

final object not extends Negation


