package de.ekut.tbi.validation


import cats.data.ValidatedNel



trait ValidatorBuilder[E,C[_]]
{
  self =>

  type Constraint[x] = C[x]

  def apply[T: Constraint]: Validator[E,T]


  def and(other: ValidatorBuilder[E,C]): ValidatorBuilder[E,C] =
    new ValidatorBuilder[E,C]{
      def apply[T: Constraint]: Validator[E,T] =
        Validator(t => self.apply[T].apply(t) andThen (other.apply[T].apply(_)))
    }

  def or(other: => ValidatorBuilder[E,C]): ValidatorBuilder[E,C] =
    new ValidatorBuilder[E,C]{
      def apply[T: Constraint]: Validator[E,T] =
        Validator(t => self.apply[T].apply(t) orElse (other.apply[T].apply(t)))
    }

}


/*

trait ValidatorBuilder[-Constraint[_]]
{

  def apply[T: Constraint]: Validator[String,T]
}
*/

