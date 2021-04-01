package de.ekut.tbi.validation



import cats.data.{
  NonEmptyList,
  ValidatedNel
}
import cats.Traverse
import cats.syntax.traverse._

import de.ekut.tbi.validation.Validator


package object dsl
{


  def validate[E,T](t: T)(implicit validator: Validator[E,T]) = validator(t)

  def validateEach[T,E,C[T]: Traverse](ts: C[T])(implicit v: Validator[E,T]) =
    ts.traverse(v)



  implicit class ValidationSyntax[T](val t: T) extends AnyVal
  {

    def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]) =
      clause.apply[T].apply(t)


    def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]) =
      beClause.apply[T].apply(t)


    def must[E](be: BeValidator[E,T]) = be(t)


    def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
      containClause.apply[T].apply(t)

    def validate[E](implicit validator: Validator[E,T]) = validator(t)

  }


  implicit class TraversableOps[T, C[T]: Traverse](val ts: C[T])
  {
    def validateEach[E](implicit v: Validator[E,T]) = ts.traverse(v)
  }



  implicit class ValidatedOps[E,T](val v: ValidatedNel[E,T]) extends AnyVal
  {
    def otherwise[EE](err: EE) =
      v.leftMap(_ => NonEmptyList.one(err))
  }





}
