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

  
  import scala.language.implicitConversions


  implicit def toMustVerb[T](t: T): MustVerb[T] = new MustVerb(t)


  def all[T,C[T]: Traverse](ts: C[T]) = new MustVerbTraversable(ts)


  final val undefined = not (defined)

  final val nonEmpty = not (empty)

  final val invalid = not (valid)

/*
  implicit class ValidationOps[T](val t: T) extends AnyVal
  {

    def must = new MustVerb(t)

    def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]) =
      clause.apply[T].apply(t)


    def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]) =
      beClause.apply[T].apply(t)


    def must[E](be: BeValidator[E,T]) = be(t)


    def must(matchRegex: RegexValidator)(implicit str: T =:= String) = matchRegex(t)


    def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
      containClause.apply[T].apply(t)

    def validate[E](implicit validator: Validator[E,T]) = validator(t)

  }
*/


/*
  implicit class StringValidationOps(val s: String) extends AnyVal
  {

    def must(matchRegex: RegexValidator) = matchRegex(s)

  }
*/


  implicit class TraversableOps[T, C[T]: Traverse](val ts: C[T])
  {
    def validateEach[E](implicit v: Validator[E,T]) = ts.traverse(v)
  }



  implicit class ValidatedOps[E,T](val v: ValidatedNel[E,T]) extends AnyVal
  {
    def otherwise[EE](err: => EE) =
      v.leftMap(_ => NonEmptyList.one(err))

    def orError[EE](err: => EE) = otherwise(err)

  }





}
