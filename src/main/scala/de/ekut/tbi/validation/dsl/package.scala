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
