package de.ekut.tbi.validation



import cats.data.{
  NonEmptyList,
  ValidatedNel
}
import cats.Traverse
import cats.syntax.traverse._
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  Validator,
  ValidatorBuilder
}


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



  type Given[U] = { type Equals[T] = T =:= U }


  sealed trait VBJunction[+E,LC[_],RC[_]]{
    def apply[T: LC: RC]: Validator[E,T]
  }


  implicit class ValidatorBuilderLogicOps[E,LC[_]](val left: ValidatorBuilder[E,LC]) extends AnyVal
  {

    def or[RC[_]](right: => ValidatorBuilder[E,RC]): VBJunction[E,LC,RC] =
      new VBJunction[E,LC,RC]{
        def apply[T: LC: RC]: Validator[E,T] =
          t => left.apply[T].apply(t) orElse right.apply[T].apply(t)
      }

    def or[U](right: Validator[E,U]): VBJunction[E,LC,Given[U]#Equals] =
      new VBJunction[E,LC,Given[U]#Equals]{
        def apply[T: LC: Given[U]#Equals]: Validator[E,T] =
          t => left.apply[T].apply(t) orElse right(t).asInstanceOf[ValidatedNel[E,T]]
      }

    def and[RC[_]](right: ValidatorBuilder[E,RC]): VBJunction[E,LC,RC] =
      new VBJunction[E,LC,RC]{
        def apply[T: LC: RC]: Validator[E,T] =
          t => (left.apply[T].apply(t),right.apply[T].apply(t)).mapN((_,_) => t)
      }

    def and[U](right: Validator[E,U]): VBJunction[E,LC,Given[U]#Equals] =
      new VBJunction[E,LC,Given[U]#Equals]{
        def apply[T: LC: Given[U]#Equals]: Validator[E,T] =
          t => (left.apply[T].apply(t),right(t).asInstanceOf[ValidatedNel[E,T]]).mapN((_,_) => t)
      }

  }

  
  implicit class ValidatorLogicOps[E,T](val left: Validator[E,T]) extends AnyVal
  {

    def or[EE >: E, RC[_]](right: => ValidatorBuilder[EE,RC])(implicit rc: RC[T]): Validator[EE,T] =
      t => left(t) orElse right.apply[T].apply(t)


    def and[EE >: E, RC[_]](right: ValidatorBuilder[EE,RC])(implicit rc: RC[T]): Validator[EE,T] =
      t => (left(t), right.apply[T].apply(t)).mapN((_,_) => t)


    def or(right: => Validator[E,T]): Validator[E,T] =
      t => left(t) orElse right(t)


    def and(right: Validator[E,T]): Validator[E,T] =
      t => (left(t), right(t)).mapN((_,_) => t)

  }


}
