package de.ekut.tbi.validation



import cats.data.{
  NonEmptyList,
  ValidatedNel
}
import cats.Traverse
import cats.syntax.traverse._
import cats.syntax.apply._
import cats.syntax.either._
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


  implicit def toSubject[T](t: T): Subject[T] = new Subject(t)


  def all[T,C[T]: Traverse](ts: C[T]): TraversableSubject[T,C] = new TraversableSubject(ts)

  def all[T](t1: T, t2: T, ts: T*): TraversableSubject[T,List] = all((t1 +: t2 +: ts).toList)



  final val undefined = not (defined)

  final val nonEmpty = not (empty)

/*
  import cats.data.Validated._

  implicit class OptionOps[T](val opt: Option[T]) extends AnyVal
  {
    def value: ValidatedNel[String,T] =
      opt.fold(invalidNel[String,T]("No value on undefined Option"))(validNel[String,T](_))

    def valueOr[E](e: => E): ValidatedNel[E,T] =
      opt.fold(invalidNel[String,E](e))(validNel[E,T](_))
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



/*
  sealed trait VBJunction[+E,LC[_],RC[_]] extends ValidatorBuilder[E,Both[LC,RC]#Of]{
    def apply[T: Constraint]: Validator[E,T]
  }
*/

  type Given[U] = { type Equals[T] = T =:= U }

  type And[LC[_],RC[_]] = { type Both[T] = (LC[T],RC[T]) }


  type VBJunction[+E,LC[_],RC[_]] = ValidatorBuilder[E, (LC And RC)#Both]

  implicit def apply[T,LC[_],RC[_]](implicit lc: LC[T], rc: RC[T]): (LC[T],RC[T]) = (lc,rc)


  implicit class ValidatorBuilderLogicOps[E,LC[_]](val left: ValidatorBuilder[E,LC]) extends AnyVal
  {

    def or[EE >: E, RC[_]](right: => ValidatorBuilder[EE,RC]): VBJunction[EE,LC,RC] =
      new VBJunction[EE,LC,RC]{
        def apply[T](implicit c: Constraint[T]): Validator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            left.apply[T].apply(t) orElse right.apply[T].apply(t)
        }
      }

    def or[EE >: E, U](right: Validator[EE,U]): VBJunction[EE,LC,Given[U]#Equals] =
      new VBJunction[EE,LC,Given[U]#Equals]{
        def apply[T](implicit c: Constraint[T]): Validator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            left.apply[T].apply(t) orElse right(t).asInstanceOf[ValidatedNel[EE,T]]
        }
      }

    def and[EE >: E, RC[_]](right: ValidatorBuilder[EE,RC]): VBJunction[EE,LC,RC] =
      new VBJunction[EE,LC,RC]{
        def apply[T](implicit c: Constraint[T]): Validator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            (left.apply[T].apply(t),right.apply[T].apply(t)).mapN((_,_) => t)
        }
      }

    def and[EE >: E, U](right: Validator[EE,U]): VBJunction[EE,LC,Given[U]#Equals] =
      new VBJunction[EE,LC,Given[U]#Equals]{
        def apply[T](implicit c: Constraint[T]): Validator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            (left.apply[T].apply(t),right(t).asInstanceOf[ValidatedNel[EE,T]]).mapN((_,_) => t)
        }
      }

/*
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
*/
  }

  
  implicit class ValidatorLogicOps[E,T](val left: Validator[E,T]) extends AnyVal
  {

    def or[EE >: E, RC[_]](right: => ValidatorBuilder[EE,RC])(implicit rc: RC[T]): Validator[EE,T] =
      t => left(t) orElse right.apply[T].apply(t)


    def and[EE >: E, RC[_]](right: ValidatorBuilder[EE,RC])(implicit rc: RC[T]): Validator[EE,T] =
      t => (left(t), right.apply[T].apply(t)).mapN((_,_) => t)


    def or[EE >: E](right: => Validator[EE,T]): Validator[EE,T] =
      t => left(t) orElse right(t)


    def and[EE >: E](right: Validator[EE,T]): Validator[EE,T] =
      t => (left(t), right(t)).mapN((_,_) => t)

  }



  implicit class ValidatedTuple2Ops[Err,A,B](
    val vs: Tuple2[ValidatedNel[Err,A],ValidatedNel[Err,B]]
  ) extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple3Ops[Err,A,B,C](
    val vs: Tuple3[ValidatedNel[Err,A],ValidatedNel[Err,B],ValidatedNel[Err,C]]
  ) extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple4Ops[Err,A,B,C,D](
    val vs: Tuple4[ValidatedNel[Err,A],ValidatedNel[Err,B],ValidatedNel[Err,C],ValidatedNel[Err,D]]
  ) extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple5Ops[Err,A,B,C,D,E](
    val vs: Tuple5[ValidatedNel[Err,A],ValidatedNel[Err,B],ValidatedNel[Err,C],ValidatedNel[Err,D],ValidatedNel[Err,E]]
  ) extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }





}
