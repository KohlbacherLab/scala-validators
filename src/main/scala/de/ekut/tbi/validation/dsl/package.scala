package de.ekut.tbi.validation



import cats.data.{
  NonEmptyList,
  Validated,
  ValidatedNel
}
import cats.Traverse
import cats.syntax.traverse._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.validated._
import cats.instances.list._

import de.ekut.tbi.validation.{
  Validator,
  ValidatorBuilder
}


package object dsl
{

  import scala.language.implicitConversions


  def validate[E,T](t: T)(implicit validator: Validator[E,T]) =
    validator(t)

  def validateOpt[E,T](t: Option[T])(implicit validator: Validator[E,T]) =
    option(t) must validator

  def validateEach[T,E,C[T]: Traverse](ts: C[T])(implicit v: Validator[E,T]) =
    ts.traverse(v)

  
  implicit def toSubject[T](t: T): Subject[T] =
    new Subject(t)


  def all[T,C[T]: Traverse](ts: C[T]): TraversableSubject[T,C] =
    new TraversableSubject(ts)

  def all[T](t1: T, t2: T, ts: T*): TraversableSubject[T,List] =
    all((t1 +: t2 +: ts).toList)


  def valueIn[T](opt: Option[T]): OptionSubject[T] =
    OptionSubject(opt) 

  def option[T](opt: Option[T]): OptionSubject[T] =
    valueIn(opt)


  final val undefined =
    not (defined)

  final val nonEmpty =
    not (empty)


  def ifDefined[T,E](
    opt: Option[T]
  )(
    validator: Validator[E,T]
  ): ValidatedNel[E,Option[T]] =
    valueIn(opt) must validator


  implicit class TraversableOps[T, C[T]: Traverse](val ts: C[T])
  {
    def validateEach[E](implicit v: Validator[E,T]) =
      ts.traverse(v)
  }



  implicit class ValidatedOps[E,T](val v: ValidatedNel[E,T]) extends AnyVal
  {
    def otherwise[EE](err: => EE) =
      v.leftMap(_ => NonEmptyList.one(err))

    def `else`[EE](err: => EE) =
      v.leftMap(_ => NonEmptyList.one(err))

    def orError[EE](err: => EE) = otherwise(err)
  }



  type Given[U] = { type Equals[T] = T =:= U }

  type And[LC[_],RC[_]] = { type Both[T] = (LC[T],RC[T]) }


  type VBJunction[+E,LC[_],RC[_]] = ValidatorBuilder[E, (LC And RC)#Both]

  type NegatableVBJunction[+E,LC[_],RC[_]] = NegatableValidatorBuilder[E, (LC And RC)#Both]


  implicit def apply[T,LC[_],RC[_]](implicit lc: LC[T], rc: RC[T]): (LC[T],RC[T]) = (lc,rc)


  implicit class NegatableValidatorBuilderLogicOps[E, LC[_]](
    val left: NegatableValidatorBuilder[E,LC]
  )(
    implicit error: String => E
  )
  {

    def or[EE >: E, RC[_]](right: => NegatableValidatorBuilder[EE,RC]): NegatableVBJunction[EE,LC,RC] =
      new NegatableVBJunction[EE,LC,RC]{ self =>

        type Type = NegatableVBJunction[EE,LC,RC]

        def apply[T](implicit c: Constraint[T]): NegatableValidator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            left.apply[T].apply(t) orElse right.apply[T].apply(t)
        }

        def negated =
          new NegatableVBJunction[EE,LC,RC]{

            type Type = NegatableVBJunction[EE,LC,RC]

            def apply[T](implicit c: Constraint[T]) = {
              t =>
                implicit val (lc,rc) = c

                Validated.condNel(
                  !(left.apply[T].apply(t).isValid) && !(right.apply[T].apply(t).isValid),
                  t,
                  error(s"$t must not have been valid")
                ) 
            }

            def negated = self
          }
      }

    def or[EE >: E, U](right: NegatableValidator[EE,U]): NegatableVBJunction[EE,LC,Given[U]#Equals] =
      new NegatableVBJunction[EE,LC,Given[U]#Equals]{ self =>

        type Type = NegatableVBJunction[EE,LC,Given[U]#Equals]

        def apply[T](implicit c: Constraint[T]): NegatableValidator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            left.apply[T].apply(t) orElse right(t).asInstanceOf[ValidatedNel[EE,T]]
        }

        def negated =
          new NegatableVBJunction[EE,LC,Given[U]#Equals]{

            type Type = NegatableVBJunction[EE,LC,Given[U]#Equals]

            def apply[T](implicit c: Constraint[T]) = {
              t =>
                implicit val (lc,rc) = c

                Validated.condNel(
                  !(left.apply[T].apply(t)).isValid && !(right(t).isValid),
                  t,
                  error(s"$t must not have been valid")
                ) 
            }

            def negated = self
          }

      }

    def and[EE >: E, RC[_]](right: NegatableValidatorBuilder[EE,RC]): NegatableVBJunction[EE,LC,RC] =
      new NegatableVBJunction[EE,LC,RC]{ self =>

        type Type = NegatableVBJunction[EE,LC,RC]

        def apply[T](implicit c: Constraint[T]): NegatableValidator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            (left.apply[T].apply(t),right.apply[T].apply(t)).mapN((_,_) => t)
        }

        def negated =
          new NegatableVBJunction[EE,LC,RC]{

            type Type = NegatableVBJunction[EE,LC,RC]

            def apply[T](implicit c: Constraint[T]) = {
              t =>
                implicit val (lc,rc) = c

                Validated.condNel(
                  !(left.apply[T].apply(t).isValid) || !(right.apply[T].apply(t).isValid),
                  t,
                  error(s"$t must not have been valid")
                ) 
            }

            def negated = self
          }
      }

    def and[EE >: E, U](right: NegatableValidator[EE,U]): NegatableVBJunction[EE,LC,Given[U]#Equals] =
      new NegatableVBJunction[EE,LC,Given[U]#Equals]{ self =>

        type Type = NegatableVBJunction[EE,LC,Given[U]#Equals]

        def apply[T](implicit c: Constraint[T]): NegatableValidator[EE,T] = {
          t =>
            implicit val (lc,rc) = c
            (left.apply[T].apply(t),right(t).asInstanceOf[ValidatedNel[EE,T]]).mapN((_,_) => t)
        }

        def negated =
          new NegatableVBJunction[EE,LC,Given[U]#Equals]{

            type Type = NegatableVBJunction[EE,LC,Given[U]#Equals]

            def apply[T](implicit c: Constraint[T]) = {
              t =>
                implicit val (lc,rc) = c

                Validated.condNel(
                  !(left.apply[T].apply(t).isValid) || !(right(t).isValid),
                  t,
                  error(s"$t must not have been valid")
                ) 
            }

            def negated = self
          }

      }

  }

  
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



  implicit class ValidatedTuple2Ops[Err](
    val vs: Tuple2[ValidatedNel[Err,_],ValidatedNel[Err,_]]
  ) extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple3Ops[Err](
    val vs: Tuple3[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple4Ops[Err](
    val vs: Tuple4[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
      ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple5Ops[Err](
    val vs: Tuple5[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }

  implicit class ValidatedTuple6Ops[Err](
    val vs: Tuple6[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }

  implicit class ValidatedTuple7Ops[Err](
    val vs: Tuple7[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
      ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }

  implicit class ValidatedTuple8Ops[Err](
    val vs: Tuple8[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple9Ops[Err](
    val vs: Tuple9[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }

  implicit class ValidatedTuple10Ops[Err](
    val vs: Tuple10[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple11Ops[Err](
    val vs: Tuple11[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }


  implicit class ValidatedTuple12Ops[Err](
    val vs: Tuple12[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }

  implicit class ValidatedTuple13Ops[Err](
    val vs: Tuple13[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }

  implicit class ValidatedTuple14Ops[Err](
    val vs: Tuple14[
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_],
      ValidatedNel[Err,_]
    ]
  )
  extends AnyVal
  {
    def errorsOr[T](t: => T): ValidatedNel[Err,T] =
      vs.mapN { case _: Product => t }
  }



}
