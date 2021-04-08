package de.ekut.tbi.validation


import cats.data.ValidatedNel
import cats.data.Validated.condNel

//import cats.syntax.apply._
//import cats.instances.list._


trait Validator[E,T] extends (T => ValidatedNel[E,T])
{
  self => 

  type Type <: Validator[E,T]

/*
  def and(other: Validator[E,T]): Validator[E,T] 


  def or(other: => Validator[E,T]): Validator[E,T] 
*/

  def negated: Type

}


object Validator
{

  final case class Impl[E,T]
  (
    f: T => Boolean,
    mustBeTrue: Boolean,
    error: T => E,
    errorWhenNegated: T => E
  )
  extends Validator[E,T]
  {

    type Type = Impl[E,T]

    override def apply(t: T): ValidatedNel[E,T] = {
      if (mustBeTrue)
        condNel(f(t), t, error(t))
      else 
        condNel(!f(t), t, errorWhenNegated(t))
    }

//    override def and(other: Validator[E,T]): Validator[E,T] = ??? 
    
//    override def or(other: => Validator[E,T]): Validator[E,T] = ???

    override def negated: Impl[E,T] = copy(mustBeTrue = !mustBeTrue)

  }


  import scala.language.implicitConversions
/*
  implicit def fromValidation[T](
    f: T => ValidatedNel[String,T]
  ): Validator[String,T] =
    new Validator[String,T]{ self =>

      type Type = Validator[String,T]

      def apply(t: T) = f(t)

      def negated =
        new Validator[String,T]{
          type Type = Validator[String,T]
          def apply(tt: T) =
            condNel(!f(tt).isValid, tt, s"$tt should not have been valid")

          def negated = self
        }

    }
*/

  implicit def fromValidationE[E,T](
    f: T => ValidatedNel[E,T]
  )(
    implicit toError: String => E
  ): Validator[E,T] =
    new Validator[E,T]{ self =>

      type Type = Validator[E,T]

      def apply(t: T) = f(t)

      def negated =
        new Validator[E,T]{

          type Type = Validator[E,T]

          def apply(tt: T) = condNel(!f(tt).isValid, tt, toError(s"$tt should not have been valid"))

          def negated = self
        }

    }


  def apply[T](
    f: T => Boolean
  ): Validator[String,T] =
    Impl(f,true,t => s"$t does not pass validation",t => s"$t passes validation although negated")
    

  def apply[T](
    f: T => Boolean
  )(
    error: T => String
  ): Validator[String,T] =
    Impl(f,true,error, t => error(t).replace(" not ",""))
    

  def apply[E,T](
    f: T => Boolean
  )(
    error: T => E,
    errorWhenNegated: T => E
  ): Validator[E,T] =
    Impl(f,true,error,errorWhenNegated)
  
  

}
