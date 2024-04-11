package de.ekut.tbi.validation


import cats.data.ValidatedNel
import cats.data.Validated.condNel


trait Validator[+E,T] extends (T => ValidatedNel[E,T])
{
  self =>

  def combineWith[EE >: E](other: Validator[EE,T]): Validator[EE,T] = {

    implicit val sg: cats.Semigroup[T] =
      cats.Semigroup.instance((t,_) => t)

    t => self(t) combine other(t)
  }
}

trait NegatableValidator[+E,T] extends Validator[E,T]
  with Negatable[NegatableValidator[E,T]]



object Validator
{
  def apply[E,T](implicit v: Validator[E,T]): Validator[E,T] = v

  private final case class Impl[E,T]
  (
    f: T => Boolean,
    mustBeTrue: Boolean,
    error: T => E,
    errorWhenNegated: T => E
  )
  extends NegatableValidator[E,T]
  {

    type Type = Impl[E,T]

    override def apply(t: T): ValidatedNel[E,T] = {
      if (mustBeTrue)
        condNel(f(t), t, error(t))
      else 
        condNel(!f(t), t, errorWhenNegated(t))
    }

    override def negated: Impl[E,T] = copy(mustBeTrue = !mustBeTrue)

  }


  def apply[T](
    f: T => Boolean
  ): NegatableValidator[String,T] =
    Impl(f,true,t => s"$t does not pass validation",t => s"$t passes validation although negated")
    

  def apply[T](
    f: T => Boolean
  )(
    error: T => String
  ): NegatableValidator[String,T] =
    Impl(f,true,error, t => error(t).replace(" not ",""))
    

  def apply[E,T](
    f: T => Boolean
  )(
    error: T => E,
    errorWhenNegated: T => E
  ): NegatableValidator[E,T] =
    Impl(f,true,error,errorWhenNegated)



  import scala.language.implicitConversions

  implicit def from[E,A](
    f: A => ValidatedNel[E,A]
  ): Validator[E,A] =
    new Validator[E,A]{ self =>

      type Type = Validator[E,A]

      def apply(a: A) = f(a)

    }

}


object NegatableValidator
{

  def apply[E,T](implicit v: NegatableValidator[E,T]): NegatableValidator[E,T] = v


  import scala.language.implicitConversions

  implicit def from[E,T](
    f: T => ValidatedNel[E,T]
  )(
    implicit toError: String => E
  ): NegatableValidator[E,T] =
    new NegatableValidator[E,T]{ self =>

      type Type = NegatableValidator[E,T]

      def apply(t: T) = f(t)

      def negated =
        new NegatableValidator[E,T]{

          type Type = NegatableValidator[E,T]

          def apply(tt: T) = condNel(!f(tt).isValid, tt, toError(s"$tt should not have been valid"))

          def negated = self
        }

    }

}
