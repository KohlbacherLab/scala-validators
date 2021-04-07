package de.ekut.tbi.validation.dsl


import scala.reflect.ClassTag

import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  Unconstrained,
  Validator,
  ValidatorBuilder
}


sealed trait IsInstanceValidator extends Validator[String,Any]
{
  type Type = IsInstanceValidator
}

object IsInstanceValidator
{
  private final case class Impl(v: Validator[String,Any]) extends IsInstanceValidator 
  {
    def apply(t: Any) = v(t) 
    def negated = Impl(v.negated)
  }

  private[dsl] def apply[T](ct: ClassTag[T]): IsInstanceValidator =
    Impl(
      Validator[String,Any](
        ct.unapply(_).isDefined
      )(
        t => s"$t is not an instance of ${ct.runtimeClass.getName}",
        t => s"$t is an instance of ${ct.runtimeClass.getName}"
      )
    )

}



sealed trait AWord
{
  def apply[T](implicit ct: ClassTag[T]): IsInstanceValidator =
    IsInstanceValidator(ct)
}


final object a extends AWord

