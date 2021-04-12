package de.ekut.tbi.validation.dsl


import scala.reflect.ClassTag

import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  Unconstrained,
  Validator,
  ValidatorBuilder,
  NegatableValidator,
  NegatableValidatorBuilder
}



//sealed trait IsInstanceClause[U] extends ValidatorBuilder[String,Unconstrained]
sealed trait IsInstanceClause[U] extends NegatableValidatorBuilder[String,Unconstrained]
{
  self =>

  type Type = IsInstanceClause[U]

  def negated =
    new IsInstanceClause[U]{
      def apply[T: Constraint] = self.apply[T].negated
    }

  def or(other: => Type) =
    new IsInstanceClause[U]{
      def apply[T: Constraint] =
        t => self.apply[T].apply(t) orElse other.apply[T].apply(t)
    }

  def and(other: Type) =
    new IsInstanceClause[U]{
      def apply[T: Constraint] =
        t => (self.apply[T].apply(t),other.apply[T].apply(t)).mapN((_,_) => t)
    }
}


sealed trait AWord
{

  def apply[U](implicit ct: ClassTag[U]): IsInstanceClause[U] =
    new IsInstanceClause[U]{
      def apply[T: Unconstrained] =
        Validator[String,T](
          ct.unapply(_).isDefined
        )(
          t => s"$t is not an instance of ${ct.runtimeClass.getName}",
          t => s"$t is an instance of ${ct.runtimeClass.getName}"
        )
    }

}


final object a extends AWord


