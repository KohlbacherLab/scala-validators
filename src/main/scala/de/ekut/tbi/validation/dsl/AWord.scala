package de.ekut.tbi.validation.dsl


import scala.reflect.ClassTag

import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  Unconstrained,
  Validator,
  ValidatorBuilder
}



sealed trait IsInstanceClause[U] extends ValidatorBuilder[String,Unconstrained]
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


