package de.ekut.tbi.validation.dsl



import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanContain,
  Validator,
  ValidatorBuilder,
  NegatableValidator,
  NegatableValidatorBuilder
}


sealed trait InWord[Us] extends NegatableValidatorBuilder[String,({ type CanContainT[x] = CanContain[x,Us]})#CanContainT]
{
  self =>

  type Type = InWord[Us]

  def negated =
    new InWord[Us]{
      def apply[T: Constraint] = self.apply[T].negated
    }

  def or(other: => Type) =
    new InWord[Us]{
      def apply[T: Constraint] =
        t => self.apply[T].apply(t) orElse other.apply[T].apply(t)
    }

  def and(other: Type) =
    new InWord[Us]{
      def apply[T: Constraint] =
        t => (self.apply[T].apply(t), other.apply[T].apply(t)).mapN((_,_) => t)
    }

}

final object in
{

  def apply[Us](us: Us): InWord[Us] =
    new InWord[Us]{
      def apply[T](implicit cc: CanContain[T,Us]): NegatableValidator[String,T] =
        Validator[String,T](
          cc.contains(us)(_)
        )(
          t => s"$t is not contained in $us",
          t => s"$t is contained in $us"
        )
    }
  
}

