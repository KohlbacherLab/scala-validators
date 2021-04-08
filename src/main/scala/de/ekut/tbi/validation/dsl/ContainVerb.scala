package de.ekut.tbi.validation.dsl



import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanContain,
  Validator,
  ValidatorBuilder
}


sealed trait AnyOfWord[U]
{
  val values: Set[U]
}

object anyOf
{
  def apply[U](u1: U, u2: U, us: U*): AnyOfWord[U] = {
    new AnyOfWord[U]{ val values = (u1 +: u2 +: us).toSet }
  }
}


sealed trait AllOfWord[U]
{
  val values: Set[U]
}

object allOf
{
  def apply[U](u1: U, u2: U, us: U*): AllOfWord[U] = {
    new AllOfWord[U]{ val values = (u1 +: u2 +: us).toSet }
  }
}


sealed trait OnlyWord[U]
{
  val value: U
}

object only
{
  def apply[U](u: U): OnlyWord[U] = {
    new OnlyWord[U]{ val value = u }
  }
}


sealed trait ContainClause[U] extends ValidatorBuilder[String,({ type CanContainT[x] = CanContain[U,x]})#CanContainT]
{
  self =>

  type Type = ContainClause[U]

  def negated =
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]) = self.apply[T].negated
    }

  def or(other: => Type) =
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]) =
        t => self.apply[T].apply(t) orElse other.apply[T].apply(t) 
    }

  def and(other: Type) =
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]) =
        t => (self.apply[T].apply(t), other.apply[T].apply(t)).mapN((_,_) => t) 
    }

}

sealed trait ContainVerb
{

  def apply[U](u: U): ContainClause[U] = {
    new ContainClause[U]{
      self =>
      def apply[T](implicit cc: CanContain[U,T]) =
        Validator[String,T](
          cc.contains(_)(u)
        )(
          t => s"$t does not contain $u",
          t => s"$t contains $u"
        )
    }
  }


  def apply[U](anyOf: AnyOfWord[U]): ContainClause[U] = {
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator[String,T](
          cc.containsAnyOf(_)(anyOf.values)
        )(
          t => s"$t does not contain any element of ${anyOf.values}",
          t => s"$t contains some element of ${anyOf.values}"
        )
    }
  }


  def apply[U](allOf: AllOfWord[U]): ContainClause[U] = {
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator[String,T](
          cc.containsAllOf(_)(allOf.values)
        )(
          t => s"$t does not contain all elements of ${allOf.values}",
          t => s"$t contains all elements of ${allOf.values}"
        )
    }
  }

  def apply[U](only: OnlyWord[U]): ContainClause[U] = {
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator[String,T](
          cc.containsOnly(_)(only.value)
        )(
          t => s"$t does not contain only ${only.value}",
          t => s"$t contains only ${only.value}"
        )
    }
  }


}

final object contain extends ContainVerb
