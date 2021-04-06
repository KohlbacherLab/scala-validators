package de.ekut.tbi.validation.dsl



import cats.data.Validated.condNel

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


sealed trait ContainVerb
{

  def apply[U](u: U): ContainClause[U] = {
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator(
          cc.contains(_)(u), t => s"$t does not contain $u"
        )
    }
  }


  def apply[U](anyOf: AnyOfWord[U]): ContainClause[U] = {
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator(
          t => condNel(cc.containsAnyOf(t)(anyOf.values), t , s"$t does not contain any element of ${anyOf.values}")
        )
    }
  }


  def apply[U](allOf: AllOfWord[U]): ContainClause[U] = {
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator(
          t => condNel(cc.containsAllOf(t)(allOf.values), t , s"$t does not contain all element of ${allOf.values}")
        )
    }
  }

  def apply[U](only: OnlyWord[U]): ContainClause[U] = {
    new ContainClause[U]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator(
          t => condNel(cc.containsOnly(t)(only.value), t , s"$t does not contain only ${only.value}")
        )
    }
  }


/*

  type Given[U] = { type CanContainT[x] = CanContain[U,x]}

  def apply[U](u: U): ValidatorBuilder[Given[U]#CanContainT] = {
    new ValidatorBuilder[Given[U]#CanContainT]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator(
          t => condNel(cc.contains(t)(u), t , s"$t does not contain $u")
        )
    }
  }


  def apply[U](anyOf: AnyOfWord[U]): ValidatorBuilder[Given[U]#CanContainT] = {

    new ValidatorBuilder[Given[U]#CanContainT]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator(
          t => condNel(cc.containsAnyOf(t)(anyOf.values), t , s"$t does not contain any element of ${anyOf.values}")
        )
    }
  }


  def apply[U](allOf: AllOfWord[U]): ValidatorBuilder[Given[U]#CanContainT] = {

    new ValidatorBuilder[Given[U]#CanContainT]{
      def apply[T](implicit cc: CanContain[U,T]): Validator[String,T] =
        Validator(
          t => condNel(cc.containsAllOf(t)(allOf.values), t , s"$t does not contain all element of ${allOf.values}")
        )
    }
  }
*/

}

final object contain extends ContainVerb
