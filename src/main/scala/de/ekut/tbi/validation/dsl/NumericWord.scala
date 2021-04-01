package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder
}



sealed trait NumericWord[C[_]] extends ValidatorBuilder[String,C]


sealed trait NumericValidator[E,T] extends Validator[E,T]

object NumericValidator
{
  def apply[E,T: Numeric](v: Validator[E,T]): NumericValidator[E,T] =
    new NumericValidator[E,T]{ def apply(t: T) = v(t) }
}



final object positive extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(num.gt(t,num.zero), t , s"$t is not positive")
    )
}

final object nonNegative extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(num.gteq(t,num.zero), t , s"$t is not non-negative")
    )
}

final object negative extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(num.lt(t,num.zero), t , s"$t is not negative")
    )
}

final object equalTo
{

  def apply[T](u: T)(implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(t == u, t , s"$t is not equal to $u")
    )

}



/*
sealed trait NumericWord[C[_]] extends ValidatorBuilder[C]


final object positive extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]): Validator[String,T] =
    Validator(
      t => condNel(num.gt(t,num.zero), t , s"$t is not positive")
    )
}

final object nonNegative extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]): Validator[String,T] =
    Validator(
      t => condNel(num.gteq(t,num.zero), t , s"$t is not non-negative")
    )
}

final object negative extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]): Validator[String,T] =
    Validator(
      t => condNel(num.lt(t,num.zero), t , s"$t is not negative")
    )
}

final object equalTo
{

  type Given[U] = { type EqualsU[x] = U =:= x }


  def apply[U: Numeric](u: U): NumericWord[Given[U]#EqualsU] =
    new NumericWord[Given[U]#EqualsU] {
      def apply[T : Given[U]#EqualsU]: Validator[String,T] =
        Validator(
          t => condNel(t == u, t , s"$t is not equal to $u")
        )
    }

}
*/

