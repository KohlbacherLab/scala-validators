package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder
}



sealed trait NumericWord[C[_]] extends ValidatorBuilder[String,C]

sealed trait OrderedWord[C[_]] extends ValidatorBuilder[String,C]


final object positive extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]) =
    Validator[String,T](
      num.gt(_,num.zero)
    )(
      t => s"$t is not positive",
      t => s"$t is positive"
    )
}

final object nonNegative extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]) =
    Validator[String,T](
      num.gteq(_,num.zero)
    )(
      t => s"$t is not non-negative",
      t => s"$t is non-negative"
    )
}

final object negative extends NumericWord[Numeric]
{
  def apply[T](implicit num: Numeric[T]) =
    Validator[String,T](
      num.lt(_,num.zero)
    )(
      t => s"$t is not negative",
      t => s"$t is negative"
    )
}


final object equalTo
{

  def apply[T](u: T)(implicit num: Numeric[T]) =
    Validator[String,T](
      t => t == u
    )(
      t => s"$t is not equal to $u",
      t => s"$t is equal to $u"
    )

}

final object lessThan
{

  def apply[T](u: T)(implicit num: Numeric[T]) =
    Validator[String,T](
      num.lt(_,u)
    )(
      t => s"$t is not less than $u",
      t => s"$t is less than $u"
    )

}

final object lessThanOrEqual
{

  def apply[T](u: T)(implicit num: Numeric[T]) = 
    Validator[String,T](
      num.lteq(_,u)
    )(
      t => s"$t is not less than or equal $u",
      t => s"$t is less than or equal $u"
    )

}


final object greaterThan
{

  def apply[T](u: T)(implicit num: Numeric[T]) = 
    Validator[String,T](
      num.gt(_,u)
    )(
      t => s"$t is not greater than $u",
      t => s"$t is greater than $u"
    )

}

final object greaterThanOrEqual
{

  def apply[T](u: T)(implicit num: Numeric[T]) =
    Validator[String,T](
      num.gteq(_,u)
    )(
      t => s"$t is not greater than or equal $u",
      t => s"$t is greater than or equal $u"
    )

}


final object before
{

  def apply[T](u: T)(implicit order: Ordering[T]) =
    Validator[String,T](
      order.lt(_,u)
    )(
      t => s"$t is not before $u",
      t => s"$t is before $u"
    )

}

final object after
{

  def apply[T](u: T)(implicit order: Ordering[T]) =
    Validator[String,T](
      order.gt(_,u)
    )(
      t => s"$t is not after $u",
      t => s"$t is after $u"
    )

}



/*
sealed trait NumericValidator[E,T] extends Validator[E,T]


object NumericValidator
{
  def apply[E,T: Numeric](v: Validator[E,T]): NumericValidator[E,T] =
    new NumericValidator[E,T]{ def apply(t: T) = v(t) }
}


sealed trait OrderedValidator[E,T] extends Validator[E,T]

object OrderedValidator
{
  def apply[E,T: Ordering](v: Validator[E,T]): OrderedValidator[E,T] =
    new OrderedValidator[E,T]{ def apply(t: T) = v(t) }
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

final object lessThan
{

  def apply[T](u: T)(implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(num.lt(t,u), t , s"$t is not less than $u")
    )

}

final object lessThanOrEqual
{

  def apply[T](u: T)(implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(num.lteq(t,u), t , s"$t is not less than or equal $u")
    )

}


final object greaterThan
{

  def apply[T](u: T)(implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(num.gt(t,u), t , s"$t is not greater than $u")
    )

}

final object greaterThanOrEqual
{

  def apply[T](u: T)(implicit num: Numeric[T]): NumericValidator[String,T] =
    NumericValidator(
      t => condNel(num.gteq(t,u), t , s"$t is not greater than or equal $u")
    )

}


final object before
{

  def apply[T](u: T)(implicit order: Ordering[T]): OrderedValidator[String,T] =
    OrderedValidator(
      t => condNel(order.lt(t,u), t , s"$t is not before $u")
    )

}

final object after
{

  def apply[T](u: T)(implicit order: Ordering[T]): OrderedValidator[String,T] =
    OrderedValidator(
      t => condNel(order.gt(t,u), t , s"$t is not after $u")
    )

}
*/
