package de.ekut.tbi.validation


import cats.data.ValidatedNel
import cats.data.Validated.condNel



trait Validator[E,T] extends (T => ValidatedNel[E,T])
{
  self => 

  def and(other: Validator[E,T]): Validator[E,T] = 
    t => self(t) andThen other


  def or(other: => Validator[E,T]): Validator[E,T] = 
    t => self(t) orElse other(t)

}


object Validator
{

  def apply[E,T](v: T => ValidatedNel[E,T]): Validator[E,T] =
    new Validator[E,T]{
      def apply(t: T) = v(t)
    }


  def apply[E,T](
    f: T => Boolean,
    error: T => E
  ): Validator[E,T] =
    Validator(t => condNel(f(t), t, error(t)) )

}
