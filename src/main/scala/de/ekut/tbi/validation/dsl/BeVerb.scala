package de.ekut.tbi.validation.dsl



import cats.data.ValidatedNel
import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeDefined,
  CanBeEmpty,
  CanContain,
  Validator,
  ValidatorBuilder
}



sealed trait BeClause[Constraint[_]] extends ValidatorBuilder[String,Constraint]


sealed trait BeValidator[E,T] extends Validator[E,T]
/*
object BeValidator
{
  def apply[E,T](f: T => ValidatedNel[E,T]): BeValidator[E,T] =
    new BeValidator[E,T]{
      def apply(t: T) = f(t)
    }

  def apply[E,T](
    f: T => Boolean,
    error: T => E
  ): BeValidator[E,T] =
    apply(t => condNel(f(t), t, error(t)))
}
*/

sealed trait BeVerb
{

  def apply[T](ref: T): BeValidator[String,T] =
    new BeValidator[String,T]{
      def apply(t: T) = condNel(t == ref, t, s"$t is not $ref")
    }



  def apply(defined: DefinedWord) =
    new BeClause[DefinedWord#Constraint]{
      def apply[T: DefinedWord#Constraint] = defined.apply[T]
    }


  def apply[Constraint[_]](nw: NumericWord[Constraint]) =
    new BeClause[Constraint]{ 
      def apply[T: Constraint] = nw.apply[T]
    }

/*
  def apply[E,T](nv: OrderedValidator[E,T]): BeValidator[E,T] = 
    new BeValidator[E,T]{
      def apply(t: T) = nv(t)
    }

  def apply[E,T](nv: NumericValidator[E,T]): BeValidator[E,T] = 
    new BeValidator[E,T]{
      def apply(t: T) = nv(t)
    }
*/

  def apply[E,T](v: Validator[E,T]): BeValidator[E,T] = 
    new BeValidator[E,T]{
      def apply(t: T) = v(t)
    }


  def apply(empty: EmptyWord) =
    new BeClause[EmptyWord#Constraint]{
      def apply[T: EmptyWord#Constraint] = empty.apply[T]
    }


  def apply[U](in: InWord[U]) =
    new BeClause[InWord[U]#Constraint]{
      def apply[T: InWord[U]#Constraint] = in.apply[T]
    } 


}

final object be extends BeVerb
