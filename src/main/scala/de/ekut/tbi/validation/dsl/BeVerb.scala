package de.ekut.tbi.validation.dsl



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


sealed trait BeVerb
{


  def apply(defined: DefinedWord) =
    new BeClause[DefinedWord#Constraint]{
      def apply[T: DefinedWord#Constraint] = defined.apply[T]
    }


  def apply[Constraint[_]](nw: NumericWord[Constraint]) =
    new BeClause[Constraint]{ 
      def apply[T: Constraint] = nw.apply[T]
    }


  def apply[E,T](nv: NumericValidator[E,T]): BeValidator[E,T] = 
    new BeValidator[E,T]{
      def apply(t: T) = nv(t)
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
