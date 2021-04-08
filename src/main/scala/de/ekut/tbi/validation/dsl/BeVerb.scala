package de.ekut.tbi.validation.dsl



import cats.data.ValidatedNel
import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanBeDefined,
  CanBeEmpty,
  CanContain,
  Unconstrained,
  Validator,
  ValidatorBuilder
}


@annotation.implicitNotFound(
  "Couldn't find implicit Validator for ${T}. Define one or ensure it is in scope"
)
trait HasImplicitValidator[T]{
  val validator: Validator[String,T]
}

object HasImplicitValidator
{
  implicit def apply[T](implicit v: Validator[String,T]): HasImplicitValidator[T] =
    new HasImplicitValidator[T]{
      val validator = v
    }
}
 

sealed trait ValidWord extends ValidatorBuilder[String,HasImplicitValidator]
{
  self =>

  type Type = ValidWord

  def apply[T](implicit hv: HasImplicitValidator[T]): Validator[String,T] = hv.validator

  def negated =
    new ValidWord {   
      override def apply[T](implicit h: HasImplicitValidator[T]): Validator[String,T] = h.validator.negated
      override def negated = self
    }
  
  def or(other: => Type) =
    new ValidWord {   
      override def apply[T: HasImplicitValidator]: Validator[String,T] =
        t => self.apply[T].apply(t) orElse other.apply[T].apply(t)
    }
  
    
}
  
final object valid extends ValidWord

/*
final object valid extends ValidWord
{
  override def apply[T](implicit hv: HasImplicitValidator[T]): Validator[String,T] = hv.validator

  override def negated = invalid
  
  def or(other: => Type) =

}

final object invalid extends ValidWord
{
  override def apply[T](implicit hv: HasImplicitValidator[T]) = valid.apply[T].negated 

  override def negated = valid

}
*/




sealed trait BeClause[C[_]] extends ValidatorBuilder[String,C]
{
  self =>

  type Type = BeClause[C]

  def negated =
    new BeClause[C]{
      def apply[T: Constraint] = self.apply[T].negated
    }

  def or(other: => Type) =
    new BeClause[C]{
      def apply[T: Constraint] = 
        t => self.apply[T].apply(t) orElse other.apply[T].apply(t)
    }
}


sealed trait BeValidator[E,T] extends Validator[E,T]
{
  type Type = BeValidator[E,T]
}

object BeValidator
{

  private final case class Impl[E,T](v: Validator[E,T]) extends BeValidator[E,T]
  {
    override def apply(t: T) = v(t)

    override def negated = BeValidator(v.negated)
  }

  def apply[E,T](v: Validator[E,T]): BeValidator[E,T] = Impl(v)
}


sealed trait BeVerb
{

  self =>

  def apply(valid: ValidWord): BeClause[HasImplicitValidator] =
    new BeClause[HasImplicitValidator]{ 
      def apply[T: HasImplicitValidator] = valid.apply[T]
    }


  def apply[T](ref: T): BeValidator[String,T] =
    BeValidator(
      Validator[String,T](
        _ == ref
      )(
        t => s"$t is not $ref",
        t => s"$t is $ref"
      )
    )



  def apply(defined: DefinedWord): BeClause[DefinedWord#Constraint] =
    new BeClause[DefinedWord#Constraint]{
      def apply[T: DefinedWord#Constraint] = defined.apply[T]
    }


  def apply[Constraint[_]](nw: NumericWord[Constraint]): BeClause[Constraint] =
    new BeClause[Constraint]{ 
      def apply[T: Constraint] = nw.apply[T]
    }



  def apply[U](v: IsInstanceClause[U]): BeClause[Unconstrained] = 
    new BeClause[Unconstrained]{
      def apply[T: Unconstrained] = v.apply[T]
    }

    


  def apply[E,T](v: Validator[E,T]): BeValidator[E,T] = 
    BeValidator(v)


  def apply(empty: EmptyWord): BeClause[EmptyWord#Constraint] =
    new BeClause[EmptyWord#Constraint]{
      def apply[T: EmptyWord#Constraint] = empty.apply[T]
    }


  def apply[U](in: InWord[U]): BeClause[InWord[U]#Constraint] =
    new BeClause[InWord[U]#Constraint]{
      def apply[T: InWord[U]#Constraint] = in.apply[T]
    } 


}

final object be extends BeVerb
