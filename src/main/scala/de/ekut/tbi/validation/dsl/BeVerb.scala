package de.ekut.tbi.validation.dsl



import cats.data.ValidatedNel
import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanBeDefined,
  CanBeEmpty,
  CanContain,
  Unconstrained,
  Validator,
  ValidatorBuilder,
  Negatable,
  NegatableValidator,
  NegatableValidatorBuilder
}


/*
trait HasValidator[T]{
  val validator: Validator[Any,T]
}

object HasValidator
{
  implicit def apply[T](implicit v: Validator[Any,T]): HasValidator[T] =
    new HasValidator[T]{
      val validator = v
    }
}
 

sealed trait ValidWord extends ValidatorBuilder[Any,HasValidator]
{
  self =>

  type Type = ValidWord

  def apply[T](implicit hv: Constraint[T]): Validator[Any,T] = hv.validator
}
  
final object valid extends ValidWord
*/

sealed trait BeClause[C[_]] extends NegatableValidatorBuilder[Any,C]
{
  self =>

  type Type = BeClause[C]

  def negated =
    new BeClause[C]{
      def apply[T: Constraint] = self.apply[T].negated
    }
}


/*
//sealed trait BeClause[C[_], V <: ValidatorBuilder[Any,C]]
sealed trait BeClause[C[_]]
{
  type V <: ValidatorBuilder[Any,C]

//  this: V =>

//  type Constraint[x]
}


sealed trait SimpleBeClause[C[_]] extends BeClause[C] with ValidatorBuilder[Any,C]
//sealed trait SimpleBeClause[C[_]] extends BeClause[C,ValidatorBuilder[Any,C]] with ValidatorBuilder[Any,C]
{
  self =>

  type V = ValidatorBuilder[Any,C]

  type Type = SimpleBeClause[C]
}

sealed trait NegatableBeClause[C[_]] extends BeClause[C] with NegatableValidatorBuilder[Any,C]
//sealed trait NegatableBeClause[C[_]] extends BeClause[C,NegatableValidatorBuilder[Any,C]] with NegatableValidatorBuilder[Any,C]
{
  self =>

  type V = NegatableValidatorBuilder[Any,C]

  type Type = NegatableBeClause[C]

  def negated =
    new NegatableBeClause[C]{
      def apply[T: Constraint] = self.apply[T].negated
    }
}
*/


trait HasValidator[E,T]{
  val validator: NegatableValidator[E,T]
}

object HasValidator
{
  implicit def apply[E,T](implicit v: NegatableValidator[E,T]): HasValidator[E,T] =
    new HasValidator[E,T]{
      val validator = v
    }
}


sealed trait ValidWord[E] extends NegatableValidatorBuilder[E,({ type f[x] = HasValidator[E,x] })#f]
{
  self =>

  type Type = ValidWord[E]

  def apply[T](implicit hv: Constraint[T]) = hv.validator

  def negated =
    new ValidWord[E]{
      override def apply[T: Constraint]: NegatableValidator[E,T] = self.apply[T].negated
      override def negated = self
    }

}
  
final object valid
{
  implicit def apply[E]: ValidWord[E] = new ValidWord[E]{}
}


sealed trait BeValidator[E,T] extends NegatableValidator[E,T]
{
  type Type = BeValidator[E,T]
}

object BeValidator
{

  private final case class Impl[E,T](v: NegatableValidator[E,T]) extends BeValidator[E,T]
  {
    override def apply(t: T) = v(t)

    override def negated = BeValidator(v.negated)
  }

  def apply[E,T](v: NegatableValidator[E,T]): BeValidator[E,T] = Impl(v)
}


sealed trait BeVerb
{

  self =>

/*
  def apply(valid: ValidWord): BeClause[HasValidator] =
    new BeClause[HasValidator]{ 
      def apply[T: HasValidator] = valid.apply[T]
    }
*/

  def apply[E](valid: ValidWord[E]): BeClause[({ type f[x] = HasValidator[E,x] })#f] =
    new BeClause[({ type f[x] = HasValidator[E,x] })#f]{ 
      def apply[T: ({ type f[x] = HasValidator[E,x] })#f] = valid.apply[T]
    }

  def apply(v: valid.type): BeClause[({ type f[x] = HasValidator[Any,x] })#f] = apply(v.apply[Any])



  def apply[T](ref: T): BeValidator[String,T] =
    BeValidator(
      Validator[String,T](
        _ == ref
      )(
        t => s"$t is not $ref",
        t => s"$t is $ref"
      )
    )


//  def apply(defined: DefinedWord): BeClause[DefinedWord#Constraint] =
  def apply(defined: DefinedWord): BeClause[DefinedWord#Constraint] =
    new BeClause[DefinedWord#Constraint]{
      def apply[T: DefinedWord#Constraint] = defined.apply[T]
    }


//  def apply(success: SuccessWord): BeClause[SuccessWord#Constraint] =
  def apply(success: SuccessWord): BeClause[SuccessWord#Constraint] =
    new BeClause[SuccessWord#Constraint]{
      def apply[T: SuccessWord#Constraint] = success.apply[T]
    }


//  def apply[Constraint[_]](nw: NumericWord[Constraint]): BeClause[Constraint] =
  def apply[Constraint[_]](nw: NumericWord[Constraint]): BeClause[Constraint] =
    new BeClause[Constraint]{ 
      def apply[T: Constraint] = nw.apply[T]
    }



//  def apply[U](v: IsInstanceClause[U]): BeClause[Unconstrained] = 
  def apply[U](v: IsInstanceClause[U]): BeClause[Unconstrained] = 
    new BeClause[Unconstrained]{
      def apply[T: Unconstrained] = v.apply[T]
    }

    


//  def apply[E,T](v: NegatableValidator[E,T]): BeValidator[E,T] = 
  def apply[E,T](v: NegatableValidator[E,T]): BeValidator[E,T] = 
    BeValidator(v)


//  def apply(empty: EmptyWord): BeClause[EmptyWord#Constraint] =
  def apply(empty: EmptyWord): BeClause[EmptyWord#Constraint] =
    new BeClause[EmptyWord#Constraint]{
      def apply[T: EmptyWord#Constraint] = empty.apply[T]
    }


//  def apply[U](in: InWord[U]): BeClause[InWord[U]#Constraint] =
  def apply[U](in: InWord[U]): BeClause[InWord[U]#Constraint] =
    new BeClause[InWord[U]#Constraint]{
      def apply[T: InWord[U]#Constraint] = in.apply[T]
    } 


}


final object be extends BeVerb
