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
  NegatableValidator,
  NegatableValidatorBuilder
}



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




sealed trait BeClause[C[_]] extends NegatableValidatorBuilder[Any,C]
{
  self =>

  type Type = BeClause[C]

  def negated =
    new BeClause[C]{
      def apply[T: Constraint] = self.apply[T].negated
    }
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

  def apply(valid: ValidWord): BeClause[HasValidator] =
    new BeClause[HasValidator]{ 
      def apply[T: HasValidator] = valid.apply[T]
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

    


  def apply[E,T](v: NegatableValidator[E,T]): BeValidator[E,T] = 
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
