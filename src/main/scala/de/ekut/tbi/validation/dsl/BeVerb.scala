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
  
final object valid extends ValidWord
{
  def apply[T](implicit hv: HasImplicitValidator[T]): Validator[String,T] = hv.validator
}

final object invalid extends ValidWord
{
  def apply[T](implicit hv: HasImplicitValidator[T]) = valid.apply[T].negated 
}





sealed trait BeClause[Constraint[_]] extends ValidatorBuilder[String,Constraint]


//sealed trait BeValidator[E,T] extends Validator[E,T]
sealed trait BeValidator[E,T] extends Validator[E,T]
{
  type Sub = BeValidator[E,T]
}

object BeValidator
{
  private final case class Impl[E,T](v: Validator[E,T]) extends BeValidator[E,T]
  {
    override def apply(t: T) = v(t)

    override def negated = BeValidator(v.negated)
//    override def negated = v.negated
  }

  def apply[E,T](v: Validator[E,T]): BeValidator[E,T] = Impl(v)
}


sealed trait BeVerb
{

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
//    new BeValidator[String,T]{
//      def apply(t: T) = condNel(t == ref, t, s"$t is not $ref")
//    }



  def apply(defined: DefinedWord) =
    new BeClause[DefinedWord#Constraint]{
      def apply[T: DefinedWord#Constraint] = defined.apply[T]
    }


  def apply[Constraint[_]](nw: NumericWord[Constraint]) =
    new BeClause[Constraint]{ 
      def apply[T: Constraint] = nw.apply[T]
    }


  def apply[E,T](v: Validator[E,T]): BeValidator[E,T] = 
    BeValidator(v)
//    new BeValidator[E,T]{
//      def apply(t: T) = v(t)
//    }


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
