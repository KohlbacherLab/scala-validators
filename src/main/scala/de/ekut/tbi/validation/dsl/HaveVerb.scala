package de.ekut.tbi.validation.dsl



import cats.data.Validated.condNel
import cats.syntax.apply._
import cats.instances.list._

import de.ekut.tbi.validation.{
  CanHaveSize,
  Validator,
  ValidatorBuilder,
  NegatableValidator,
  NegatableValidatorBuilder
}



sealed trait SizeClause extends NegatableValidatorBuilder[String,CanHaveSize]
{
  self =>

  type Type = SizeClause

  def negated =
    new SizeClause{
      def apply[T: Constraint] = self.apply[T].negated
    }
}

final class SizeWord private[dsl](val value: Int) extends AnyVal

object size
{
  def apply(l: Int): SizeWord =
    new SizeWord(l)
  
  def apply(v: NegatableValidator[String,Int]): SizeClause =
    new SizeClause {
      def apply[T](implicit chs: CanHaveSize[T]): NegatableValidator[String,T] =
        t => v(chs.sizeOf(t)).map(_ => t)
    }
}

object length
{
  def apply(l: Int): SizeWord = {
    new SizeWord(l)
  }
}


sealed trait HaveClause[C[_]] extends NegatableValidatorBuilder[String,C]
{
  self =>

  type Type = HaveClause[C]

  def negated =
    new HaveClause[C]{
      def apply[T: Constraint] = self.apply[T].negated
    }
    
}


sealed trait HaveVerb
{

  def apply(size: SizeClause): HaveClause[CanHaveSize] =
    new HaveClause[CanHaveSize]{
      def apply[T](implicit cs: CanHaveSize[T]) =
        size.apply[T]
    }

  def apply(size: SizeWord): HaveClause[CanHaveSize] =
    new HaveClause[CanHaveSize]{
      def apply[T](implicit cs: CanHaveSize[T]) =
        Validator[String,T](
          cs.hasSize(_)(size.value)
        )(
          t => s"$t does not have size ${size.value}",
          t => s"$t has size ${size.value}"
        )
    }

}

final object have extends HaveVerb
