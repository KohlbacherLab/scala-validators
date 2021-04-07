package de.ekut.tbi.validation.dsl



import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanHaveSize,
  Validator,
  ValidatorBuilder
}



sealed trait SizeWord
{
  val value: Int
}

object size
{
  def apply(l: Int): SizeWord = {
    new SizeWord{ val value = l }
  }
}


sealed trait HaveClause[C[_]] extends ValidatorBuilder[String,C]
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

  def apply(size: SizeWord): HaveClause[CanHaveSize] =
    new HaveClause[CanHaveSize]{
      def apply[T](implicit cs: CanHaveSize[T]) =
        Validator[String,T](
          cs.hasSize(_)(size.value)
        )(
          t => s"$t does not have size ${size.value}",
          t => s"$t has size ${size.value}",
        )
    }

}

final object have extends HaveVerb
