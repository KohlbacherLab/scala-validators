package de.ekut.tbi.validation.dsl



import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  CanContain,
  Validator,
  ValidatorBuilder
}


sealed trait InWord[Us] extends ValidatorBuilder[String,({ type CanContainT[x] = CanContain[x,Us]})#CanContainT]

final object in
{

  def apply[Us](us: Us): InWord[Us] =
    new InWord[Us]{
      def apply[T](implicit cc: CanContain[T,Us]): Validator[String,T] =
        Validator[String,T](
          cc.contains(us)(_)
        )(
          t => s"$t is not contained in $us",
          t => s"$t is contained in $us"
        )
//        Validator(cc.contains(us)(_))(t => s"$t is not contained in $us")
    }
  
}

