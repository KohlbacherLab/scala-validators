package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import scala.util.matching.Regex

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  NegatableValidator,
}



final object matchRegex
{

  def apply(regex: Regex): NegatableValidator[String,String] =
    Validator[String,String](
      regex.matches(_)
    )(
      t => s"'$t' does not match regex '$regex'",
      t => s"'$t' matches regex '$regex'"
    )

  def apply(pattern: String): NegatableValidator[String,String] = apply(pattern.r)

}

