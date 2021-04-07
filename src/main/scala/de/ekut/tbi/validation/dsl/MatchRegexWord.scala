package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import scala.util.matching.Regex

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder
}



final object matchRegex
{

  def apply(regex: Regex) =
    Validator[String,String](
      regex.matches(_)
    )(
      t => s"'$t' does not match regex '$regex'",
      t => s"'$t' matches regex '$regex'"
    )

  def apply(pattern: String): Validator[String,String] = apply(pattern.r)

}

