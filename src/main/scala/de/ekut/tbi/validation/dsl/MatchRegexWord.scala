package de.ekut.tbi.validation.dsl


import cats.data.Validated.condNel

import scala.util.matching.Regex

import de.ekut.tbi.validation.{
  CanBeDefined,
  Validator,
  ValidatorBuilder
}




sealed trait RegexValidator extends Validator[String,String]


final object matchRegex
{

  def apply(regex: Regex): RegexValidator =
    new RegexValidator {
      def apply(t: String) = condNel(regex.matches(t), t , s"'$t' does not match regex '$regex'")
    }

  def apply(pattern: String): RegexValidator = apply(pattern.r)

}

