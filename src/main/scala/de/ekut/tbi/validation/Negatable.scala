package de.ekut.tbi.validation



trait Negatable[+S]
{
  type Type <: S

  def negated: Type
}
