package de.ekut.tbi.validation.dsl


import cats.data.ValidatedNel
import cats.data.Validated.condNel

import de.ekut.tbi.validation.{
  ValidatorBuilder
}


sealed trait MustOps[T,R]
{

  def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]): ValidatedNel[E,R]

  def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]): ValidatedNel[String,R]

  def must[E](be: BeValidator[E,T]): ValidatedNel[E,R]

  def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]): ValidatedNel[String,R]

}



//final class MustVerb[T](val t: T) extends AnyVal with MustOps[T,T]
final class MustVerb[T](val t: T) extends MustOps[T,T]
{

  def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]) =
    clause.apply[T].apply(t)


  def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]) =
    beClause.apply[T].apply(t)


  def must[E](be: BeValidator[E,T]) = be(t)


  def must(matchRegex: RegexValidator)(implicit str: T =:= String) = matchRegex(t)


  def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    containClause.apply[T].apply(t)

}



import cats.Traverse
import cats.syntax.traverse._


final class MustVerbTraversable[T,C[T]: Traverse] private[dsl](val ts: C[T]) extends MustOps[T,C[T]]
{

  def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]) =
    ts.traverse(clause.apply[T])


  def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]) =
    ts.traverse(beClause.apply[T])


  def must[E](be: BeValidator[E,T]) =
    ts.traverse(be)


  def must(regexMatch: RegexValidator)(implicit str: T =:= String) =
    ts.asInstanceOf[C[String]].traverse(regexMatch)


  def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    ts.traverse(containClause.apply[T])

}



/*
final class MustVerb[T](val t: T) extends AnyVal
{

  def apply[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]) =
    clause.apply[T].apply(t)


  def apply[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]) =
    beClause.apply[T].apply(t)


  def apply[E](be: BeValidator[E,T]) = be(t)


  def apply(matchRegex: RegexValidator)(implicit str: T =:= String) = matchRegex(t)


  def apply[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    containClause.apply[T].apply(t)

}
*/
