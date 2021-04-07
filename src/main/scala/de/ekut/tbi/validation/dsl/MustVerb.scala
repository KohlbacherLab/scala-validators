package de.ekut.tbi.validation.dsl


import cats.data.ValidatedNel
import cats.data.Validated.condNel

import cats.Traverse
import cats.syntax.traverse._


import de.ekut.tbi.validation.{
  Validator,
  ValidatorBuilder
}


sealed trait MustOps[T,R]
{

  def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]): ValidatedNel[String,R]

  def must[E](be: BeValidator[E,T]): ValidatedNel[E,R]

//  def must(matchRegex: Validator[String,String])(implicit str: T =:= String): ValidatedNel[String,R]

  def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]): ValidatedNel[String,R]

  def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]): ValidatedNel[String,R]
}



final class MustVerb[T](val t: T) extends MustOps[T,T]
{

  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) =
    beClause.apply[T].apply(t)


  override def must[E](be: BeValidator[E,T]) = be(t)


  def must(matchRegex: Validator[String,String])(implicit str: T =:= String) = matchRegex(t)


  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    containClause.apply[T].apply(t)

  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    clause.apply[T].apply(t)

}



final class MustVerbTraversable[T,C[T]: Traverse] private[dsl](val ts: C[T]) extends MustOps[T,C[T]]
{

  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) =
    ts.traverse(beClause.apply[T])


  override def must[E](be: BeValidator[E,T]) =
    ts.traverse(be)


  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    ts.asInstanceOf[C[String]].traverse(regexMatch)


  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    ts.traverse(containClause.apply[T])


  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    ts.traverse(clause.apply[T])

}
