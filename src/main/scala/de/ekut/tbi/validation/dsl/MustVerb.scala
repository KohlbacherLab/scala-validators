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

  def must[E](v: Validator[E,T]): ValidatedNel[E,R]

  def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]): ValidatedNel[E,R]

/*
  def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]): ValidatedNel[String,R]

  def must[E](be: BeValidator[E,T]): ValidatedNel[E,R]

  def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]): ValidatedNel[String,R]
*/

}



final class MustVerb[T](val t: T) extends MustOps[T,T]
{

  override def must[E](v: Validator[E,T]) = v(t)

  override def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]) =
    clause.apply[T].apply(t)

/*
  override def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]) =
    beClause.apply[T].apply(t)


  override def must[E](be: BeValidator[E,T]) = be(t)


  def must(matchRegex: Validator[String,String])(implicit str: T =:= String) = matchRegex(t)


  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    containClause.apply[T].apply(t)
*/
}



final class MustVerbTraversable[T,C[T]: Traverse] private[dsl](val ts: C[T]) extends MustOps[T,C[T]]
{

  override def must[E](v: Validator[E,T]) = ts.traverse(v)


  override def must[E,Constraint[_]](clause: ValidatorBuilder[E,Constraint])(implicit constraint: clause.Constraint[T]) =
    ts.traverse(clause.apply[T])

/*
  override def must[Constraint[_]](beClause: BeClause[Constraint])(implicit constraint: Constraint[T]) =
    ts.traverse(beClause.apply[T])


  override def must[E](be: BeValidator[E,T]) =
    ts.traverse(be)


  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    ts.asInstanceOf[C[String]].traverse(regexMatch)


  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    ts.traverse(containClause.apply[T])
*/
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
