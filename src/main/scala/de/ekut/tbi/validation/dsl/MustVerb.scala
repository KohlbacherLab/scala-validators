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

  def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]): ValidatedNel[Any,R]

  def must[E](be: Validator[E,T]): ValidatedNel[E,R]


  def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]): ValidatedNel[String,R]

  def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]): ValidatedNel[String,R]

  def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]): ValidatedNel[E,R]

//  def must[E,C[_]](vb: ValidatorBuilder[E,C])(implicit cc: C[T]): ValidatedNel[E,R]
}



//final case class MustVerb[T](t: T) extends MustOps[T,T]
final class MustVerb[T](val t: T) extends MustOps[T,T]
{

  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) =
    beClause.apply[T].apply(t)


  override def must[E](be: Validator[E,T]) = be(t)


  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    regexMatch(t)


  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    containClause.apply[T].apply(t)

  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    clause.apply[T].apply(t)

  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    junction.apply[T].apply(t)

//  override def must[E,C[_]](vb: ValidatorBuilder[E,C])(implicit cc: C[T]) =
//    vb.apply[T].apply(t)

}



//final case class MustVerbTraversable[T,C[T]: Traverse] private[dsl](ts: C[T]) extends MustOps[T,C[T]]
final class MustVerbTraversable[T,C[T]: Traverse] private[dsl](val ts: C[T]) extends MustOps[T,C[T]]
{

  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) =
    ts.traverse(beClause.apply[T])


  override def must[E](be: Validator[E,T]) =
    ts.traverse(be)


  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    ts.asInstanceOf[C[String]].traverse(regexMatch)


  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    ts.traverse(containClause.apply[T])


  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    ts.traverse(clause.apply[T])


  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    ts.traverse(junction.apply[T])

//  override def must[E,C[_]](vb: ValidatorBuilder[E,C])(implicit cc: C[T]) = 
//     ts.traverse(vb.apply[T])
}
