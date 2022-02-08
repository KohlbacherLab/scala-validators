package de.ekut.tbi.validation.dsl


import cats.data.ValidatedNel
import cats.data.Validated.condNel

import cats.Traverse
import cats.syntax.traverse._
import cats.syntax.validated._


import de.ekut.tbi.validation.{
  Validator,
  ValidatorBuilder
}


sealed trait SubjectOps[T,R]
{

  def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]): ValidatedNel[Any,R]

  def must[E](be: Validator[E,T]): ValidatedNel[E,R]


  def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]): ValidatedNel[String,R]

  def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]): ValidatedNel[String,R]

  def must[E,LC[_],RC[_]](junction: NegatableVBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]): ValidatedNel[E,R]

  def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]): ValidatedNel[E,R]

}



final case class Subject[T](t: T) extends SubjectOps[T,T]
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

  override def must[E,LC[_],RC[_]](junction: NegatableVBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    junction.apply[T].apply(t)

  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    junction.apply[T].apply(t)

}


final case class OptionSubject[T] private[dsl](val opt: Option[T]) extends SubjectOps[T,Option[T]]
{

  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) =
    opt.map(beClause.apply[T].apply(_).map(Some(_)))
       .getOrElse(None.validNel[String])


  override def must[E](be: Validator[E,T]) =
    opt.map(be(_).map(Some(_)))
       .getOrElse(None.validNel[E])


  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    opt.map(regexMatch(_).map(Some(_)))
       .getOrElse(None.validNel[String])


  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    opt.map(containClause.apply[T].apply(_).map(Some(_)))
       .getOrElse(None.validNel[String])


  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    opt.map(clause.apply[T].apply(_).map(Some(_)))
       .getOrElse(None.validNel[String])

  override def must[E,LC[_],RC[_]](junction: NegatableVBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    opt.map(junction.apply[T].apply(_).map(Some(_)))
       .getOrElse(None.validNel[E])

  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    opt.map(junction.apply[T].apply(_).map(Some(_)))
       .getOrElse(None.validNel[E])

}

final case class TraversableSubject[T,C[T]: Traverse] private[dsl](val ts: C[T]) extends SubjectOps[T,C[T]]
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

  override def must[E,LC[_],RC[_]](junction: NegatableVBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    ts.traverse(junction.apply[T])

  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    ts.traverse(junction.apply[T])

}
