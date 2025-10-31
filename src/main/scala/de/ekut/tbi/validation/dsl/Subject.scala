package de.ekut.tbi.validation.dsl


import cats.data.ValidatedNel
import cats.data.Validated.condNel
import cats.data.Validated.{
  Valid,
  Invalid
}
import cats.Semigroup
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

  private def check[E,Tpr](t: Option[Tpr],validator: Validator[E,Tpr]) =
    t.map(validator(_).map(Some(_)))
     .getOrElse(None.validNel[E])


  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) =
    check(opt,beClause.apply[T])

  override def must[E](be: Validator[E,T]) =
    check(opt,be)

  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    check(opt.asInstanceOf[Option[String]],regexMatch)

  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    check(opt,containClause.apply[T])

  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    check(opt,clause.apply[T])

  override def must[E,LC[_],RC[_]](junction: NegatableVBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    check(opt,junction.apply[T])

  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    check(opt,junction.apply[T])

}


final case class AllSubject[T,C[T]: Traverse] private[dsl](ts: C[T]) extends SubjectOps[T,C[T]]
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


final case class AtLeastSubject[T,C[X] <: Iterable[X]] private[dsl](n: Int, ts: C[T]) extends SubjectOps[T,C[T]]
{

  private def check[E,Tpr](ts: C[Tpr], validator: Validator[E,Tpr]) = {

    val (valids,invalids) = ts.map(validator).partition(_.isValid)

    (valids.size >= n) match {

      case true => ts.validNel

      case false =>
        implicit lazy val sg = Semigroup.instance[Tpr]((t1,t2) => t2)

        invalids.reduce(_ combine _).map(_ => ts)
    }

  }


  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) = 
    check(ts,beClause.apply[T])

  override def must[E](be: Validator[E,T]) =
    check(ts,be)

  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    check(ts.asInstanceOf[C[String]],regexMatch)

  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    check(ts,containClause.apply[T])

  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    check(ts,clause.apply[T])

  override def must[E,LC[_],RC[_]](junction: NegatableVBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    check(ts,junction.apply[T])

  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    check(ts,junction.apply[T])

}

/*
final case class AtMostSubject[T,C[X] <: Iterable[X]] private[dsl](n: Int, ts: C[T]) extends SubjectOps[T,C[T]]
{

  private def check[E,Tpr](ts: C[Tpr], validator: Validator[E,Tpr]) = {

    val (valids,invalids) = ts.map(validator).partition(_.isValid)

    (valids.size <= n) match {

      case true => ts.validNel

      case false =>
        implicit lazy val sg = Semigroup.instance[Tpr]((t1,t2) => t2)

        invalids.reduce(_ combine _).map(_ => ts)
    }

  }


  override def must[C[_]](beClause: BeClause[C])(implicit constraint: beClause.Constraint[T]) = 
    check(ts,beClause.apply[T])

  override def must[E](be: Validator[E,T]) =
    check(ts,be)

  def must(regexMatch: Validator[String,String])(implicit str: T =:= String) =
    check(ts.asInstanceOf[C[String]],regexMatch)

  override def must[U](containClause: ContainClause[U])(implicit cc: containClause.Constraint[T]) =
    check(ts,containClause.apply[T])

  override def must[C[_]](clause: HaveClause[C])(implicit cc: clause.Constraint[T]) =
    check(ts,clause.apply[T])

  override def must[E,LC[_],RC[_]](junction: NegatableVBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    check(ts,junction.apply[T])

  override def must[E,LC[_],RC[_]](junction: VBJunction[E,LC,RC])(implicit lc: LC[T], rc: RC[T]) =
    check(ts,junction.apply[T])

}
*/
