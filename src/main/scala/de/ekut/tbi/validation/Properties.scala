package de.ekut.tbi.validation



import cats.data.Validated
import cats.data.Validated.condNel


sealed trait Unconstrained[-T]

object Unconstrained
{
  implicit def apply[T]: Unconstrained[T] = new Unconstrained[T]{}
}


trait CanBeDefined[-T]{
  def isDefined(t: T): Boolean
}

object CanBeDefined
{
  def apply[T](implicit cbd: CanBeDefined[T]) = cbd

  implicit def hasMethodIsDefined[T <: AnyRef { def isDefined: Boolean }]: CanBeDefined[T] =
    new CanBeDefined[T]{

      import scala.language.reflectiveCalls

      def isDefined(t: T) = t.asInstanceOf[{ def isDefined: Boolean }].isDefined

    }

}


trait CanBeSuccess[-T]{
  def isSuccess(t: T): Boolean
}

object CanBeSuccess
{
  def apply[T](implicit cbd: CanBeSuccess[T]) = cbd

  implicit def hasMethodIsSuccess[T <: AnyRef { def isSuccess: Boolean }]: CanBeSuccess[T] =
    new CanBeSuccess[T]{

      import scala.language.reflectiveCalls

      def isSuccess(t: T) = t.asInstanceOf[{ def isSuccess: Boolean }].isSuccess

    }

  implicit def validatedSuccess[E,T]: CanBeSuccess[Validated[E,T]] =
    new CanBeSuccess[Validated[E,T]]{
      def isSuccess(v: Validated[E,T]) = v.isValid
    }
}


trait CanBeEmpty[-T]{
  def isEmpty(t: T): Boolean
}

object CanBeEmpty
{
  def apply[T](implicit cbd: CanBeEmpty[T]) = cbd

  implicit def hasIsEmpty[T <: AnyRef { def isEmpty: Boolean }]: CanBeEmpty[T] =
    new CanBeEmpty[T]{

      import scala.language.reflectiveCalls

      def isEmpty(t: T) = t.asInstanceOf[{ def isEmpty: Boolean }].isEmpty

    }

  implicit def hasParameterlessIsEmpty[T <: AnyRef { def isEmpty(): Boolean }]: CanBeEmpty[T] =
    new CanBeEmpty[T]{

      import scala.language.reflectiveCalls

      def isEmpty(t: T) = t.asInstanceOf[{ def isEmpty(): Boolean }].isEmpty()

    }

}


trait CanContain[T,-C]{
  self =>

  def contains(c: C)(t: T): Boolean

  def containsOnly(c: C)(t: T): Boolean

  def containsAnyOf(c: C)(ts: Set[T]): Boolean =
    ts.exists(t => self.contains(c)(t))

  def containsAllOf(c: C)(ts: Set[T]): Boolean =
    ts.forall(t => self.contains(c)(t))

}


object CanContain
{
  def apply[T,C](implicit cc: CanContain[T,C]) = cc

  implicit def optionContains[T]: CanContain[T,Option[T]] =
    new CanContain[T,Option[T]]{
      def contains(opt: Option[T])(t: T) = opt.contains(t)

      def containsOnly(opt: Option[T])(t: T): Boolean = opt.contains(t)
    }

  implicit val stringContainsChar: CanContain[Char,String] =
    new CanContain[Char,String]{
      def contains(s: String)(ch: Char) = s.contains(ch)

      def containsOnly(s: String)(t: Char): Boolean =
        s.forall(_ == t)
    }

  implicit def stringContainsCharSequence[Chars <: CharSequence]: CanContain[Chars,String] =
    new CanContain[Chars,String]{
      def contains(s: String)(ch: Chars) = s.contains(ch)

      def containsOnly(c: String)(t: Chars): Boolean = {
        val s = t.toString

        if (s.length == 1) c.forall(_ == s.head)
        else c contentEquals s

      }
    }

  implicit def iterableContains[T,C[X] <: Iterable[X]]: CanContain[T,C[T]] =
    new CanContain[T,C[T]]{
      def contains(c: C[T])(t: T) = c.exists(_ == t)

      def containsOnly(c: C[T])(t: T): Boolean = c.forall(_ == t)
    }

  import scala.util.Either

  implicit def eitherContains[A,B]: CanContain[B,Either[A,B]] =
    new CanContain[B,Either[A,B]]{
      def contains(either: Either[A,B])(t: B) = either.contains(t)

      def containsOnly(either: Either[A,B])(t: B): Boolean = either.contains(t)
    }


}


trait CanHaveSize[-C]{
  def sizeOf(c: C): Int

  def hasSize(c: C)(size: Int): Boolean = sizeOf(c) == size
}

object CanHaveSize
{

  def apply[C](implicit cc: CanHaveSize[C]) = cc

  implicit def hasSize[C <: { def size: Int }]: CanHaveSize[C] =
    new CanHaveSize[C]{

      import scala.language.reflectiveCalls

      def sizeOf(c: C): Int =
        c.asInstanceOf[{ def size: Int }].size

    }

  implicit def charSeqSize[Chars <: CharSequence]: CanHaveSize[Chars] =
    new CanHaveSize[Chars]{
      def sizeOf(chars: Chars): Int =
        chars.length
    }

}
