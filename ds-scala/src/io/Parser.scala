package io

import io.Parser.{Cons, Fail, Result, Rep, Rex}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object Parser {

  /** Result of applying Parser to Stream */
  sealed trait Result[+A] {

    def follow: Reader

    /** Map result if Match. */
    def map[B](f: A => B): Result[B]

    /** Map this instance if Match. */
    def apply[B](f: (Match[A] => Result[B])): Result[B]

    /** Map this instance if if Fail **/
    def atFail[B >: A](f: Fail[A] => Result[B]): Result[B]

    //def flat[B]:Parsed[B]

    //def asMatch:Match[A]
  }

  /** @param follow : Reader after parsed content has been consumed */
  case class Match[+A](result: A, follow: Reader) extends Result[A] {
    def map[B](f: A => B) = Match(f(result), follow)

    def apply[B](f: (Match[A] => Result[B])) = f(this)

    def atFail[B >: A](f: Fail[A] => Result[B]) = this

    /** New match with results combined by function and reader from second match */
    def add[B,C](m:Match[B])(f:(A,B)=>C ):Match[C]=Match(f(result,m.result),m.follow)

    //def asMatch = this
  }


  case class Fail[+A](parser: Parser[_], follow: Reader) extends Result[A] {
    def map[B](f: A => B) = Fail[B](this)

    def apply[B](f: Match[A] => Result[B]) = Fail[B](this)

    override def toString = s"$parser failed parsing: $follow"

    def atFail[B >: A](f: Fail[A] => Result[B]) = f(this)

    //def asMatch = throw new ClassCastException
  }

  object Fail {
    def apply[B](f: Fail[_]): Fail[B] = Fail[B](f.parser, f.follow)
  }

  /* Can be applied to a Stream to produce a Match[A] */
  trait Parser[+A] {
    /* Parse from character stream. */
    def apply(r: Reader): Result[A]

    /* Shortcut for apply(Reader(s)) */
    def apply(s: String): Result[A] = apply(Reader(s))

    /* Infix operator for Then(this,p) */
    def ~[B >: A](p: Parser[B]):Parser[Seq[B]] = Then(this, p)

    def |[B >: A](p: Parser[B]):Parser[B] = Or(this, p)

    def >[B](f:A=>B):Parser[B]=Trans(this,f)

  }

  /** Translate */
  case class Trans[A,B](p:Parser[A],f:A=>B) extends Parser[B] {
    def apply(r:Reader)=p(r) map { f(_) }
  }

  case class Gate[A,B,C](start:Parser[A], p:Parser[B], end:Parser[C]) extends Parser[B] {
    def apply(r:Reader)=Pre(start,Post(p,end))(r)
  }

  case class Pre[A,B](pre:Parser[A], p:Parser[B]) extends Parser[B] {
    def apply(r:Reader)= Then(pre,p)(r) map { _.tail match { case s:Seq[B] => s.head case b:B => b } }
  }

  case class Post[A,B](p:Parser[A], post:Parser[B]) extends Parser[A] {
    def apply(r:Reader)= Then(p,post)(r) map { _.dropRight(1) match { case s:Seq[A] => s.head case a:A => a } }
  }


  /** Parse next n characters from stream into string */
  case class Chars(n: Int) extends Parser[String] {
    def apply(start: Reader) = {
      val buf = new ArrayBuffer[Char](n)

      def consume(r: Reader, i: Int) = {
        buf += r.char;
        r.next
      }

      val end = (1 to n).foldLeft(start)(consume)
      Match(buf.mkString(""), end)
    }
  }

  /** Parse predefined string */
  case class Cons(val s: String) extends Parser[String] {
    def apply(r: Reader) =
      Chars(s.length)(r) { m =>
        if (m.result == s) m else Fail(this, r)
      }

    override def toString = s"Cons($s)"
  }

  /** Any character of given set */
  case class OneOf(c: Set[Char]) extends Parser[Char] {
    def apply(r: Reader) =
      if (c(r.char)) Match(r.char, r.next)
      else Fail(this, r)
  }



  case class Or[A](p1:Parser[A], p2:Parser[A]) extends Parser[A] {
    def apply(r: Reader) = p1(r).atFail { _ => p2(r) }
  }

  implicit class Str2Cons(s: String) extends Cons(s)

  implicit class Parser2SeqParser[+A](p: Parser[A]) extends Parser[Seq[A]] {
    def apply(r: Reader) = p(r).map(Vector(_))
  }

  /** p1 followed by p2 **/
  case class Then[A](p1: Parser[Seq[A]], p2: Parser[Seq[A]]) extends Parser[Seq[A]] {
    def apply(r: Reader) =
      p1(r) { m1 =>
        p2(m1.follow) { m2 =>
          Match[Seq[A]](m1.result ++ m2.result, m2.follow)
        }
      }
  }

  /** Find prefix Regex match within next n characters of stream  */
  case class Rex(n: Int)(reStr: String) extends Parser[String] {

    def apply(r: Reader) =
      reStr.r.findPrefixMatchOf(r.next(n)) match {
        case Some(m) => Chars(m.end)(r)
        case None => Fail(this, r)
      }

    override def toString = s"Rex($n)($reStr)"
  }

  /** Repeat */
  case class Rep[A](p: Parser[A], min: Int = 0, max: Int = Int.MaxValue) extends Parser[Seq[A]] {

    def apply(r: Reader) = {

      @tailrec
      def recurse(p: Parser[A], r:Reader, min: Int, max: Int, ms: Match[Seq[A]]) : Result[Seq[A]]= {
        if (max < 1) return ms
        p(r) match {
          case m: Match[A] =>
            recurse( p, m.follow, min - 1, max - 1, ms.add(m){ _ :+ _ } )
          case f: Fail[A] =>
            if (min < 1) ms else Fail[Seq[A]](f)
        }

      }
      recurse(p, r, min, max, Match(Vector[A](), r))
    }

  }
}
  object Test {

    def and {
      import io.Parser.{Rep, Str2Cons}
      val r = Reader("2017-08-26")
      val p = Rep(Rex(5)("""\d+-?"""))
      println(p(r).toString)
    }
  }

