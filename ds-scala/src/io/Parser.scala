package io

import io.Parser.{Cons, Rex}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object Parser {

  /** Result of applying Parser to Stream */
  sealed trait Parsed[A] {
    /** Map result if Match. */
    def map[B](f:A=>B):Parsed[B]
    /** Map complete match if Match. */
    def apply[B](f:(Match[A]=>Parsed[B])):Parsed[B]

    def follow:Reader

    def atFail(f:Fail[A]=>Parsed[A]):Parsed[A]
  }

  /** @param follow : Reader after parsed content has been consumed */
  case class Match[A](result: A,follow: Reader) extends Parsed[A] {
    def map[B](f:A=>B)=Match(f(result),follow)
    def apply[B](f:(Match[A]=>Parsed[B]))=f(this)
    def atFail(f:Fail[A]=>Parsed[A]) = this
  }

  case class Fail[A](parser:Parser[_], follow: Reader) extends Parsed[A] {
    def map[B](f:A=>B)=Fail[B](this)
    def apply[B](f:Match[A]=>Parsed[B])=Fail[B](this)
    override def toString=s"$parser failed parsing: $follow"
    def atFail(f:Fail[A]=>Parsed[A])=f(this)
  }
  object Fail {
    def apply[B](f:Fail[_]):Fail[B]=Fail[B](f.parser, f.follow)
  }

  /* Can be applied to a Stream to produce a Match[A] */
  trait Parser[A] {
    /* Parse from character stream. */
    def apply(r: Reader): Parsed[A]
    /* Shortcut for apply(Reader(s)) */
    def apply(s: String): Parsed[A] = apply(Reader(s))
    /* Infix operator for Then(this,p) */
    def ~(p:Parser[A])=Then(this,p)
  }

  /** Parse next n characters from stream into string */
  case class Chars(n: Int) extends Parser[String] {
    def apply(start: Reader) = {
      val buf = new ArrayBuffer[Char](n)

      def consume(r: Reader, i: Int) = {
        buf += r.char; r.next
      }

      val end = (1 to n).foldLeft(start)(consume)
      Match(buf.mkString(""), end)
    }
  }

  /** Parse predefined string */
  class Cons(val s: String) extends Parser[String] {
    def apply(r: Reader) =
      Chars(s.length)(r)  { m =>
        if (m.result == s) m else Fail(this,r)
      }
    override def toString=s"Cons($s)"
  }

  /** Any character of given set */
  case class OneOf(c: Set[Char]) extends Parser[Char] {
    def apply(r: Reader) =
      if (c(r.char)) Match(r.char, r.next)
      else Fail(this,r)
  }

  implicit class Str2Cons(s:String) extends Cons(s)

  implicit class Parser2SeqParser[A](p:Parser[A]) extends Parser[Seq[A]] {
    def apply(r:Reader)=p(r).map(List(_))
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
  case class Rex(n:Int)(reStr:String) extends Parser[String] {

    def apply(r:Reader)=
      reStr.r.findPrefixMatchOf(r.next(n)) match {
        case Some(m) => Chars(m.end)(r)
        case None => Fail(this,r)
      }

    override def toString=s"Rex($n)($reStr)"
  }

  case class Repeat[A](p:Parser[A], min:Int=0, max:Int=Int.MaxValue) extends Parser[Seq[A]] {

    def apply(r:Reader)={

      @tailrec
      def repeatNext(p:Parser[A], min:Int,max:Int, ms:Match[Seq[A]]):Parsed[Seq[A]]=
        p(r) map { List(_,ms.result) } { m =>
          if (max > 1) repeatNext(p, min - 1, max - 1, m)
          else m
        } atFail { f =>
          if (min<1) ms else Fail[B](this)
      }
    }
      repeatNext(p,min,max,Nil)
    }

  }


}

object Test {

  def and {
    import io.Parser.Str2Cons
    val r = Reader("Hallo Welt 2017")
    val p = "Hallo" ~ " Welt " ~ Rex(2)("""\d+""")
    println(p(r).toString)
  }
