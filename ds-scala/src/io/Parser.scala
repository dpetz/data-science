package io

import io.Parser.{Consume, Rex}

import scala.collection.mutable.ArrayBuffer


object Parser {

  /** Result of applying Parser to Stream */
  sealed class Parsed[A] {
    /** Map result */
    def map[B](f:A=>B):Parsed[B]
  }

  /** @param follow : Reader after parsed content has been consumed */
  case class Match[A](result: A, follow: Reader) extends Parsed[A] {

    /** Appends results of p to results of this. Returns NoMatch if p is NoMatch */
    def <<[A](p:Parsed[A]):Parsed[Seq[A]]=
      p match {
        case m2:Match[A] => Match(List(result, m2.result), m2.follow)
        case _ => NoMatch[Seq[A]]
      }

    def map[B](f:A=>B)=Match(f(result),follow)
  }

  case class NoMatch[A]() extends Parsed[A] {
    def map[B](f:A=>B)=NoMatch[B]
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
  class Consume(val s: String) extends Parser[String] {
    def apply(r: Reader) = {
      Chars(s.length)(r) match {
        case m: Match[String] => if (m.result == s) m else NoMatch()
        case n:NoMatch[String] => n //n:NoMatch[String] => n
      }
    }
  }

  /** Any character of given set */
  case class Any(c: Set[Char]) extends Parser[Char] {
    def apply(r: Reader) =
      if (c(r.char)) Match(r.char, r.next)
      else NoMatch()
  }



  implicit class Infix(s:String) extends Consume(s)

  implicit class Parser2SeqParser[A](p:Parser[A]) extends Parser[Seq[A]] {
    def apply(r:Reader)=p(r).map(List(_))
  }

  /** p1 followed by p2 **/
  case class Then[A](p1: Parser[A], p2: Parser[A]) extends Parser[Seq[A]] {
    def apply(r: Reader) =
      p1(r) match {
        case m1: Match[A] => m1 << p2(m1.follow)
        case _ => NoMatch[Seq[A]]
      }
  }

  /** Find prefix Regex match within next n characters of stream  */
  case class Rex(reStr:String,n:Int) extends Parser[String] {
    val re = reStr.r
    def apply(r:Reader)=
      re.findPrefixMatchOf(r.next(n)) match {
        case Some(m) => Chars(m.end)(r)
        case None => NoMatch[String]
      }
  }
}

object Test {

  def and {
    import io.Parser.Infix
    val r = Reader("Hallo Welt7")
    val p = "Hallo" ~ " Welt" ~ Rex("\d")
    println(p(r).toString)
  }
  /*
  val reader = Reader("Hallo Welt")
  val m = Parser.Chars(6)(reader)
  m.toString
  val m2 = Parser.Consume("Welt")(m.follow)
  m2.toString
  */
}