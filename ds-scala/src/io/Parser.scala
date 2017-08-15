package io

import scala.collection.mutable.ArrayBuffer


object Parser {

  /** Result of applying Parser to Stream */
  sealed trait Parsed[A]

  /** @param follow : Reader after parsed content has been consumed */
  case class Match[A](result: A, follow: Reader) extends Parsed[A] {
  }

  case class NoMatch[A]() extends Parsed[A]

  /* Can be applied to a Stream to produce a Match[A] */
  trait Parser[A] {
    def apply(r: Reader): Parsed[A]
  }

  /** Parse next n characters from stream into string */
  case class Chars(n: Int) extends Parser[String] {
    def apply(start: Reader): Parsed[String] = {
      val buf = new ArrayBuffer[Char](n)

      def consume(r: Reader, i: Int) = {
        buf += r.char; r.next
      }

      val end = (1 to n).foldLeft(start)(consume)
      Match(buf.mkString(""), end)
    }
  }

  /** Parse predefined string */
  case class Consume(s: String) extends Parser[String] {
    def apply(r: Reader): Parsed[String] = {
      Chars(s.length)(r) match {
        case m: Match[String] => if (m.result == s) m else NoMatch()
        case n:NoMatch[String] => n //n:NoMatch[String] => n
      }
    }
  }

  /** Any character of given set */
  case class Any(c: Set[Char]) extends Parser[Char] {
    def apply(r: Reader): Parsed[Char] = {
      if (c(r.char)) Match(r.char, r.next)
      else NoMatch()
    }
  }

  /** p1 followed by p2 **/
  case class And[A](p1: Parser[A], p2: Parser[A]) extends Parser[Seq[A]] {
    def apply(r: Reader): Parsed[Seq[A]] = {
      p1(r) match {
        case m1: Match[A] => p2(m1.follow) match {
          case m2: Match[A] => Match(List(m1.result, m2.result), m2.follow)
          case _ => NoMatch[Seq[A]]
        }
        case _ => NoMatch[Seq[A]]
      }
    }
  }
}

object Test {

  def and {
    val r = Reader("Hallo Welt")
    val m = Parser.And(Parser.Consume("Hallo"),Parser.Consume(" Welt"))(r)
    println(m.toString)
  }
  /*
  val reader = Reader("Hallo Welt")
  val m = Parser.Chars(6)(reader)
  m.toString
  val m2 = Parser.Consume("Welt")(m.follow)
  m2.toString
  */
}