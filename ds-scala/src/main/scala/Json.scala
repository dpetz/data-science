/**
  * Created by Dirk on 21.04.17.
  */

import Json.Lexer

import scala.collection.AbstractSeq
import scala.util.Try
import scala.util.matching.Regex.Match
import scala.util.matching.Regex

/** Parsed JSON String */
abstract class Json(in:Lexer) {
  def out:Lexer
}

/**
  *
  * TODO: Use https://github.com/scala/scala-parser-combinators or http://www.lihaoyi.com/fastparse/
  *
  * @see https://de.wikipedia.org/wiki/JavaScript_Object_Notation
  * @see https://www.tutorialspoint.com/scala/scala_regular_expressions.htm
  * @see https://en.wikipedia.org/wiki/Lexical_analysis
  * @see https://en.wikipedia.org/wiki/Recursive_descent_parser
  * @see https://en.wikipedia.org/wiki/Parser_combinator
  */
object Json {

  val sampleObject =
    """{
      |  "Herausgeber": "Xema",
      |  "Nummer": "1234-5678-9012-3456",
      |  "Deckung": 2e+6,
      |  "Waehrung": "EURO",
      |  "Inhaber":
      |  {
      |    "Name": "Mustermann",
      |    "Vorname": "Max",
      |    "maennlich": true,
      |    "Hobbys": [ "Reiten", "Golfen", "Lesen" ],
      |    "Alter": 42,
      |    "Kinder": [],
      |    "Partner": null
      |  }
      |}
    """.stripMargin

  val sampleMatrix= """[[1,49,4,0],[1,41,9,0],[1,40,8,0],[1,25,6,0],[1,21,1,0],[1,21,0,0],[1,19,3,0],[1,19,0,0],[1,18,9,0],[1,18,8,0],[1,16,4,0],[1,15,3,0],[1,15,0,0],[1,15,2,0],[1,15,7,0],[1,14,0,0],[1,14,1,0],[1,13,1,0],[1,13,7,0],[1,13,4,0],[1,13,2,0],[1,12,5,0],[1,12,0,0],[1,11,9,0],[1,10,9,0],[1,10,1,0],[1,10,1,0],[1,10,7,0],[1,10,9,0],[1,10,1,0],[1,10,6,0],[1,10,6,0],[1,10,8,0],[1,10,10,0],[1,10,6,0],[1,10,0,0],[1,10,5,0],[1,10,3,0],[1,10,4,0],[1,9,9,0],[1,9,9,0],[1,9,0,0],[1,9,0,0],[1,9,6,0],[1,9,10,0],[1,9,8,0],[1,9,5,0],[1,9,2,0],[1,9,9,0],[1,9,10,0],[1,9,7,0],[1,9,2,0],[1,9,0,0],[1,9,4,0],[1,9,6,0],[1,9,4,0],[1,9,7,0],[1,8,3,0],[1,8,2,0],[1,8,4,0],[1,8,9,0],[1,8,2,0],[1,8,3,0],[1,8,5,0],[1,8,8,0],[1,8,0,0],[1,8,9,0],[1,8,10,0],[1,8,5,0],[1,8,5,0],[1,7,5,0],[1,7,5,0],[1,7,0,0],[1,7,2,0],[1,7,8,0],[1,7,10,0],[1,7,5,0],[1,7,3,0],[1,7,3,0],[1,7,6,0],[1,7,7,0],[1,7,7,0],[1,7,9,0],[1,7,3,0],[1,7,8,0],[1,6,4,0],[1,6,6,0],[1,6,4,0],[1,6,9,0],[1,6,0,0],[1,6,1,0],[1,6,4,0],[1,6,1,0],[1,6,0,0],[1,6,7,0],[1,6,0,0],[1,6,8,0],[1,6,4,0],[1,6,2,1],[1,6,1,1],[1,6,3,1],[1,6,6,1],[1,6,4,1],[1,6,4,1],[1,6,1,1],[1,6,3,1],[1,6,4,1],[1,5,1,1],[1,5,9,1],[1,5,4,1],[1,5,6,1],[1,5,4,1],[1,5,4,1],[1,5,10,1],[1,5,5,1],[1,5,2,1],[1,5,4,1],[1,5,4,1],[1,5,9,1],[1,5,3,1],[1,5,10,1],[1,5,2,1],[1,5,2,1],[1,5,9,1],[1,4,8,1],[1,4,6,1],[1,4,0,1],[1,4,10,1],[1,4,5,1],[1,4,10,1],[1,4,9,1],[1,4,1,1],[1,4,4,1],[1,4,4,1],[1,4,0,1],[1,4,3,1],[1,4,1,1],[1,4,3,1],[1,4,2,1],[1,4,4,1],[1,4,4,1],[1,4,8,1],[1,4,2,1],[1,4,4,1],[1,3,2,1],[1,3,6,1],[1,3,4,1],[1,3,7,1],[1,3,4,1],[1,3,1,1],[1,3,10,1],[1,3,3,1],[1,3,4,1],[1,3,7,1],[1,3,5,1],[1,3,6,1],[1,3,1,1],[1,3,6,1],[1,3,10,1],[1,3,2,1],[1,3,4,1],[1,3,2,1],[1,3,1,1],[1,3,5,1],[1,2,4,1],[1,2,2,1],[1,2,8,1],[1,2,3,1],[1,2,1,1],[1,2,9,1],[1,2,10,1],[1,2,9,1],[1,2,4,1],[1,2,5,1],[1,2,0,1],[1,2,9,1],[1,2,9,1],[1,2,0,1],[1,2,1,1],[1,2,1,1],[1,2,4,1],[1,1,0,1],[1,1,2,1],[1,1,2,1],[1,1,5,1],[1,1,3,1],[1,1,10,1],[1,1,6,1],[1,1,0,1],[1,1,8,1],[1,1,6,1],[1,1,4,1],[1,1,9,1],[1,1,9,1],[1,1,4,1],[1,1,2,1],[1,1,9,1],[1,1,0,1],[1,1,8,1],[1,1,6,1],[1,1,1,1],[1,1,1,1],[1,1,5,1]]"""

  val sampleList= """[42,10]"""

  /** Split JSON String in it's tokens
    *
    */
  case class Lexer(s:CharSequence) {

    /** Any of: Number,String,[,],{,} */

    /** First match */
    private val fm: Option[Match] = Lexer.re.findPrefixMatchOf(s)

    /** First token */
    val first: Token =
      Token (fm.map { _.group(1) })

    /** Shorthand for first */
    def ^():Token=first

    def ?(e:String):Boolean= first == e


    /** Following tokens */
    lazy val follow: Lexer =
      Lexer (fm.map { _.after } getOrElse "")

    /** consume first token */
    def >>(e: String): Lexer = {
      first.assert(e)
      follow
    }

    /** consume first token if matches */
    def ~>(e: String): Lexer = {
      println(s"Testing if $this consuming $e ...")
      if (first == e) follow else this
    }

    def >>(e:Option[String]):Lexer= e.map(>>(_)).getOrElse(this)

  }

  object Lexer {
    val re:Regex = """(?:\s)*([\{\}\[\]:]|"\w+"|\d+)""".r
    val empty = new Lexer("")
  }

  case class Token(s:Option[String]) {

    /** Returns first token as Double (else exception)*/
    def double:Double = {
      Try {
        s.get.toDouble
      } getOrElse failed ("Double")
      .0
    }

    /** Returns first token as String (else exception) */
    def string:String = {
      Try {
        s.get.toString.replace("\"","")
      } getOrElse failed ("String")
      ""
    }

    def isEmpty= s.isEmpty | s.get.isEmpty
    def get=s getOrElse ""

    def assert(e:String) {
       if (this!=e) failed(e)
    }

    def ==(t:String):Boolean= s contains t
    def !=(t:String):Boolean= ! ==(t)

    private def failed(e:String) {
      throw new IllegalArgumentException (s"Not a $e: $s")
    }


  }

  val lexer = new Lexer(sampleList)

  val json = Json(lexer)

  def apply(in:Lexer):Json =
    in.first.get match {
      case "[" => Arr(in,Some("["))
      case _ => Num(in)
    }

  /** Parser for JSON Arrays */
  case class Arr(in:Lexer, consume:Option[String]=None) extends Json(in) {
      val head:Json = Json(in >> consume)
      val tail:Option[Arr] = if (head.out ? "]") None else Some (Arr(head.out,Some(",")))
      def out:Lexer = tail map(_.out) getOrElse (head.out >> "]")
      def toSeq:Seq[Json]= tail map (head :: _.toSeq.toList) getOrElse List(head)
    }

  case class Num(in:Lexer) extends Json(in) {
    val value:Double= {println(in.first);in.first.double }
    def out=in.follow
  }


}


