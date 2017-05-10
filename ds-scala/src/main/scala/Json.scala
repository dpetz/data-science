/**
  * Created by Dirk on 21.04.17.
  */

import scala.collection.immutable.ListMap

import java.util.Scanner
import java.util.regex.Pattern

import Json.{Arr, Lexer, Num, Str}
import scala.collection.immutable.ListMap
import scala.util.matching.Regex.Match
import scala.util.matching.Regex

/** Parsed JSON String.
  */
trait Json


/**
  *
  * TODO: Use https://github.com/scala/scala-parser-combinators or http://www.lihaoyi.com/fastparse/
  *
  * @see https://de.wikipedia.org/wiki/JavaScript_Object_Notation
  * @see https://www.tutorialspoint.com/scala/scala_regular_expressions.htm
  * @see https://en.wikipedia.org/wiki/Lexical_analysis
  * @see https://en.wikipedia.org/wiki/Recursive_descent_parser
  * @see https://en.wikipedia.org/wiki/Parser_combinator
  * @see https://jazzy.id.au/2012/11/06/iteratees_for_imperative_programmers.html
  */
object Json {

  val regex:Regex = """(?:\s*)([\{\}\[\]:,]|"[^"]*"|[\d-+.eE]+|null|true|false)""".r

  /** Call methods like 'next' withafter need to wrap string as option  **/
  implicit def Str2Opt( x : String) : Option[String] = Option(x)

  def apply(s:String):Json=apply(new Scanner(s))

  def apply(l:Lexer):Parsed =
    l.tok.get.charAt(0) match {
      case '[' => new Arr(l,"[")
      case '{' => new Obj(l,"{")
      case '"' => new Str(l)
      case 'n' => new Null(l)
      case 't' => new True(l)
      case 'f' => new False(l)
      case _ => new Num(l)
    }

  case class Parsed(json:Json,lex:Lexer)

  implicit class ScannerOps(val l:Lexer) {
    def next:String = s.next(Json.regex)
    def ?(str:String):Boolean = s.`match`.group(1) == str
  }

  class Num(l:Lexer) extends Json {
    val value:Double = s.next.toDouble
  }

  class Str(l:Lexer) extends Json {
    val value:String= s.next.replace("\"","")
  }

  class Sym(l:Lexer) extends Json {
    require ( s.next == toString)
  }

  class True(l:Lexer) extends Sym(s) {
    override def toString="true"
  }

  class False(l:Lexer) extends Sym(s) {
    override def toString="false"
  }

  class Null(l:Lexer) extends Sym(s) {
    override def toString="null"
  }



  /** A parsed JSON array. It works like a list:
    * 'head' points to current element, 'tail' to successors. */
  class Arr(l:Lexer, consume:Option[String]=None) extends Json {

    consume.foreach (s next _)

    /** Current Array element. None if empty array*/
      val head:Option[Json] = if (s hasNext "]") {s.next;None} else Some(Json(s))

      /** Subsequent array elements ore none for last element */
      val tail:Option[Arr] = if ( s.hasNext("]") ) {s.next;None} else Some (new Arr(s,","))


    /** Wrap as sequence */
      lazy val toSeq:Seq[Json]= head.map { h =>
        tail map (h :: _.toSeq.toList) getOrElse List(h)
        } getOrElse List()

      override def toString:String = s"[${toSeq.mkString(",")}]"
    }

  /** A parsed JSON object. It works like a MapList:
    * 'head' points to current (name,value) pair, 'tail' to successor pairs. */
  case class Obj(l:Lexer, consume:Option[String]=None) extends Json {

    consume.foreach (s next _)

    /** Current property name */
    val name:Option[Json] = if (s hasNext "}") {s.next;None} else Some(new Str(s))
    /** Current property value */
    val value:Option[Json] = name.map(j => Json(s.next(":")))


    /** Parses all subsequent properties */
    val tail:Option[Obj] = if ( s.hasNext("}") ) {s.next;None} else Some (new Obj(s,","))


    /** Wrap as sequence */
    lazy val toMap:Map[Json,Json]= name.map { n =>
      tail map ( _.toMap + ((n,value.get)) ) getOrElse ListMap((n,value.get))
    } getOrElse ListMap()

    override def toString:String = s"${getClass.getName}(${toMap.toList.mkString(",")})"
  }

  /**
    * Parses JSON String into sequence of string tokens.
    * Use 'tok' to access current token and '>>' to progress to next token.
    */
   class Lexer(s:CharSequence) {

    val fm:Option[Match] = Json.regex.findPrefixMatchOf(s)

    /** Current token. Any of: Number,String,[,],{,} */
    def tok: Option[String] = fm.map { _.group(1) }

    /** Check if current token equals a string */
    def ?(s:String):Boolean = tok contains s

    /** Consumes first token.
      * @param check Assert current token
      */
    def next(check:Option[String]=None):Lexer= {
      if (!check.forall(?(_))) throw
        new IllegalArgumentException(s"$check expected at $this")
      new Lexer (fm.get.after)
    }

    def toSeq:Seq[String]= tok map { _ :: next().toSeq.toList } getOrElse List()

    override def toString = s"Lexer($s)"
  }


  object Test {
    // TODO: http://www.scalatest.org/
    val complex =
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

    val matrix= """[[1,49,4,0],[1,41,9,0],[1,40,8,0],[1,25,6,0],[1,21,1,0],[1,21,0,0],[1,19,3,0],[1,19,0,0],[1,18,9,0],[1,18,8,0],[1,16,4,0],[1,15,3,0],[1,15,0,0],[1,15,2,0],[1,15,7,0],[1,14,0,0],[1,14,1,0],[1,13,1,0],[1,13,7,0],[1,13,4,0],[1,13,2,0],[1,12,5,0],[1,12,0,0],[1,11,9,0],[1,10,9,0],[1,10,1,0],[1,10,1,0],[1,10,7,0],[1,10,9,0],[1,10,1,0],[1,10,6,0],[1,10,6,0],[1,10,8,0],[1,10,10,0],[1,10,6,0],[1,10,0,0],[1,10,5,0],[1,10,3,0],[1,10,4,0],[1,9,9,0],[1,9,9,0],[1,9,0,0],[1,9,0,0],[1,9,6,0],[1,9,10,0],[1,9,8,0],[1,9,5,0],[1,9,2,0],[1,9,9,0],[1,9,10,0],[1,9,7,0],[1,9,2,0],[1,9,0,0],[1,9,4,0],[1,9,6,0],[1,9,4,0],[1,9,7,0],[1,8,3,0],[1,8,2,0],[1,8,4,0],[1,8,9,0],[1,8,2,0],[1,8,3,0],[1,8,5,0],[1,8,8,0],[1,8,0,0],[1,8,9,0],[1,8,10,0],[1,8,5,0],[1,8,5,0],[1,7,5,0],[1,7,5,0],[1,7,0,0],[1,7,2,0],[1,7,8,0],[1,7,10,0],[1,7,5,0],[1,7,3,0],[1,7,3,0],[1,7,6,0],[1,7,7,0],[1,7,7,0],[1,7,9,0],[1,7,3,0],[1,7,8,0],[1,6,4,0],[1,6,6,0],[1,6,4,0],[1,6,9,0],[1,6,0,0],[1,6,1,0],[1,6,4,0],[1,6,1,0],[1,6,0,0],[1,6,7,0],[1,6,0,0],[1,6,8,0],[1,6,4,0],[1,6,2,1],[1,6,1,1],[1,6,3,1],[1,6,6,1],[1,6,4,1],[1,6,4,1],[1,6,1,1],[1,6,3,1],[1,6,4,1],[1,5,1,1],[1,5,9,1],[1,5,4,1],[1,5,6,1],[1,5,4,1],[1,5,4,1],[1,5,10,1],[1,5,5,1],[1,5,2,1],[1,5,4,1],[1,5,4,1],[1,5,9,1],[1,5,3,1],[1,5,10,1],[1,5,2,1],[1,5,2,1],[1,5,9,1],[1,4,8,1],[1,4,6,1],[1,4,0,1],[1,4,10,1],[1,4,5,1],[1,4,10,1],[1,4,9,1],[1,4,1,1],[1,4,4,1],[1,4,4,1],[1,4,0,1],[1,4,3,1],[1,4,1,1],[1,4,3,1],[1,4,2,1],[1,4,4,1],[1,4,4,1],[1,4,8,1],[1,4,2,1],[1,4,4,1],[1,3,2,1],[1,3,6,1],[1,3,4,1],[1,3,7,1],[1,3,4,1],[1,3,1,1],[1,3,10,1],[1,3,3,1],[1,3,4,1],[1,3,7,1],[1,3,5,1],[1,3,6,1],[1,3,1,1],[1,3,6,1],[1,3,10,1],[1,3,2,1],[1,3,4,1],[1,3,2,1],[1,3,1,1],[1,3,5,1],[1,2,4,1],[1,2,2,1],[1,2,8,1],[1,2,3,1],[1,2,1,1],[1,2,9,1],[1,2,10,1],[1,2,9,1],[1,2,4,1],[1,2,5,1],[1,2,0,1],[1,2,9,1],[1,2,9,1],[1,2,0,1],[1,2,1,1],[1,2,1,1],[1,2,4,1],[1,1,0,1],[1,1,2,1],[1,1,2,1],[1,1,5,1],[1,1,3,1],[1,1,10,1],[1,1,6,1],[1,1,0,1],[1,1,8,1],[1,1,6,1],[1,1,4,1],[1,1,9,1],[1,1,9,1],[1,1,4,1],[1,1,2,1],[1,1,9,1],[1,1,0,1],[1,1,8,1],[1,1,6,1],[1,1,1,1],[1,1,1,1],[1,1,5,1]]"""
    val list= """[ "zehn",10, 55.75466, true,-44.565, 55e-2, 69234.2423432E78,null ]"""
    // import Json.Lexer; val json = Json ( new Lexer(Json.Test.complex) )

  }
}
