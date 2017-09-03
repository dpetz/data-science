package io

import io.Parser.{Cons, Or, Parser, Repeat, Rex, Then}


/*
abstract class Json {
def elems:Seq[]=Nil
}
*/

object Json {

  /** Translate */
  class Trans[A,B](p:Parser[A],f:A=>B) extends Parser[B] {
    def apply(r:Reader)=p(r) map { f(_) }
  }
  object Trans { def apply[A,B](p:Parser[A],f:A=>B):Trans[A,B]=  new Trans(p,f) }


  //val regex:Regex = """(?:\s*)([\{\}\[\]:,]|"[^"]*"|[\d-+.eE]+|null|true|false)""".r


  case class Str2Json(s:String, j:Json) extends Trans(Cons(s), {_:String => j})

  val pNum = Trans[String,Num](Rex(25)("""[\d-+.eE]+"""), { s:String => Num(s.toDouble) } )

  def trim[_](s:Seq[_])=s.drop(1).dropRight(1)

  // TODO Longer strings and escapes, see http://www.json.org/
  val pStr =  Trans[Seq[String],Str](Then(Then(Cons("\""),Repeat(Rex(1)("[^\"]"))),Cons("\"")), {
    s:Seq[String] => Str(trim(s).mkString(""))
  } )

  val pNull = Str2Json("null", Null)

  val pTrue = Str2Json("true", True)

  val pFalse = Str2Json("false", False)

  val pJson = Or[Json](pNum,Or(pStr,Or(pNull,Or(pTrue,pFalse))))


  private val ArrStart = Trans(Then(Cons("["),pJson), {s:Seq[_] => s(1)})

  private val ArrElem = Trans(Then(Cons(","),pJson), {s:Seq[_] => s(1)})

  val pArr = Trans(Then(Then(ArrStart,Repeat(ArrElem)),Cons("]")),
    {m:Seq[_] => {
      Str(m.dropRight(1).mkString(";"))
    }})

  /** JSON object */
  sealed trait Json

  sealed trait NonTerminal extends Json
  sealed trait Terminal extends Json

  /** Array */
  case class Arr (values:Seq[Json]) extends NonTerminal
  /** Object */
  case class Obj (pairs:Map[String,Json]) extends NonTerminal
  /** Symbol */
  sealed class Sym(override val toString:String) extends Terminal
  /** Value */
  sealed trait Val[A] extends Json { def value:A}

  case object True extends Sym("true")
  case object Null extends Sym("null")
  case object False extends Sym("false")

  case class Num(value:Double) extends Val[Double]
  case class Str(value:String) extends Val[String]




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

  val matrix = """[[1,49,4,0],[1,41,9,0],[1,40,8,0],[1,25,6,0],[1,21,1,0],[1,21,0,0],[1,19,3,0],[1,19,0,0],[1,18,9,0],[1,18,8,0],[1,16,4,0],[1,15,3,0],[1,15,0,0],[1,15,2,0],[1,15,7,0],[1,14,0,0],[1,14,1,0],[1,13,1,0],[1,13,7,0],[1,13,4,0],[1,13,2,0],[1,12,5,0],[1,12,0,0],[1,11,9,0],[1,10,9,0],[1,10,1,0],[1,10,1,0],[1,10,7,0],[1,10,9,0],[1,10,1,0],[1,10,6,0],[1,10,6,0],[1,10,8,0],[1,10,10,0],[1,10,6,0],[1,10,0,0],[1,10,5,0],[1,10,3,0],[1,10,4,0],[1,9,9,0],[1,9,9,0],[1,9,0,0],[1,9,0,0],[1,9,6,0],[1,9,10,0],[1,9,8,0],[1,9,5,0],[1,9,2,0],[1,9,9,0],[1,9,10,0],[1,9,7,0],[1,9,2,0],[1,9,0,0],[1,9,4,0],[1,9,6,0],[1,9,4,0],[1,9,7,0],[1,8,3,0],[1,8,2,0],[1,8,4,0],[1,8,9,0],[1,8,2,0],[1,8,3,0],[1,8,5,0],[1,8,8,0],[1,8,0,0],[1,8,9,0],[1,8,10,0],[1,8,5,0],[1,8,5,0],[1,7,5,0],[1,7,5,0],[1,7,0,0],[1,7,2,0],[1,7,8,0],[1,7,10,0],[1,7,5,0],[1,7,3,0],[1,7,3,0],[1,7,6,0],[1,7,7,0],[1,7,7,0],[1,7,9,0],[1,7,3,0],[1,7,8,0],[1,6,4,0],[1,6,6,0],[1,6,4,0],[1,6,9,0],[1,6,0,0],[1,6,1,0],[1,6,4,0],[1,6,1,0],[1,6,0,0],[1,6,7,0],[1,6,0,0],[1,6,8,0],[1,6,4,0],[1,6,2,1],[1,6,1,1],[1,6,3,1],[1,6,6,1],[1,6,4,1],[1,6,4,1],[1,6,1,1],[1,6,3,1],[1,6,4,1],[1,5,1,1],[1,5,9,1],[1,5,4,1],[1,5,6,1],[1,5,4,1],[1,5,4,1],[1,5,10,1],[1,5,5,1],[1,5,2,1],[1,5,4,1],[1,5,4,1],[1,5,9,1],[1,5,3,1],[1,5,10,1],[1,5,2,1],[1,5,2,1],[1,5,9,1],[1,4,8,1],[1,4,6,1],[1,4,0,1],[1,4,10,1],[1,4,5,1],[1,4,10,1],[1,4,9,1],[1,4,1,1],[1,4,4,1],[1,4,4,1],[1,4,0,1],[1,4,3,1],[1,4,1,1],[1,4,3,1],[1,4,2,1],[1,4,4,1],[1,4,4,1],[1,4,8,1],[1,4,2,1],[1,4,4,1],[1,3,2,1],[1,3,6,1],[1,3,4,1],[1,3,7,1],[1,3,4,1],[1,3,1,1],[1,3,10,1],[1,3,3,1],[1,3,4,1],[1,3,7,1],[1,3,5,1],[1,3,6,1],[1,3,1,1],[1,3,6,1],[1,3,10,1],[1,3,2,1],[1,3,4,1],[1,3,2,1],[1,3,1,1],[1,3,5,1],[1,2,4,1],[1,2,2,1],[1,2,8,1],[1,2,3,1],[1,2,1,1],[1,2,9,1],[1,2,10,1],[1,2,9,1],[1,2,4,1],[1,2,5,1],[1,2,0,1],[1,2,9,1],[1,2,9,1],[1,2,0,1],[1,2,1,1],[1,2,1,1],[1,2,4,1],[1,1,0,1],[1,1,2,1],[1,1,2,1],[1,1,5,1],[1,1,3,1],[1,1,10,1],[1,1,6,1],[1,1,0,1],[1,1,8,1],[1,1,6,1],[1,1,4,1],[1,1,9,1],[1,1,9,1],[1,1,4,1],[1,1,2,1],[1,1,9,1],[1,1,0,1],[1,1,8,1],[1,1,6,1],[1,1,1,1],[1,1,1,1],[1,1,5,1]]"""
  val list = """["zehn",10,55.75466,true,-44.565,55e-2,69234.2423432E78,null]"""
  // import Json.Lexer; val json = Json ( new Lexer(Json.Test.complex) )
}
}