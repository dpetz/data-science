package io

import scala.annotation.tailrec
import scala.io.Source

/** Functional version of scala.io.Source */
trait Reader {
  /** Current character */
  def char:Char
  /** Advance to next character */
  def next:Reader
  /** Check if next character */
  def hasNext:Boolean

  /** Peek into next n characters */
  def next(n:Int):String = {
    /** Accumulator parameter to allow recursive call optimization.
      * See https://stackoverflow.com/questions/6005392/isnt-that-code-in-tail-recursive-style */
    @tailrec
    def nextAsList(l:List[Char], r:Reader, n:Int):List[Char] =
      if (n==1) r.char :: l
      else nextAsList(r.char :: l, r.next, n-1)
    nextAsList(Nil, this,n).toString
  }
}

object Reader {

  /** Implement via lazy Source. Not exposed to preserve immutability */
  private class SourceReader(src:Source) extends Reader {
    lazy val char:Char=src.next()
    lazy val hasNext:Boolean=src.hasNext
    lazy val next:Reader= {
      char // trigger src.next()
      new SourceReader(src)
    }
  }
  def apply(s:String):Reader=new SourceReader(Source.fromString(s))
}