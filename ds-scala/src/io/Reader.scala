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
  /** Position since start (zero based) */
  def pos:Int

  /** Peek into next n characters */
  def next(n:Int):String = {
    /** Accumulator parameter to allow recursive call optimization.
      * See https://stackoverflow.com/questions/6005392/isnt-that-code-in-tail-recursive-style */
    @tailrec
    def collectNext(l:Seq[Char], r:Reader, n:Int):Seq[Char] = {
      if (!r.hasNext) return l ++ "<END>".toCharArray
      if (n==0) return l
      return collectNext(l :+ r.char, r.next, n-1)
    }
    collectNext(Vector(), this,n).mkString
  }

  override def toString=s"Reader(${next(5)}...)"
}

object Reader {

  /** Implement via lazy Source. Not exposed to preserve immutability */
  private class SourceReader(src:Source,val pos:Int) extends Reader {
    lazy val char=src.next
    lazy val hasNext=src.hasNext
    lazy val next= {
      char // trigger src.next()
      new SourceReader(src,pos+1)
    }
  }
  def apply(s:String):Reader=new SourceReader(Source.fromString(s),0)
}