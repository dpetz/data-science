package io

import scala.io.Source

/** Functional version of scala.io.Source */
trait Reader {
  /** Current character */
  def char:Char
  /** Advance to next character */
  def next:Reader
  /** Check if next character */
  def hasNext:Boolean
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