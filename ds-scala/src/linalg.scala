//package linalg

import scala.collection.AbstractSeq

object LinAlg {

/** Infix operators to treat sequences like math vec */
implicit class SeqMathOps(val v:Seq[Double]) extends AnyVal {

  type Vec = Seq[Double]
  type BinOp = (Double, Double) => Double

  /** zips and applies binary operation */
  private def op(w: Vec, f: BinOp) = v.zip(w).map(x => f(x._1, x._2))

  /** Add elementwise */
  def +(w: Vec) = op(w, _ + _)

  /** Substract elementwise */
  def -(w: Vec) = op(w, _ - _)


  /** Add constant */
  def +(d: Double) = v map (_ + d)

  /* Multiply elementwise */
  def *(w: Vec) = op(w, _ * _)

  /** Dot product **/
  def dot(w: Vec) = *(w).sum


}



  /**
    *
    */
  trait Matrix extends Seq[Double] {

    def apply(i:Int,j:Int)
    def rows:Int
    def cols:Int


  }

  object Matrix {


  class RowMatrix(v:Seq[Double],width:Int) extends AbstractSeq[Double] with Matrix {

    require(v.size % width == 0)

    def rows=v.size / width

    def cols=width

    def apply(i:Int,j:Int)=v(i*width+j)

    override def iterator = v.iterator

    override def apply(idx: Int): Double = v(idx)

    override def length = v.length

  }

    def fromRows(v:Seq[Double],width:Int)=new RowMatrix(v,width)

  }
}