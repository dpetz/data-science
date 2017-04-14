package linalg

import linalg.LinAlg.Vec

import scala.collection.{AbstractSeq, TraversableLike}


object LinAlg {

  type Vec=Seq[Double]

  def op(v:Vec,w:Vec,f:(Double,Double)=>Double)=v.zip(w).map(x=>f(x._1,x._2))

  def +(v:Vec,w:Vec)=op(v,w,_+_)

  def +(v:Vec,d:Double)=v map (_+d)

  def *(v:Vec,w:Vec)=op(v,w,_*_)

  /** Dot product **/
  def dot(v:Vec,w:Vec) = *(v,w).sum


}

/**
  *
  */
trait Matrix extends Seq[Double] {

  def apply(i:Int,j:Int)
  def rows:Int
  def cols:Int


}

class RowMatrix(v:Seq[Double],width:Int) extends AbstractSeq[Double] with Matrix {

  require(v.size % width == 0)

  def rows=v.size / width

  def cols=width

  def apply(i:Int,j:Int)=v(i*width+j)

  override def iterator = v.iterator

  override def apply(idx: Int): Double = v(idx)

  override def length = v.length


}


//object Gradient {
//  def diffQuotient(f:Vec=>Double, x:Vec, h:Double=0.00001)=
//  (f(x + h) - f(x)) / h
//
//  /**compute the ith partial difference quotient of f at v*//
//  def partial_difference_quotient(f, v, i, h=0.00001):
//  w = [v_j + (h if j == i else 0)    # add h to just the ith element of v
//  for j, v_j in enumerate(v)]
//
//  return (f(w) - f(v)) / h
//
//  def estimate_gradient(f, v, h=0.00001):
//  [partial_difference_quotient(f, v, i, h)
//  for i, _ in enumerate(v)]
//
//}