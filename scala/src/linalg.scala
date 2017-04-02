package linalg


/**
  *
  */
trait Matrix {

  def op(w:Matrix,op:(Double,Double)=>(Double)):Matrix

  def toSeq():Seq[Double]

  /** Row-wise */
  def +(w:Vec)=op(w,_+_)

  def +(d:Double)=new Vec(toSeq().map(_+d))

  def :*(w:Vec)=op(w,_*_)

  /** Dot product **/
  def *(w:Vec)=op(w,_*_).toSeq().sum

}

class VectorBasedMatrix(v:Seq[Double],n:Int=0,m:Int=0) extends Matrix{

  val rows = if (n > 0) n else v.size/m
  val cols = if (m > 0) m else v.size/n
  val data = v.toVector

  require(rows*cols == data.size)

  def toSeq() = data

  /** Apply binary operation pairwise to elements of two matrices **/
  def op(w:Matrix,op:(Double,Double)=>(Double))=
    new VectorBasedMatrix(toSeq.zip(w.toSeq()).map(x=>op(x._1,x._2)))


}

class Vec(v:Seq[Double]) extends VectorBasedMatrix(v,m=1)

object Vec {
  def apply(elems:Double*)=new Vec(elems.toVector)
  def apply(elems:Vector[Double])=new Vec(elems.toVector)
}


class DataColumn[D](val name:String, data:Seq[D])


class DataFrame(columns:DataColumn[Any])

object DataFrame {

  // def fromRows(names:Seq[String],types:Seq[Ty])

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