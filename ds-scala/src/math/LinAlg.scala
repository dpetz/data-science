import scala.collection.AbstractSeq
import scala.util.Random

object LinAlg {

  type Vec = Seq[Double]

  val locale = java.util.Locale.US

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

  /** Return Infinity where function NaN */
  def save(func:Vec => Double,v:Vec,fallback:Double):Double={
    val x = func(v)
    if (x.isNaN) fallback else x
  }

  /** Negate vector function */
  def negateVecFunc[T](f:T => Vec):T=>Vec= { v:T => f(v).map {-_} }

  /** Negate real function (one argument) */
  def negateDoubleFunc[T](f:T => Double):T=>Double={ v:T => -f(v)}

  /* Why not working?
  def negate[T,R](f:T => R):(T=>R) = f match {
      case vf:(T=>Vec)    => v:T => vf(v).map {-_}
      case df:(T=>Double) =>  v:T => -df(v)
      case _ => throw new IllegalArgumentException(s"Not evaluating to Vec or Double: $f")
  }
  */

  /** Negate real function (two arguments) */
  def negateDoubleFunc[T1,T2](f:(T1,T2) => Double):(T1,T2) => Double={(v:T1,i:T2) => -f(v,i) }

  /** Applies function to all vector elements */
  def elementwise(v:Vec, f:(Vec,Int)=>Double):Vec= v.indices map { f(v,_) }


  /** String interpolate vecto elements */
  def format(v:Vec,ipol:String,start:String="(",end:String=")"):String={
    v./: (start) { (s,d) =>
      s +  { if (s != start)  "," else "" } + ( ipol formatLocal(locale, d) )
    } + end
  }

  /* Returns random slices of given size.
 * Each element returns once except remainder which is ignored */
  def randomSlices(v:Vec,size:Int=1):TraversableOnce[Vec]={
    val vShuffled = Random.shuffle(v)
    for (b <- 0 until v.length / size)
      yield vShuffled.slice(b*size,b*size+size)
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

      /** Assert indices */
      private def assert(i:Int,j:Int): Unit ={
        if (j<0 | j>=width)
          throw new IllegalArgumentException(f"Column index not in [0,$width):$j")
        if (i<0 | i>=(size/width))
          throw new IllegalArgumentException(f"Row index not in [0,${size/width}):$i")
      }
      def rows=v.size / width
      def cols=width
      def apply(i:Int,j:Int)={ assert(i,j); v(i*width+j) }
      override def iterator = v.iterator
      override def apply(idx: Int): Double = v(idx)
      override def length = v.length
    }

    def fromRows(v:Seq[Double],width:Int)=new RowMatrix(v,width)
  }
}
