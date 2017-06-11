import scala.util.Random
import LinAlg.{Vec,elementwise}

/** Gradient of real-valued function
  * Estimated via difference quotient unless provided analytically
  * @param func Function mapping vectors to doubles
  * @param derivatives (Optional) analytical derivatives
  * @param h tiny constant to approximate limit for difference quotient
  *
  * */
class Gradient(val func:Vec=>Double, val derivatives:Option[(Vec,Int)=>Double]=None, val h:Double = 0.00001) {

  /** Compute the ith partial difference quotient of f at v */
  private def estimate(v:Vec, i:Int):Double={
    val w = v.zipWithIndex.map {
      case (x, j) => if (j==i) x+h else x
    }
    return (func(w) - func(v)) / h
  }

  /** Estimates gradient. Computational expensive (2n function evaluations)
    * and estimation error can be substantial, see Gradient.test
    * @return vector of partial difference quotients
    * */
  def estimate(v:Vec):Vec= v.indices.map { estimate(v,_) }

  /** Returns (derivatives or estimated) gradient at v */
  def apply(v:Vec):Vec= derivatives map { elementwise(v,_) } getOrElse estimate(v)

  /** Returns (derivatives or estimated) partial derivatives at v and index i */
  def apply(v:Vec,i:Int):Double= derivatives match {
    case Some(a) => a(v,i)
    case None    => estimate(v,i)
  }

  /* Negates function and derivatives gradient (if any) */
  def negate:Gradient=new Gradient(
    LinAlg.negateDoubleFunc(func),
    derivatives map { d => LinAlg.negateDoubleFunc(d) },
    h
  )
}

/** Call test to see it at work */
object Gradient {

  def apply(f:Vec=>Double,g:(Vec,Int)=>Double)=new Gradient(f,Some(g))
  def apply(f:Vec=>Double)=new Gradient(f)


  def test() {
    println("Creating vector with 10 random integers between 1 and 10 ...")
    println("Estimate gradient for function x_0^0 + x_1^1 + x_2^2 + ... + x_9^9")
    def sumPowersToIndex(v:Seq[Double])=
      v.zipWithIndex.map { case (x,i) => Math.pow(x,i) } .sum

    val v = Seq.fill(10)(Random.nextInt(10) + 1.0)
    val g = Gradient(sumPowersToIndex)(v)

    println("Vector=(" + v.map(_.toInt).mkString(",") +
      f"). Function value=${sumPowersToIndex(v)}%.2f\nv\tEstimate\tError")

    for (i <- 0 to 9) printf("%.1f\t%.1f\t%.4f\n" , v(i), g(i), g(i)-i*Math.pow(v(i),i-1))
  }

}