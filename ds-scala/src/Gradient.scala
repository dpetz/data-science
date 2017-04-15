import scala.util.Random

/** Estimates gradient of real-valued function via difference quotient.
  * Works but computational expensive (2n function evaluations)
  * and estimation error can be substantial, see Gradient.test
  * @param f Function mapping vectors to doubles
  * @param h tiny constant to approximate limit for difference quotient
  *
  * */
class Gradient(val f:Seq[Double]=>Double, val h:Double = 0.00001) {

  type Vec = Seq[Double]

  /** Compute the ith partial difference quotient of f at v */
  private def partialDiffQuot(v:Vec, i:Int):Double={
    val w = v.zipWithIndex.map {
      case (x, j) => if (j==i) x+h else x
    }
    return (f(w) - f(v)) / h
  }

  /** Estimates gradient.
    * @return vector of partial difference quotients
    */
  def estimateAt(v:Vec):Vec=
    v.indices.map { partialDiffQuot(v,_) }

}

/** Call test to see it at work */
object Gradient {

  def apply(f:Seq[Double]=>Double)=new Gradient(f)

  def test() {
    println("Creating vector with 10 random integers between 1 and 10 ...")
    println("Estimate gradient for function x_0^0 + x_1^1 + x_2^2 + ... + x_9^9")
    def sumPowersToIndex(v:Seq[Double])=
      v.zipWithIndex.map { case (x,i) => Math.pow(x,i) } .sum

    val v = Seq.fill(10)(Random.nextInt(10) + 1.0)
    val g = Gradient(sumPowersToIndex).estimateAt(v)

    println("Vector=(" + v.map(_.toInt).mkString(",") +
      f"). Function value=${sumPowersToIndex(v)}%.2f\nv\tEstimate\tError")

    for (i <- 0 to 9) printf("%.1f\t%.1f\t%.4f\n" , v(i), g(i), g(i)-i*Math.pow(v(i),i-1))
  }

}