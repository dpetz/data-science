import scala.util.Random
import LinAlg.SeqMathOps
import scala.annotation.tailrec

/**
  * Created by Dirk on 15.04.17.
  */
case class GradientDescent(
                       func:Seq[Double] => Double,
                       grad:Seq[Double]=>Seq[Double],
                       v:Seq[Double],
                       tolerance:Double=0.000001,
                       stepSizes:Seq[Double] = List(100,10,1,0.1,.01,.001,.0001,.00001),
                       previousDescent:Option[GradientDescent]=None)
  extends Traversable[GradientDescent] {

  type Vec = Seq[Double]

  def value:Double=func(v)

  def count:Int= if (previousDescent.isEmpty) 1 else previousDescent.get.count + 1

  private def save(func:Vec => Double,v:Vec):Double={
    val x = func(v)
    if (x.isNaN) Double.PositiveInfinity else x
  }

  private def step(v:Vec, direction:Vec, stepSize:Double):Vec =
    v.zip(direction).map { case (vi,zi) => vi + stepSize* zi }

  @tailrec final def minimize:GradientDescent= {
    val negGradient = grad(v).map(-_)
    val vMinFunc:Vec = stepSizes map { step(v,negGradient,_) } minBy { save(func,_) }
    if (previousDescent.isDefined)
      if ( Math.abs(previousDescent.get.value - func(vMinFunc)) < tolerance )
        return this
    GradientDescent(func,grad,vMinFunc,tolerance,stepSizes,Some(this)).minimize
  }

  override def foreach[U](f: (GradientDescent) => U)= {
    if (previousDescent.isDefined) previousDescent.get.foreach(f)
    f(this)
  }


}


object GradientDescent {

  type Vec = Seq[Double]

  def test {

    def sumOfSquares(v:Vec):Double=v.map(x=>x*x).sum
    def sumOfSquaresGradient(v:Vec):Vec=v.map(x=>2*x)

    // Random start with 3 coordinates from [-10,10)
    val vStart = Seq.fill(3)(Random.nextDouble*20-10)

    val min = GradientDescent(sumOfSquares,sumOfSquaresGradient,vStart).minimize

    println(s"Steps: ${min.count}")
    min.foreach { gd =>
      printf("%f --> %f\n",gd.value,gd.value)
    }
  }

}