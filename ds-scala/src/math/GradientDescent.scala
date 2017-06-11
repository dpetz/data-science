import GradientDescent.{gridStepSizes, toleranceStop}
import LinAlg.{Vec, save,format,randomSlices}

import scala.util.Random
import scala.annotation.tailrec

/**
  * One step of a gradient descent algorithm
  *
  * @param grad Gradient including function
  * @param pos Current position/coordinates
  * @param stepSizes Sequence of possible step sizes
  * @param stop Predicate if continue
  * @param previous Previous descent step (if any)
  */
case class GradientDescent( grad:Gradient, pos:Vec,
                            stepSizes:GradientDescent => Seq[Double] =gridStepSizes,
                            stop:(GradientDescent, Vec) => Boolean=toleranceStop(_,_,0.000001),
                            previous:Option[GradientDescent]=None)
  extends Traversable[GradientDescent] {


  def value:Double=grad.func(pos)

  def count:Int= previous map (_.count + 1) getOrElse 1

  /**
    * Try one step.
    * @param v current position
    * @param direction direction of descent
    * @param stepSize candidate stepsize
    * @return new position
    */
  private def step(v:Vec, direction:Vec, stepSize:Double):Vec =
    v.zip(direction).map { case (vi,zi) => vi + stepSize* zi }

  /** Recursively descent
    * @return last step
    */
  @tailrec final def minimize:GradientDescent= {
    val negGradient = grad(pos).map(-_)
    val vMinFunc = stepSizes(this) map { step(pos,negGradient,_) } minBy { save(grad.func,_,Double.PositiveInfinity) }
    if ( stop(this,vMinFunc) ) return this
    new GradientDescent(grad,vMinFunc,stepSizes,stop,Some(this)).minimize
  }



  /* Iterate beginning first until this step */
  override def foreach[U](f: (GradientDescent) => U)= {
    previous.foreach(_.foreach(f))
    f(this)
  }
}

object GradientDescent {

  /** Stops if objective does not improve beyond tolerance. */
  def toleranceStop(gd:GradientDescent, nextV:Vec,tolerance:Double):Boolean=
    Math.abs(gd.value - gd.grad.func(nextV)) < tolerance

  def gridStepSizes(gd:GradientDescent):Seq[Double]=
    List(100,10,1,.1,.01,.001,.0001,.00001)


  def minimize(grad:Gradient,start:Vec):GradientDescent=apply(grad,start).minimize

  def maximize(grad:Gradient, start:Vec):GradientDescent=minimize(grad.negate,start)


  def test(dim:Int=3,dist:Double=10) {

    println(s"Descending $dim-dimensional v*v. Random start coordinates in [-$dist,$dist) ...")

    val min = minimize(
      Gradient(
        _.map(x=>x*x).sum,                          // function
        (v,i) => 2*v(i)),                           // ith derivative
      Seq.fill(dim)(Random.nextDouble*2*dist-dist)) // start random

    println("i\tValue\tPosition\n" + "="*30)
    min.foreach {
      gd => printf("%s\t%.4f\t",gd.count,gd.value)
        println(format(gd.pos,"%.4f"))
    }
    printf("Expecting value 0 at position (0"+",0"*dim+")")


  }

}