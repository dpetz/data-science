val myData = """Emil,10
Luka,8
Theo,5
Max,2"""




type Strings = Seq[String]


/** Read full table in csv format (without header) */
def read[T](body:String,rowSep:String="\n",colSep:String=",")(f:Strings=>T):Seq[T]=
  body.split(rowSep) map (_.split(",").toSeq) map (f(_))

case class Kid (name:String, age:Int)

object Kid {
  def apply(r: Seq[String])=new Kid(r(0),r(1).toInt)
}

read(myData){ Kid(_) }

/**

import com.sun.tools.javac.code.TypeTag
import util.ReflectionHelpers

  trait Factory[T] { def load(row:Strings):T }

object Factory {
  def apply[T](implicit ev:Factory[T]) = ev
}

implicit object KidFactory extends Factory[Kid] {
  def load(row:Strings)=Kid(row(0), row(1).toInt)
}

type KidFactory = ReflectionHelpers.CaseClassFactory[Kid]
  **/





