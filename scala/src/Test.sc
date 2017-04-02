import com.sun.tools.javac.code.TypeTag
import util.ReflectionHelpers

val myData = """Emil,10
Luka,8
Theo,5
Max,2"""

type Strings = Seq[String]


/** Parses row into sequence of values */
def readRow(colSep:String="\n")(s:String):Strings=s.split(",")


/** Read full table in csv format (without header) */
def read(body:String,rowSep:String=",",rowReader:String=>Strings=partial(readRow(x,_):Seq[Strings]=
  body.split(rowSep) map rowReader
}

def read(splitRows)



case class Kid (name:String, age:Int)

object Kid {
  def apply(r: Strings)=new Kid(r(0),r(1).toInt)
}


read myData map Kid


  /**

  trait Factory[T] { def load(row:Strings):T }

object Factory {
  def apply[T](implicit ev:Factory[T]) = ev
}

implicit object KidFactory extends Factory[Kid] {
  def load(row:Strings)=Kid(row(0), row(1).toInt)
}

type KidFactory = ReflectionHelpers.CaseClassFactory[Kid]
**/





