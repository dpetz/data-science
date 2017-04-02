import com.sun.tools.javac.code.TypeTag
import util.ReflectionHelpers

val myData = """Emil,10
Luka,8
Theo,5
Max,2"""

trait Row{
  def load(row:Seq[String]):Row
}

type Strings = Seq[String]

case class Kid (name:String, age:Int)

trait Factory[T] { def load(row:Strings):T }

object Factory {
  def apply[T](implicit ev:Factory[T]) = ev
}

implicit object KidFactory extends Factory[Kid] {
  def load(row:Strings)=Kid(row(0), row(1).toInt)
}

type KidFactory = ReflectionHelpers.CaseClassFactory[Kid]

def loadRow[R](data:String):R=
   Factory[R]load data.split(",")


def loadTable[R](data:String):Seq[R]=
  data split "\n" map loadRow[R]


loadTable[Kid](myData)