import scala.collection.immutable.List
sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = this match{    
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match{
    case None => None
    case Some(a) => f(a)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](bb: => Option[B]): Option[B] = this match {
    case None => bb
    case Some(a) => this
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if(f(a)) this else None
  }
  override def toString() = this match{
    case None => "None"
    case Some(a) => "Option("+a.toString()+")"
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

case class Employee(name:String,department:String)

object justKill {
   def main(args: Array[String]): Unit = {
     def mean(xs:Seq[Double]): Option[Double]=
       if (xs.isEmpty) None
       else Some(xs.sum / xs.length)
       
     def variance[Double](xs:Seq[Double]): Option[Double] = {
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2)))) 
     }
     
     ￼def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] ={
       a flatMap (aa => b map (bb => f(aa, bb)))
     }
     
     def lift[A,B](f: A=>B): Option[A] => Option[B] = _ map f
     
     val newList = Seq(1.0,3.0,4.0,5.0,6.0,7.0)
     val ab = Some(1)
     val ab2 = None
     println("Option map 1 result: "+ab.map((x:Int) => x * 2))
     println("Option map None result: "+ab2.map((x:Int) => (x*2)))
     println("Option flatMap 1 result: "+ab.flatMap((x:Int) => Some(x * 2)))
     println("Option flatMap None result: "+ab2.flatMap((x:Int) => Some(x*2)))
     
     println("Option getOrElse 1 result: "+ab.getOrElse(30))
     println("Option getOrElse None result: "+ab2.getOrElse(30))
     
     println("Option orElse 1 result: "+ab.orElse(Some(30)))
     println("Option orElse None result: "+ab2.orElse(Some(30)))
     
     println("Option filter 1 result: "+ab.filter((x:Int)=>(x > 0)))
     println("Option filter None result: "+ab2.filter((x:Int)=>(x > 0)))
     
     
     ///More example of using Option
     val employee=scala.collection.immutable.List(Employee("Alice","cs"),Employee("Bob","Math")).map { e => (e.name,e) }
     println("Map list: " + employee)
     println("Map : " + employee.toMap)
     val dept:scala.Option[String] = employee.toMap.get("Alice").map { _.department }
     println("Map dept: "+dept)
     println("Variance of List: " +newList + ".. is "+variance(newList))
     val k:scala.collection.mutable.Map[Int,String] = scala.collection.mutable.Map(1,"1")
    
   }
}
