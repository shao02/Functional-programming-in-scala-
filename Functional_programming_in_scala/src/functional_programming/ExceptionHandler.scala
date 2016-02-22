
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

object justKill {
   def main(args: Array[String]): Unit = {
     def mean(xs:Seq[Double]): Option[Double]=
       if (xs.isEmpty) None
       else Some(xs.sum / xs.length)
       
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
   }
}
