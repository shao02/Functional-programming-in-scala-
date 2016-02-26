
case class Employee(name:String,department:String)
object ExceptionHandlerV2 {
  def main(args: Array[String]): Unit = {
    val employeeByName:Map[String,Employee] =
    List(Employee("Alice","M&A"),Employee("Bob","CS")).map(e=>(e.name,e)).toMap
    println(employeeByName)
    
    val depart:String = employeeByName.get("Bob").filter(_.department!="M&A").map(_.name).getOrElse("default department")
    println(depart)
  }
  
  def mean(xs:Seq[Double]): Option[Double] = {
    if(xs.length == 0) None
    else Some(xs.sum / xs.length)
  }
  
  
  ///!!!! return a function expects to take Option[A] and return Option[B]... _ is Option[A]
  def lift[A,B](f:A=>B):Option[A] =>Option[B] = _ map f
  
  //flatMap[B](f:A=> Option[B]):Option[B]
  
  def variance(xs:Seq[Double]) : Option[Double] = {
    mean(xs) flatMap (mn => mean(xs.map(x => math.pow(x - mn,2))))
  }
  
  
  
}