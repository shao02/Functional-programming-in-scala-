package functional_programming.gettingstarted

/**
 * @author shao
 */
//Implement partiall function
object Exercise3 {
  def partiall[A,B,C](a:A,f:(A,B)=>C): B => C ={
     f(a,_)
  }
  
  //use case
  def main(args: Array[String]){
    val a = partiall(2,(a:Int,b:Int)=>a*b)
    print(a(6))
  }
}