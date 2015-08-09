package functional_programming.gettingstarted

/**
 * @author shao
 */
//Function to get the nth Fibonacci number
object Exercise1 {
  def fib(n: Int):Int = {
     @annotation.tailrec
     def nFib(n:Int,pre:Int,prepre:Int):Int={
       if(n<0) throw new IllegalArgumentException
       else if(n == 0) pre
       else nFib(n-1,prepre,pre+prepre)   
     }
     nFib(n,0,1)
  }
}