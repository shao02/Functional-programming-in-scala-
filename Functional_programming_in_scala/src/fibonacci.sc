
// fibonacci with tail recursion
object fibonacci {
  def fibo(n:Int,pre:Int,prepre:Int):Int={
  	if(n==1) prepre
  	else fibo(n-1,prepre,pre+prepre)
  	pre+prepre
  }
  
  def partiall[A,B,C](a:A,b:B,f:(A,B)=>C):B=>C ={
  	f(a,_)
  }

	def curry[A,B,C](f:(A,B)=>C):A=>(B=>C)={
		def f(a:A)={
			f(a,_)
		}
	}
}