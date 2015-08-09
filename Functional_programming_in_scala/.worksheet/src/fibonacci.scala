
// fibonacci with tail recursion
object fibonacci {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(168); 
  def fibo(n:Int,pre:Int,prepre:Int):Int={
  	if(n==1) prepre
  	else fibo(n-1,prepre,pre+prepre)
  	pre+prepre
  };System.out.println("""fibo: (n: Int, pre: Int, prepre: Int)Int""");$skip(67); 
  
  def partiall[A,B,C](a:A,b:B,f:(A,B)=>C):B=>C ={
  	f(a,_)
  };System.out.println("""partiall: [A, B, C](a: A, b: B, f: (A, B) => C)B => C""");$skip(75); 

	def curry[A,B,C](f:(A,B)=>C):A=>(B=>C)={
		def f(a:A)={
			f(a,_)
		}
	};System.out.println("""curry: [A, B, C](f: (A, B) => C)A => (B => C)""")}
}
