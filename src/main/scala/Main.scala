package ca.hyperreal.scalgorithms

import util._
import Matrices._
import Integers._
import Algorithms._
import Standard._


object Main extends App
{
//	val r = new Random
//	
//	def rnd = r.nextInt( 1000 ) - 500

	try
	{
// 	val m1 = Matrix( 2, 1, 2, 3, 4 )
// 	val m2 = Matrix( 2, 5, 6, 7, 8 )
// 	val m3 = Matrix[QQ]( 3, 1, -3, 0, 5, -1, 1, 5, 2, 0, 1, 1, 0 )
// 	val m4 = Matrix( 3, 0, 1, 1, 0, 1, -3, 0, 5, -1, 1, 5, 2 )
// 	val unimodular = Matrix[MachineInt]( 3, 2, 3, 2, 4, 2, 3, 9, 6, 7 )
// 
// 		println( m3.rref )
// 		println( QQ(2).inv *: m3.rref )
// 		println( m4.rref )
// 		println( unimodular.inv )
		println( Matrix.norm(Matrix(2, 3, 4)) )
	}
	
	catch
	{
		case e: Exception =>
			{
			val w = new java.io.StringWriter
			
				e.printStackTrace( new java.io.PrintWriter(w) )
				println( w )
			}
	}
}