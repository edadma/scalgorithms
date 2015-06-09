package scalgorithms

import scalgorithms.Integers._


trait Ring[T] extends AdditiveGroup[T]
{
	def +( that: T ): T
	
	def *( that: T ): T
	
	def *:( a: Int ): T
	
	def unary_- : T
	
	def isZero: Boolean
	
	def isOne: Boolean
	
	def isUnit: Boolean
	
	def isUnital: Boolean
	
	def -( that: T ): T
	
	def /( that: T ): T
	
	def conj: T
	
	def inv: T
}

trait OrderedRing[T] extends Ring[T] with Ordered[T]
{
	def isPositive: Boolean
	
	def isNegative = !isPositive && !isZero
	
	def isNonpositive = !isPositive
	
	def isNonnegative = isZero || isPositive
	
	def abs: T
}

trait Field[T] extends Ring[T]
{
	def isUnit = !isZero
	
	def isUnital = true
	
	def sqrt: T
}

trait OrderedField[T] extends OrderedRing[T] with Field[T]

object Ring
{
	implicit def int2OrderedRing( a: Int ): OrderedRing[Int] =
		new OrderedRing[Int]	
		{
			def isZero = a == 0

			def isOne = a == 1
			
			def +( that: Int ) = a + that
			
			def *( that: Int ) = a*that
			
			def *:( x: Int ) = x*a
			
			def -( that: Int ) = a - that
			
			def /( that: Int ) = a*that.inv
			
			def unary_- = -a
			
			def conj = a
			
			def inv =
			{
				if (isUnit)
					a
				else
					throw new Exception( "no inverse: " + a )
			}
			
			def isUnit = a == 1 || a == -1
			
			def isUnital = true

			def compare( that: Int ) = a - that
						
			def isPositive = a > 0
			
			def abs = Math.abs( a )
			
			override def toString = a.toString
		}
		
	implicit def double2OrderedRing( a: Double ): OrderedRing[Double] =
		new OrderedRing[Double]	
		{
			def isZero = a == 0

			def isOne = a == 1
			
			def +( that: Double ) = a + that
			
			def *( that: Double ) = a*that
			
			def *:( x: Int ) = a*x
			
			def -( that: Double ) = a - that
			
			def /( that: Double ) = a/that
			
			def unary_- = -a
			
			def conj = a
			
			def inv =
			{
				if (isUnit)
					1/a
				else
					throw new Exception( "no inverse" )
			}
			
			def isUnit = a != 0
			
			def isUnital = true

			def compare( that: Double ) = if (a > that) 1 else if (a < that) -1 else 0

			def isPositive = a > 0
			
			def abs = Math.abs( a )
			
			override def toString = a.toString
		}
}