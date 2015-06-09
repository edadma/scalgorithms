package scalgorithms

import scalgorithms.Integers._


class CC[R <: OrderedRing[R]]( val a: R, val b: R ) extends Field[CC[R]]
{	
	def isZero = a.isZero && b.isZero

	def isOne = a.isOne && b.isZero
	
	def +( that: CC[R] ) = CC[R]( a + that.a, b*that.b )
	
	def *( that: CC[R] ) = CC[R]( a*that.a - b*that.b, a*that.b + b*that.a )
	
	def *:( x: Int ) = this //CC[R]( a*x, b*x )
	
	def -( that: CC[R] ) = this + -that
	
	def /( that: CC[R] ) = *( that.inv )
	
	def unary_- = CC[R]( -a, -b )
	
	def conj: CC[R] = CC[R]( a, -b )
	
	def inv =
	{
		if (isUnit)
		{
		val n = a*a + b*b
		
			CC[R]( a/n, -b/n )
		}
		else
			throw new Exception( "no inverse" )
	}
	
	override def isUnit = (a*a + b*b).isUnit
	
	override def isUnital = true
	
//	def abs = if (n < 0) -this else this
	
	def sqrt = null //MachineFloat( Math.sqrt(a) )
	
	override def toString =
		if (isZero)
			"0"
		else if (a.isZero)
			b.toString + "i"
		else if (b.isZero)
			a.toString
		else if (b.isNegative)
			a.toString + b + "i"
		else if (b.isOne)
			a + "+i"
		else
			a + "+" + b + "i"
}

object CC
{
	def i[R <: OrderedRing[R]]( implicit coerce: Int => R ) = new CC[R]( coerce(0), coerce(1) )
	
	def apply[R <: OrderedRing[R]]( a: R ) = new CC[R]( a, a - a )
	
	def apply[R <: OrderedRing[R]]( a: R, b: R ) = new CC[R]( a, b )
}