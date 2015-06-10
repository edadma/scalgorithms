package ca.hyperreal.scalgorithms

import Integers._


class QQ( _n: BigInt, _d: BigInt ) extends OrderedField[QQ]
{
	if (_d == 0)
		throw new Exception( "denominator can't be zero" )
		
	val (n, d) =
		{
		val g = _n gcd _d
		val sign = _d.signum
		
			(_n/g*sign, _d/g*sign)
		}
	
	def isZero = n == 0
	
	def isOne = n == 1 && d == 1
	
	def +( that: QQ ) = QQ( n*that.d + that.n*d, d*that.d )
	
	def *( that: QQ ) = QQ( n*that.n, d*that.d )
	
	def *:( x: Int ) = QQ( n*x, d )
	
	def -( that: QQ ) = this + -that
	
	def /( that: QQ ) = *( that.inv )
	
	def unary_- = QQ( -n, d )
	
	def conj: QQ = this
	
	def inv =
	{
		if (isUnit)
			QQ( d, n )
		else
			throw new Exception( "no inverse" )
	}
	
	override def isUnit = !isZero
	
	override def isUnital = true

	def compare( that: QQ ) = (n*that.d).compare( that.n*d )
	
	def isPositive = this > 0
	
	def abs = if (n < 0) -this else this
	
	def sqrt = null //MachineFloat( Math.sqrt(a) )
	
	override def toString = if (isZero) "0" else if (d == 1) n.toString else n + "/" + d
}

object QQ
{
//	def apply( a: Int ) = new QQ( BigInt(a), 1 )
	
	def apply( a: BigInt ) = new QQ( a, 1 )
	
	def apply( n: BigInt, d: BigInt ) = new QQ( n, d )
	
	implicit def int2QQ( a: Int ): QQ = new QQ( BigInt(a), 1 )
	
	implicit def int2QQ( a: BigInt ): QQ = new QQ( a, 1 )
}