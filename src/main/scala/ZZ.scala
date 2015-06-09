package scalgorithms


class ZZ( val a: BigInt ) extends OrderedRing[ZZ]
{
	def isZero = a == 0

	def isOne = a == 1
	
	def +( that: ZZ ) = ZZ( a + that.a )
	
	def *( that: ZZ ) = ZZ( a * that.a )
	
	def *:( x: Int ) = ZZ( a*x )
	
	def -( that: ZZ ) = ZZ( a - that.a )
	
	def /( that: ZZ ) = *( that.inv )
	
	def unary_- = ZZ( -a )
	
	def conj: ZZ = this
	
	def inv =
	{
		if (isUnit)
			this
		else
			throw new Exception( "no inverse" )
	}
	
	def isUnit = a == 1 || a == -1
	
	def isUnital = true

	def compare( that: ZZ ) = a.compare( that.a )
				
	def isPositive = this.a > 0
	
	def abs = if (this.a < 0) -this else this
	
	override def toString = a.toString
}

object ZZ
{
	def apply( a: Int ) = new ZZ( BigInt(a) )
	
	def apply( a: BigInt ) = new ZZ( a )
	
	implicit def int2ZZ( a: Int ): ZZ = new ZZ( BigInt(a) )
}