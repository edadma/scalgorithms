package ca.hyperreal.scalgorithms


class MachineInt( val a: Long ) extends OrderedRing[MachineInt]
{
	def isZero = a == 0

	def isOne = a == 1
	
	def +( that: MachineInt ) = MachineInt( a + that.a )
	
	def *( that: MachineInt ) = MachineInt( a * that.a )
	
	def *:( x: Int ) = MachineInt( a*x )
	
	def -( that: MachineInt ) = this + -that
	
	def /( that: MachineInt ) = *( that.inv )
	
	def unary_- = MachineInt( -a )
	
	def conj: MachineInt = this
	
	def inv =
	{
		if (isUnit)
			this
		else
			throw new Exception( "no inverse: " + a )
	}
	
	def isUnit = a == 1 || a == -1
	
	def isUnital = true

	def compare( that: MachineInt ) = a.compare( that.a )
				
	def isPositive = this.a > 0
	
	def abs = if (this.a < 0) -this else this
	
	override def toString = a.toString
}

object MachineInt
{
	val ZERO = new MachineInt( 0 )
	val ONE = new MachineInt( 1 )
	
	def apply( a: Long ) = new MachineInt( a )
	
	implicit def int2MachineInt( a: Int ): MachineInt = new MachineInt( a )
}