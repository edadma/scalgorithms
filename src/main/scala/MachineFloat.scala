package scalgorithms


class MachineFloat( val a: Double ) extends OrderedField[MachineFloat]
{
	def isZero = a == 0

	def isOne = a == 1
	
	def +( that: MachineFloat ) = MachineFloat( a + that.a )
	
	def *( that: MachineFloat ) = MachineFloat( a * that.a )
	
	def *:( x: Int ) = MachineFloat( a*x )
	
	def -( that: MachineFloat ) = MachineFloat( a - that.a )
	
	def /( that: MachineFloat ) = MachineFloat( a/that.a )
	
	def unary_- = MachineFloat( -a )
	
	def conj: MachineFloat = this
	
	def inv =
	{
		if (isUnit)
			MachineFloat( 1/a )
		else
			throw new Exception( "no inverse" )
	}

	def compare( that: MachineFloat ) = a.compare( that.a )
	
	def isPositive = this.a > 0
	
	def abs = if (this.a < 0) -this else this
	
	def sqrt = MachineFloat( Math.sqrt(a) )
	
	override def toString = String.format( "%.3f", new java.lang.Double(a) )
}

object MachineFloat
{
	val ZERO = new MachineFloat( 0 )
	val ONE = new MachineFloat( 1 )
	
	def apply( a: Double ) = new MachineFloat( a )
	
	implicit def int2MachineFloat( a: Int ): MachineFloat = new MachineFloat( a )
	
	implicit def double2MachineFloat( a: Double ): MachineFloat = new MachineFloat( a )
}