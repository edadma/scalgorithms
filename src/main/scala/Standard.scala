package ca.hyperreal.scalgorithms


object Standard
{
	final class _RaisedToQQ( a: Int )
	{
		def +( b: QQ ): QQ = QQ( a, 1 ) + b
		def -( b: QQ ): QQ = QQ( a, 1 ) - b
		def *( b: QQ ): QQ = QQ( a, 1 )*b
		def /( b: QQ ): QQ = QQ( a, 1 )/b
		def \( b: Int ): QQ = QQ( a, b )
	}
	
	implicit def _raiseToQQ( a: Int ): _RaisedToQQ = new _RaisedToQQ( a )
	
	final class _RaisedToMachineFloat( a: Double )
	{
		def +( b: MachineFloat ): MachineFloat = MachineFloat( a ) + b
		def -( b: MachineFloat ): MachineFloat = MachineFloat( a ) - b
		def *( b: MachineFloat ): MachineFloat = MachineFloat( a )*b
		def /( b: MachineFloat ): MachineFloat = MachineFloat( a )/b
	}
	
	implicit def _raiseToMachineFloat( a: Int ): _RaisedToMachineFloat = new _RaisedToMachineFloat( a )
	
	implicit def _raiseToMachineFloat( a: Double ): _RaisedToMachineFloat = new _RaisedToMachineFloat( a )
	
	def kroneckerDelta[T]( i: T, j: T )( implicit conv: Int => T ): T = if (i == j) 1 else 0
}