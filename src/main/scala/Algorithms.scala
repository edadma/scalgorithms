package ca.hyperreal.scalgorithms

import BigDecimal.RoundingMode.HALF_UP


object Algorithms
{
	def sqrt( a: BigDecimal ) =
	{
		val a0 = a.setScale( 10, HALF_UP )
		
		def iter( x: BigDecimal ): BigDecimal =
		{
			println( x )
		val y = x - (x*x - a0)/2/x
		
			if (x == y)
				x
			else
				iter( y )
		}
		
		if (a < 0)
			sys.error( "can't take square root of negative number: " + a0 )
		
		iter( BigDecimal(1).setScale(10, HALF_UP) )
	}
}