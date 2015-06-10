package ca.hyperreal.scalgorithms

import math._


object Matrices
{	
	final class _RaisedToMatrix[R <: Ring[R]]( r: R )
	{
		def *( m: Matrix[R] ): Matrix[R] = m.operation( (i: Int, j: Int) => r*m(i, j) )
	}
	
	implicit def _raiseToMatrix[R <: Ring[R]]( r: R ): _RaisedToMatrix[R] = new _RaisedToMatrix[R]( r )
	
	def independent[R <: Ring[R]]( ms: Matrix[R]* ) = !new DotProductMatrix[R]( ms: _* ).det.isZero
	
//	def gram[F <: Field[F]]( vs: Vector[F]* ) = new GramianMatrix[F]( vs: _* ).det
}