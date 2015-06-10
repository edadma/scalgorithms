package ca.hyperreal.scalgorithms

import Integers._


trait Group[T]
{
	def *( that: T ): T
	
	def ^( a: Int ): T
	
	def isOne: Boolean
	
//	def isUnit: Boolean
//	
//	def isUnital: Boolean
	
	def /( that: T ): T
	
	def inv: T
}

trait AdditiveGroup[T]
{
	def +( that: T ): T
	
	def unary_- : T
	
	def -( that: T ): T
	
	def *:( a: Int ): T
	
	def isZero: Boolean
}