package ca.hyperreal.scalgorithms

import Integers._


class Zn( m: Int )
{
	def apply( a: Int ) = Element( a )
	
	override def toString = "ZZ" + m.toString

	class Element( val a: Int ) extends Ring[Element]
	{
		private var _inv = -1
		
		if (a < 0 || a >= m)
			throw new Exception( "out of range" )
		
		def zero = Element( 0 )
		
		def isZero = this == (0 : Element)
		
		def isOne = this == (1 : Element)
		
		def +( that: Element ) = Element( this.a + that.a )
		
		def *( that: Element ) = Element( this.a * that.a )
		
		def *:( x: Int ) = Element( x*a )
	
		def -( that: Element ) = this + -that
		
		def /( that: Element ) = *( that.inv )
		
		def unary_- = Element( -a )
		
		def conj: Element = this
		
		def inv =
		{
			if (isUnit)
				_inv
			else
				throw new Exception( "no inverse" )
		}
		
		def isUnit =
		{
			if (_inv == -1)
				if (coprime( a, m ))
					_inv = modinv( a, m )
				else
					_inv = 0
					
			_inv > 0
		}
		
		def isUnital = true
	
		override def hashCode = a.hashCode
		
		override def equals( o: Any ) = o.isInstanceOf[Element] && o.asInstanceOf[Element].a == a
		
		override def toString = a.toString
	}
	
	object Element
	{
		def apply( a: Int ): Element =
		{
			if (a < 0)
				Element( -a )
			else if (a >= m)
				Element( a%m )
			else
				new Element( a )
		}

		implicit def int2Element( a: Int ): Element = Element( a )
	}
}

object Zn
{
	def apply( m: Int ) = new Zn( m )
}