package ca.hyperreal.scalgorithms

import math._


object Integers
{
	private def from( n: Int ): Stream[Int] = Stream.cons( n, from(n + 1) )

	private def sieve( s: Stream[Int] ): Stream[Int] = Stream.cons( s.head, sieve(s.tail filter { _ % s.head != 0 }) )

	def primes = sieve( from(2) )

	def gcd( a: Int, b: Int ) =
	{
		def _gcd( _a: Int, _b: Int ): Int =
			if (_b == 0)
				_a
			else
				_gcd( _b, _a%_b )
		
		_gcd( abs(a), abs(b) )
	}
	
	def egcd( a: Int, b: Int ) =
	{
		def _egcd( a: Int, b: Int, s1: Int, s2: Int, t1: Int, t2: Int ): (Int, Int, Int) =
		{
			if (b == 0)
				(a, s2, t2)
			else				
			{
			val q = a/b
			
				_egcd( b, a - q*b, s2 - q*s1, s1, t2 - q*t1, t1 )
			}
		}
		
	val res = _egcd( a, b, 0, 1, 1, 0 )
	
		if (res._1 < 0)
			(-res._1, -res._2, -res._3)
		else
			res
	}
	
	def divides( a: Int, b: Int ) = b%a == 0
	
	def even( a: Int ) = divides( 2, a )
	
	def odd( a: Int ) = !even( a )
	
	def coprime( a: Int, b: Int ) = gcd( a, b ) == 1
	
	def coprime( a: Int, bs: List[Int] ): Boolean = bs forall (coprime(a, _))
	
	def coprime( as: List[Int] ): Boolean =
		coprime( as.head, as.tail ) && (as.tail == Nil || coprime( as.tail ))
	
	def mod( a: Int, m: Int ) =
	{
		if (a >= m)
			a%m
		else if (a < 0)
			m - (-a%m)
		else
			a
	}
	
	def modinv( a: Int, m: Int ) =
	{
	val (g, s, _) = egcd( a, m )
	
		if (g != 1)
			throw new Exception( "modinv: no inverse" )
			
		mod( s, m )
	}
	
	def crt( eqs: (Int, Int)* ) =
	{
		if (eqs.length == 0)
			throw new Exception( "crt: system is empty" )
			
	val mods = eqs map (_._2) toList
	
		if (!coprime( mods ))
			throw new Exception( "crt: the moduli must all be pairwise coprime" )
			
	val M = mods reduceLeft( _*_ )
	var s = 0
	
		for ((ai, mi) <- eqs)
		{
		val Mi = M/mi
	
			s += mod( ai*Mi*modinv(Mi, mi), M )
		}
		
		mod( s, M )
	}
}