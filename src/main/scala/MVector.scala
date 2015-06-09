package scalgorithms

import scala.reflect.ClassTag


trait MVector[F <: Field[F], M <: AdditiveGroup[M]]
{
	def +( that: M ): M
	
	def *:( a: Int ): M
	
	def *:( a: F ): M
	
	def norm: F
	
//	def inner( that: MVector[F, M] ): MVector[F, M]
	
	def isZero: Boolean
}

//trait Inner[F <: Field[F]] extends MVector[F]
//{
//	type T
//	
//	def inner( that: T ): F
//	
//	def norm = (this inner this).sqrt
//}
//
//object Inner
//{
//	
//}

object MVector
{
	def apply[F <: Field[F]]( rs: F* )( implicit tag: ClassTag[F] ) = new ConcreteMatrix[F]( rs.length, rs: _* )
		with MVector[F, Matrix[F]]
		{
			def norm = (this dot this).sqrt
		}
	
	def apply( rs: Double* ): MVector[MachineFloat, Matrix[MachineFloat]] = MVector[MachineFloat]( rs.map(MachineFloat(_)): _* )
}