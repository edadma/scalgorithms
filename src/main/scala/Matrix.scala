package ca.hyperreal.scalgorithms

import collection.mutable._
//import math._
import scala.reflect.ClassTag


/**
<code>Matrix</code> is the base class for all matrix types. This abstract class provides a virtually
complete mathematical matrix implementation. Concrete subclasses need on provide an <code>apply( Int, Int ): R</code> method for
reading elements of the matrix. Everything else will work based on that, however, subclasses
are expected to taylor how the results of some of the operations are produced in order to improve
efficiency.

<p>

The
type parameter <code>R</code> can be any type that either extends <code>Ring[R]</code> or for which there is
an implicit conversion (view). This class extends <code>Ring[Matrix[R]]</code> to recognize that fact
that some types of matrices do form a ring. For example the set of nxn square matrices form a ring,
with the set of nxn circulant matrices forming a subring. Many instances of this class cannot be said to
be ring elements, so the fact this class extends <code>Ring[Matrix[R]]</code> is merely
a convenience to allow instances to be treated as such, if it makes sense mathematically to do so.

<p>

This class is also <code>Iterable</code. An iterator will return the matrix elements from row 1 to row
<code>rows</code>, and from column 1 to column <code>cols</code>. It is intended that this feature be used in
conjunction with the various "view" methods.
<p>

<p>
@author Edward A. Maxedon
*/
abstract class Matrix[R <: Ring[R]] extends ((Int, Int) => R)	with (Matrix[R] => Matrix[R]) with Iterable[R] with Ring[Matrix[R]]
{
	/** The number of rows in the matrix */
	val rows: Int
	
	/** The number of columns in the matrix */
	val cols: Int
	
	if (rows <= 0 || cols <= 0)
		sys.error( "Matrix: number of rows and columns must be positive" )
		
	/**
	Returns a matrix containing the result of some operation on <tt>this</tt> matrix, where the value
	of each element of the resulting matrix is given by a function <tt>f: (Int, Int) => R</tt>, which
	assigns that value to the corresponding (row, column) coordinates
	*/
	def operation( f: (Int, Int) => R ): Matrix[R] = operation( rows, cols, f )
	
	def operation( rows: Int, cols: Int, f: (Int, Int) => R ): Matrix[R] = aspect( rows, cols, f )
	
	def +( that: Matrix[R] ) =
	{
		if (rows != that.rows || cols != that.cols)
			sys.error( "+: matrices cannot be added" )
			
		operation( (i, j) => this(i, j) + that(i, j) )
	}

	def *( that: Matrix[R] ) =
	{
		if (cols != that.rows)
			sys.error( "*: matrices cannot be multiplied" )
			
		operation( rows, that.cols, (i, j) => rowView(i) dot that.columnView(j) )
	}
	
	def apply( that: Matrix[R] ) = this * that
	
	def *:( x: Int ) = operation( x*:this(_, _) )
	
	def *:( x: R ) = operation( x*this(_, _) )

	def scale( r: R ) = operation( r*this(_, _) )
	
	def -( that: Matrix[R] ) = this + -that
	
	def /( that: Matrix[R] ) = *( inv )

	def dot( that: Matrix[R] ) =
	{
		require( (isRow || isColumn) && (that.isRow || that.isColumn), "dot: expected row or column matrices" )
		
	val l = toList
	val r = that.toList
	
		if (l.length != r.length)
			sys.error( "dot: matrices must have the same number of elements" )
		
		(l zip r) map (p => p._1*p._2.conj) reduceLeft (_ + _)
	}
	
	def unary_- = operation( -this(_, _) )
	
	def conj = operation( this(_, _).conj )
	
	def isUnital = true
	
	def isUnit = isInvertible
	
	def isInvertible = det.isUnit
	
	def transpose = operation( (i, j) => this(j, i) )

	def conjTranspose = conj.transpose
	
	def tr = diagonal reduceLeft (_ + _)
	
	def inv = inverse

	def inverse: Matrix[R] = det.inv*:adj

	def minor( row: Int, col: Int ) = dropView( row, col ).det

	def cofactor( row: Int, col: Int ) =
		if ((row + col)%2 == 0)
			minor( row, col )
		else
			-minor( row, col )
		
	def cofactors = operation( cofactor )
	
	def adj = cofactors.transpose
	
	def block( row: Int, col: Int, rows: Int, cols: Int ) =
		operation( rows, cols, (i, j) => this(row - 1 + i, col - 1 + j) )

	def dropFunc( row: Int, col: Int, i: Int, j: Int ) = this( if (i >= row) i + 1 else i, if (j >= col) j + 1 else j )
	
	def drop( row: Int, col: Int ) =
	{
		if (isRow || isColumn)
			sys.error( "drop: can't drop from a row or column matrix" )
		
		operation( rows - 1, cols - 1, dropFunc(row, col, _, _) )
	}

	def det: R =
	{
		if (!isSquare)
			sys.error( "det: need square matrix" )
			
		rows match
		{
			case 1 => this( 1, 1 )
			case 2 => this( 1, 1 )*this( 2, 2 ) - this( 1, 2 )*this( 2, 1 )
			case _ => (for (i <- 1 to rows) yield this( i, 1 )*cofactor( i, 1 )) reduceLeft (_ + _)
		}
	}
	
	def iterator: Iterator[R] =
	{
		(for (i <- 1 to rows; j <- 1 to cols) yield this( i, j )).iterator
	}
	
	def copyToArray( it: Iterator[R], array: Array[Array[R]] )
	{
		for (i <- 0 until rows; j <- 0 until cols)
			array(i)(j) = it.next
	}
	
	def toArray( implicit tag: ClassTag[R] ) =
	{
	val res = Array.ofDim[R]( rows, cols )
	
		copyToArray( iterator, res )
		
		res
	}
	
	protected def swap( array: Array[Array[R]], r1: Int, r2: Int )	// just swap the array elements
	{
	val t = array(r1)
	
		array(r1) = array(r2)
		array(r2) = t
		
//	val rs = array.length
//	val cs = array(0).length
//	
//		for (j <- 0 until cs)
//		{
//		val t = array(r1)(j)
//			
//			array(r1)(j) = array(r2)(j)
//			array(r2)(j) = t
//		}
	}
	
	def rref( implicit tag: ClassTag[R] ) =
	{
	val a = toArray
	
		for (i <- 0 until math.min( rows, cols ))
		{
			if (a(i)(i).isZero)
			{
			val nz = for (j <- i+1 until rows; if !a(j)(i).isZero) yield j
			
				if (nz.isEmpty)
					sys.error( "can't be put into row echelon form" )
			
			val r = nz.head
			
				swap( a, i, r )
			}
			
		val d = a(i)(i)
		
			for (j <- i until cols)
				a(i)(j) = a(i)(j)/d
			
			for (j <- 0 until rows; if j != i)
			{
			val m = a(j)(i)
			
				for (k <- 0 until cols)
					a(j)(k) = a(j)(k) - a(i)(k)*m
			}
		}
		
		new ConcreteMatrix[R]( a )
	}
	
	def column( col: Int ) = block( 1, col, rows, 1 )
	
	def row( row: Int ) = block( row, 1, 1, cols )

	def diagonal: List[R] =
	{
		if (!isSquare)
			sys.error( "diagonal: need square matrix" )
			
		(for (i <- 1 to rows) yield this( i, i )).toList
	}
	
	def concrete( implicit tag: ClassTag[R] ) = Matrix( rows, cols, this(_, _) )
	
	def isRow = rows == 1
	
	def isColumn = cols == 1

	def isZero = all( (i, j) => this(i, j).isZero )
	
	def isOne = all( (i, j) => if (i == j) this( i, j ).isOne else this( i, j ).isZero )
	
	def isSquare = rows == cols
	
	def isHermitian = this == conjTranspose
	
	def isSkewHermitian = this == -conjTranspose
	
	def isSymmetric = this == transpose
	
	def isSkewSymmetric = this == -transpose
	
	def isOrthogonal = transpose == inv
	
	def isOrthonormal( implicit conv: Int => R ) =
	{
	val d = det
	
		isOrthogonal && (d == conv(1) || d == conv(-1))
	}
	
	def isUnitary = conjTranspose == inv
	
	def isDiagonal = all( (i, j) => i == j || (i != j && this(i, j).isZero) )
	
	def all( p: (Int, Int) => Boolean ): Boolean =
	{
		for (i <- 1 to rows; j <- 1 to cols)
			if (!p( i, j ))
				return false
				
		true
	}
	
	def some( p: (Int, Int) => Boolean ): Boolean =
	{
		for (i <- 1 to rows; j <- 1 to cols)
			if (p( i, j ))
				return true
				
		false
	}
	
	final def aspect( f: (Int, Int) => R ): Matrix[R] = aspect( rows, cols, f )
	
	final def aspect: Matrix[R] = aspect( this )
	
	final def aspect( rows: Int, cols: Int, f: (Int, Int) => R ): Matrix[R] =
	{
		if (rows < 1 || rows > this.rows || cols < 1 || cols > this.cols)
			sys.error( "aspect out of bounds" )
			
		new ViewMatrix[R]( rows, cols, f )
	}

	final def aspect( row: Int, col: Int, rows: Int, cols: Int ): Matrix[R] =
		aspect( rows, cols, (i: Int, j: Int) => this(row - 1 + i, col - 1 + j) )
	
	final def columnView( col: Int ): Matrix[R] = aspect( 1, col, rows, 1 )
	
	final def rowView( row: Int ): Matrix[R] = aspect( row, 1, 1, cols )
	
	final def dropView( row: Int, col: Int ) =
	{
		if (isRow || isColumn)
			sys.error( "drop: can't drop from a row or column matrix" )
		
		aspect( rows - 1, cols - 1, dropFunc(row, col, _, _) )
	}

	override def hashCode =
	{
	var res = 0
	
		foreach {res ^= _.hashCode}
		res
	}
	
	override def equals( a: Any ) =
	{
		if (!a.isInstanceOf[Matrix[R]])
			false
		else
			all( (i, j) => a.asInstanceOf[Matrix[R]](i, j) == this(i, j) )
	}
	
	override def toString =
	{
	val widths = new Array[Int]( cols )
	
		def format( a: Iterable[R] ) = a.toList.zipWithIndex.map( _ match {case (e, i) => ("%" + widths( i ) + "s").format(e)} )

		for (i <- 0 until cols; e <- columnView( i + 1 ))
		{
			widths( i ) = math.max( widths(i), e.toString.length )
		}

		if (rows == 1)
			rowView( 1 ).mkString( "< ", " ", " >" )
		else
		{
		val buf = new StringBuilder
	
			format( rowView(1) ).addString( buf, "/ ", " ", " \\\n" )
			
			for (i <- 2 to rows - 1)
				format( rowView(i) ).addString( buf, "| ", " ", " |\n" )
			
			format( rowView(rows) ).addString( buf, "\\ ", " ", " /" )
			buf.toString
		}		
	}
}

object Matrix
{
	def apply[R <: Ring[R]]( data: List[R]* )( implicit tag: ClassTag[R] ) =
	{
	var m: ConcreteMatrix[R] = null
	var cols = -1
	var i = 0
	
		for (r <- data)
		{
			if (cols == -1)
			{
				cols = r.length
				m = new ConcreteMatrix[R]( data.length, cols )
			}
			else if (cols != r.length)
				sys.error( "row lists must all be the same length" )
				
			if (cols == 0)
				sys.error( "row list cannot be empty" )
				
		var j = 0
		
			for (e <- r)
			{
				m.array(i)(j) = e
				j += 1
			}
			
			i += 1
		}
		
		m
	}
	
	def apply[R <: Ring[R]]( data: List[List[R]] )( implicit tag: ClassTag[R] ): Matrix[R] = Matrix( data: _* )
	
	def apply[R <: Ring[R]]( rows: Int, data: R* )( implicit tag: ClassTag[R] ) = new ConcreteMatrix[R]( rows, data: _* )
	
	def apply( rows: Int, data: Double* ): Matrix[MachineFloat] = Matrix( rows, data.map(MachineFloat(_)): _* )
	
	def apply[R <: Ring[R]]( rows: Int, cols: Int, f: (Int, Int) => R )( implicit tag: ClassTag[R] ) =
	{
	val m = new ConcreteMatrix[R]( rows, cols )
	
		for (i <- 1 to rows; j <- 1 to cols)
			m.array( i - 1 )( j - 1 ) = f( i, j )
			
		m
	}
	
	def scalar[R <: Ring[R]]( size: Int, c: R )( implicit conv: Int => R, tag: ClassTag[R] ) = new ScalarMatrix[R]( size, c ).concrete
	
	def identity[R <: Ring[R]]( size: Int )( implicit conv: Int => R, tag: ClassTag[R] ) = scalar[R]( size, 1 )
	
	def diagonal[R <: Ring[R]]( ds: R* )( implicit conv: Int => R, tag: ClassTag[R] ) = new DiagonalMatrix[R]( ds: _* ).concrete
	
	def batch[R <: Ring[R]]( ms: Matrix[R]* )( implicit tag: ClassTag[R] ) = new BatchMatrix[R]( ms: _* ).concrete
	
	def column[R <: Ring[R]]( rs: R* )( implicit tag: ClassTag[R] ) = Matrix[R]( rs.length, rs: _* )
	
	def row[R <: Ring[R]]( rs: R* )( implicit tag: ClassTag[R] ) = Matrix[R]( 1, rs: _* )
	
	def norm( m: Matrix[MachineFloat] ) =
	{
		require( m.isRow || m.isColumn, "dot: expected row or column matrix" )
		
		math.sqrt( m.toList.map(e => e*e).reduceLeft(_ + _).a )
	}
}

class ScalarMatrix[R <: Ring[R]]( size: Int, c: R )( implicit conv: Int => R ) extends Matrix[R]
{
	val rows = size
	val cols = size
	
	private val z = conv( 0 )
	
	def apply( row: Int, col: Int ) = if (row == col) c else z
}

class DiagonalMatrix[R <: Ring[R]]( ds: R* )( implicit conv: Int => R ) extends Matrix[R]
{
	val rows = ds.length
	val cols = rows
	
	if (ds.length == 0)
		sys.error( "DiagonalMatrix: diagonal must have at least one element" )
	
	private val z = conv( 0 )
		
	def apply( row: Int, col: Int ) = if (row == col) ds( row - 1 ) else z
}

class ConcreteMatrix[R <: Ring[R]]( val rows: Int, val cols: Int )( implicit tag: ClassTag[R] ) extends Matrix[R]
{	
	if (rows < 1 || cols < 1)
		sys.error( "matrix dimensions must be positive" )
	
	protected var _det: Option[R] = None
	protected var _inv: Option[ConcreteMatrix[R]] = None
	private [scalgorithms] val array = Array.ofDim[R]( rows, cols )
	
	def this( rows: Int, data: R* )( implicit tag: ClassTag[R] )
	{
		this( rows, data.length/rows )
		
		if (data.length % rows != 0)
			sys.error( "data length must be a multiple of rows" )
	
		copyToArray( data.iterator, array )
	}
	
	def this( a: Array[Array[R]] )( implicit tag: ClassTag[R] )
	{
		this( a.length, a(0).length )
		
		for (i <- 0 until rows; j <- 0 until cols)
			array(i)(j) = a(i)(j)
	}
	
	def apply( row: Int, col: Int ) = array( row - 1 )( col - 1 )
	
	override def operation( rows: Int, cols: Int, f: (Int, Int) => R ) = Matrix( rows, cols, f )
	
	override def det =
	{
		if (_det == None)
			_det = Some( super.det )
			
		_det.get
	}
	
	override def inverse: Matrix[R] = 
	{
		if (_inv == None)
			_inv = Some( super.inverse.concrete )
		
		_inv.get
	}
}

class ViewMatrix[R <: Ring[R]]( val rows: Int, val cols: Int, view: (Int, Int) => R ) extends Matrix[R]
{
	def apply( row: Int, col: Int ) = view( row, col )
}

class DotProductMatrix[R <: Ring[R]]( ms: Matrix[R]* ) extends ViewMatrix[R]( ms.length, ms.length,
	(i, j) => ms(i - 1) dot ms(j - 1) )

//class GramianMatrix[F <: Field[F]]( vs: Vector[F]* ) extends ViewMatrix[F]( vs.length, vs.length,
//	(i, j) => vs(i - 1) inner vs(j - 1) )

class BatchMatrix[R <: Ring[R]]( ms: Matrix[R]* ) extends Matrix[R]
{
		if (ms.length == 0)
			sys.error( "columns: expected at least one row or column matrix" )
		
	private val rows_ = ms.head.rows
	private val cols_ = ms.head.cols
	
		for (m <- ms)
		{
			if (m.isRow && m.isColumn)
				sys.error( "BatchMatrix: component matrices must have more than one entry" )
			
			if (!m.isRow && !m.isColumn)
				sys.error( "BatchMatrix: expected only row or column matrices" )
				
			if (m.rows != rows_ || m.cols != cols_)
				sys.error( "BatchMatrix: all component matrices must have the same size" )
		}
		
	val rows =
		if (rows_ == 1)
			ms.length
		else
			rows_
	val cols =
		if (cols_ == 1)
			ms.length
		else
			cols_
			
	private val v =
		if (rows_ == 1)
			(r: Int, c: Int) => ms(r - 1)( 1, c )
		else
			(r: Int, c: Int) => ms(c - 1)( r, 1 )
		
	def apply( row: Int, col: Int ) = v( row, col )
}