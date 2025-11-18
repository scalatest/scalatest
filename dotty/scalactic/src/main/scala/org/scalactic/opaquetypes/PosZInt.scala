package org.scalactic.opaquetypes

import org.scalactic.Resources
import scala.util.{Try, Success, Failure}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}
import scala.compiletime.{ constValueOpt, error }

opaque type PosZInt = Int

object PosZInt {
  
  inline def apply[I <: Int & Singleton](inline i: I): PosZInt =
    inline constValueOpt[I] match {
      case Some(v: Int) =>
        inline if v < 0 then
          error("PosZInt cannot be instantiated with a negative integer literal")
        else
          v.asInstanceOf[PosZInt]
      case None =>
        error("PosZInt.apply requires an integer literal")
    }

  def from(i: Int): Option[PosZInt] =
    if (i >= 0) Some(i) else None

  def ensuringValid(i: Int): PosZInt = 
    if (i < 0) 
      throw new AssertionError(Resources.invalidPosZInt)
    else i

  /**
   * A factory/validation method that produces a <code>PosZInt</code>, wrapped
   * in a <code>Success</code>, given a valid <code>Int</code> value, or if the
   * given <code>Int</code> is invalid, an <code>AssertionError</code>, wrapped
   * in a <code>Failure</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a PosZInt <code>Int</code>, it will return a <code>PosZInt</code>
   * representing that value, wrapped in a <code>Success</code>.
   * Otherwise, the passed <code>Int</code> value is not PosZInt, so this
   * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time.
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if $typeDesc$, return
   *     wrapped in a <code>Success(PosZInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Success(PosZInt)</code>, if it is $typeDesc$, else a <code>Failure(AssertionError)</code>.
   */
   def tryingValid(value: Int): Try[PosZInt] =
     if (value >= 0)
       Success(value)
     else
       Failure(new AssertionError(Resources.invalidPosZInt))

  def isValid(value: Int): Boolean = value >= 0

  /**
   * A validation method that produces a <code>Pass</code>
   * given a valid <code>Int</code> value, or
   * an error value of type <code>E</code> produced by passing the
   * given <em>invalid</em> <code>Int</code> value
   * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a $typeDesc$ <code>Int</code>, it will return a <code>Pass</code>.
   * Otherwise, the passed <code>Int</code> value is $typeDesc$, so this
   * method will return a result of type <code>E</code> obtained by passing
   * the invalid <code>Int</code> value to the given function <code>f</code>,
   * wrapped in a `Fail`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time.
   * </p>
   *
   * @param value the `Int` to validate that it is $typeDesc$.
   * @return a `Pass` if the specified `Int` value is $typeDesc$,
   *   else a `Fail` containing an error value produced by passing the
   *   specified `Int` to the given function `f`.
   */
  def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
    if (isValid(value)) Pass else Fail(f(value))     


  /**
   * A factory/validation method that produces a <code>PosZInt</code>, wrapped
   * in a <code>Good</code>, given a valid <code>Int</code> value, or if the
   * given <code>Int</code> is invalid, an error value of type <code>B</code>
   * produced by passing the given <em>invalid</em> <code>Int</code> value
   * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a PosZInt <code>Int</code>, it will return a <code>PosZInt</code>
   * representing that value, wrapped in a <code>Good</code>.
   * Otherwise, the passed <code>Int</code> value is not PosZInt, so this
   * method will return a result of type <code>B</code> obtained by passing
   * the invalid <code>Int</code> value to the given function <code>f</code>,
   * wrapped in a `Bad`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time.
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if PosZInt, return
   *     wrapped in a <code>Good(PosZInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Good(PosZInt)</code>, if it is PosZInt, else a <code>Bad(f(value))</code>.
   */
  def goodOrElse[B](value: Int)(f: Int => B): PosZInt Or B =
    if (isValid(value)) Good(value) else Bad(f(value))  

  /**
   * A factory/validation method that produces a <code>PosZInt</code>, wrapped
   * in a <code>Right</code>, given a valid <code>Int</code> value, or if the
   * given <code>Int</code> is invalid, an error value of type <code>L</code>
   * produced by passing the given <em>invalid</em> <code>Int</code> value
   * to the given function <code>f</code>, wrapped in a <code>Left</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a PosZInt <code>Int</code>, it will return a <code>PosZInt</code>
   * representing that value, wrapped in a <code>Right</code>.
   * Otherwise, the passed <code>Int</code> value is not PosZInt, so this
   * method will return a result of type <code>L</code> obtained by passing
   * the invalid <code>Int</code> value to the given function <code>f</code>,
   * wrapped in a `Left`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time.
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if PosZInt, return
   *     wrapped in a <code>Right(PosZInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Right(PosZInt)</code>, if it is PosZInt, else a <code>Left(f(value))</code>.
   */
  def rightOrElse[L](value: Int)(f: Int => L): Either[L, PosZInt] =
    if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))  

  /**
   * A factory method that produces a <code>PosZInt</code> given a
   * <code>Int</code> value and a default <code>PosZInt</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosZInt</code> representing that value.
   * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
   * method will return the passed <code>default</code> value.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Int</code> literals at
   * compile time, whereas <code>from</code> inspects
   * <code>Int</code> values at run time.
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return.
   * @param default the <code>PosZInt</code> to return if the passed
   *     <code>Int</code> value is not positive.
   * @return the specified <code>Int</code> value wrapped in a
   *     <code>PosZInt</code>, if it is positive, else the
   *     <code>default</code> <code>PosZInt</code> value.
   */
  def fromOrElse(value: Int, default: => PosZInt): PosZInt =
    if (isValid(value)) value else default 

  /**
   * The largest value representable as a $typeDesc$ <code>Int</code>, which is <code>$typeName$($typeMaxValueNumber$)</code>.
   */
  val MaxValue: PosZInt = Int.MaxValue

  /**
   * The smallest value representable as a $typeDesc$ <code>Int</code>, which is <code>$typeName$($typeMinValueNumber$)</code>.
   */
  val MinValue: PosZInt = 0   
  
  extension (x: PosZInt) {
    def value: Int = x
    def abs: PosZInt = x
  }
  
  given Conversion[PosZInt, Int] with {
    def apply(x: PosZInt): Int = x
  }

  given Ordering[PosZInt] with {
    def compare(x: PosZInt, y: PosZInt): Int = x.compareTo(y)
  }
}