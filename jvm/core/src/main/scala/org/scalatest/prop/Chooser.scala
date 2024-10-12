package org.scalatest.prop

import org.scalactic.anyvals._

/**
  * A simple one-method trait to allow you to pick values within a range.
  *
  * Quite often in test code, you need to pick a specific value for a given type within a
  * range. This typeclass represents that notion. You can think of it as the
  * generalization of functions in [[Randomizer]] such as `chooseChar`, `chooseFloat`
  * or `choosePosInt`. It is appropriate to use this typeclass when you are writing
  * a function that needs this notion of "choosing", and works for multiple
  * types.
  *
  * In principle, this typeclass makes sense for any ordered type with a finite
  * number of values. However, it is a bit different from [[scala.math.Ordering]]
  * in that it is specifically built around ScalaTest's [[Randomizer]]. This is
  * not attempting to choose truly random values; it is choosing pseudo-random
  * values via [[Randomizer]], so that the results can be replayed for debugging
  * if necessary. This is very important: all "randomness" in making the choice
  * should come from the provided [[Randomizer]].
  *
  * This typeclass is used as the basis for [[CommonGenerators.between]], so that
  * you can use that function with your own types by creating an implicit instance
  * of [[Chooser]]. (Note that such types will also require instances of [[Generator]] and
  * [[Ordering]].)
  *
  * @tparam T A type to choose a value of.
  */
trait Chooser[T] {
  /**
    * Choose a value in the given range.
    *
    * The results should be inclusive: that is, it should be possible for this function
    * to return `from` or `to`.
    *
    * Implementations of this function should be tolerant about
    * the ordering of `from` or `to` -- and it should behave appropriately if `from` is
    * less than `to` semantically.
    *
    * This function should use the provided [[Randomizer]] in making its choice, and
    * should then return the ''next'' [[Randomizer]]. (Which is returned from all
    * functions on [[Randomizer]].)
    *
    * @param from one endpoint of the target range, inclusive
    * @param to the other endpoint of the target range, inclusive
    * @param rnd the [[Randomizer]] to use for choosing a value
    * @return the selected value, and the next [[Randomizer]]
    */
  def choose(from: T, to: T)(rnd: Randomizer): (T, Randomizer)
}

/**
  * Provides Chooser instances for all major numeric types in the Scala Standard
  * Library and Scalactic.
  *
  * All of the instances provided here are simply shells over functions in
  * [[Randomizer]], but nothing is sacred about that -- your own instances
  * should use that for randomization, but will not usually be direct calls to its
  * built-in "choose" functions.
  */
object Chooser {
  // The order of the following typeclass instances is arbitrary, but matches the order
  // of the declarations in Randomizer.

  implicit val charChooser: Chooser[Char] = new Chooser[Char] {
    def choose(from: Char, to: Char)(rnd: Randomizer) = rnd.chooseChar(from, to)
  }

  implicit val byteChooser: Chooser[Byte] = new Chooser[Byte] {
    def choose(from: Byte, to: Byte)(rnd: Randomizer) = rnd.chooseByte(from, to)
  }

  implicit val shortChooser: Chooser[Short] = new Chooser[Short] {
    def choose(from: Short, to: Short)(rnd: Randomizer) = rnd.chooseShort(from, to)
  }

  implicit val intChooser: Chooser[Int] = new Chooser[Int] {
    def choose(from: Int, to: Int)(rnd: Randomizer) = rnd.chooseInt(from, to)
  }

  implicit val floatChooser: Chooser[Float] = new Chooser[Float] {
    def choose(from: Float, to: Float)(rnd: Randomizer) = rnd.chooseFloat(from, to)
  }

  implicit val posFloatChooser: Chooser[PosFloat] = new Chooser[PosFloat] {
    def choose(from: PosFloat, to: PosFloat)(rnd: Randomizer) = rnd.choosePosFloat(from, to)
  }

  implicit val posFiniteFloatChooser: Chooser[PosFiniteFloat] = new Chooser[PosFiniteFloat] {
    def choose(from: PosFiniteFloat, to: PosFiniteFloat)(rnd: Randomizer) = rnd.choosePosFiniteFloat(from, to)
  }

  implicit val posZFloatChooser: Chooser[PosZFloat] = new Chooser[PosZFloat] {
    def choose(from: PosZFloat, to: PosZFloat)(rnd: Randomizer) = rnd.choosePosZFloat(from, to)
  }

  implicit val posZFiniteFloatChooser: Chooser[PosZFiniteFloat] = new Chooser[PosZFiniteFloat] {
    def choose(from: PosZFiniteFloat, to: PosZFiniteFloat)(rnd: Randomizer) = rnd.choosePosZFiniteFloat(from, to)
  }

  implicit val doubleChooser: Chooser[Double] = new Chooser[Double] {
    def choose(from: Double, to: Double)(rnd: Randomizer) = rnd.chooseDouble(from, to)
  }

  implicit val posIntChooser: Chooser[PosInt] = new Chooser[PosInt] {
    def choose(from: PosInt, to: PosInt)(rnd: Randomizer) = rnd.choosePosInt(from, to)
  }

  implicit val posZIntChooser: Chooser[PosZInt] = new Chooser[PosZInt] {
    def choose(from: PosZInt, to: PosZInt)(rnd: Randomizer) = rnd.choosePosZInt(from, to)
  }

  implicit val longChooser: Chooser[Long] = new Chooser[Long] {
    def choose(from: Long, to: Long)(rnd: Randomizer) = rnd.chooseLong(from, to)
  }

  implicit val posLongChooser: Chooser[PosLong] = new Chooser[PosLong] {
    def choose(from: PosLong, to: PosLong)(rnd: Randomizer) = rnd.choosePosLong(from, to)
  }

  implicit val posZLongChooser: Chooser[PosZLong] = new Chooser[PosZLong] {
    def choose(from: PosZLong, to: PosZLong)(rnd: Randomizer) = rnd.choosePosZLong(from, to)
  }

  implicit val posDoubleChooser: Chooser[PosDouble] = new Chooser[PosDouble] {
    def choose(from: PosDouble, to: PosDouble)(rnd: Randomizer) = rnd.choosePosDouble(from, to)
  }

  implicit val posFiniteDoubleChooser: Chooser[PosFiniteDouble] = new Chooser[PosFiniteDouble] {
    def choose(from: PosFiniteDouble, to: PosFiniteDouble)(rnd: Randomizer) = rnd.choosePosFiniteDouble(from, to)
  }

  implicit val posZDoubleChooser: Chooser[PosZDouble] = new Chooser[PosZDouble] {
    def choose(from: PosZDouble, to: PosZDouble)(rnd: Randomizer) = rnd.choosePosZDouble(from, to)
  }

  implicit val posZFiniteDoubleChooser: Chooser[PosZFiniteDouble] = new Chooser[PosZFiniteDouble] {
    def choose(from: PosZFiniteDouble, to: PosZFiniteDouble)(rnd: Randomizer) = rnd.choosePosZFiniteDouble(from, to)
  }

  implicit val negIntChooser: Chooser[NegInt] = new Chooser[NegInt] {
    def choose(from: NegInt, to: NegInt)(rnd: Randomizer) = rnd.chooseNegInt(from, to)
  }

  implicit val negLongChooser: Chooser[NegLong] = new Chooser[NegLong] {
    def choose(from: NegLong, to: NegLong)(rnd: Randomizer) = rnd.chooseNegLong(from, to)
  }

  implicit val negFloatChooser: Chooser[NegFloat] = new Chooser[NegFloat] {
    def choose(from: NegFloat, to: NegFloat)(rnd: Randomizer) = rnd.chooseNegFloat(from, to)
  }

  implicit val negFiniteFloatChooser: Chooser[NegFiniteFloat] = new Chooser[NegFiniteFloat] {
    def choose(from: NegFiniteFloat, to: NegFiniteFloat)(rnd: Randomizer) = rnd.chooseNegFiniteFloat(from, to)
  }

  implicit val negDoubleChooser: Chooser[NegDouble] = new Chooser[NegDouble] {
    def choose(from: NegDouble, to: NegDouble)(rnd: Randomizer) = rnd.chooseNegDouble(from, to)
  }

  implicit val negFiniteDoubleChooser: Chooser[NegFiniteDouble] = new Chooser[NegFiniteDouble] {
    def choose(from: NegFiniteDouble, to: NegFiniteDouble)(rnd: Randomizer) = rnd.chooseNegFiniteDouble(from, to)
  }

  implicit val negZIntChooser: Chooser[NegZInt] = new Chooser[NegZInt] {
    def choose(from: NegZInt, to: NegZInt)(rnd: Randomizer) = rnd.chooseNegZInt(from, to)
  }

  implicit val negZLongChooser: Chooser[NegZLong] = new Chooser[NegZLong] {
    def choose(from: NegZLong, to: NegZLong)(rnd: Randomizer) = rnd.chooseNegZLong(from, to)
  }

  implicit val negZFloatChooser: Chooser[NegZFloat] = new Chooser[NegZFloat] {
    def choose(from: NegZFloat, to: NegZFloat)(rnd: Randomizer) = rnd.chooseNegZFloat(from, to)
  }

  implicit val negZFiniteFloatChooser: Chooser[NegZFiniteFloat] = new Chooser[NegZFiniteFloat] {
    def choose(from: NegZFiniteFloat, to: NegZFiniteFloat)(rnd: Randomizer) = rnd.chooseNegZFiniteFloat(from, to)
  }

  implicit val negZDoubleChooser: Chooser[NegZDouble] = new Chooser[NegZDouble] {
    def choose(from: NegZDouble, to: NegZDouble)(rnd: Randomizer) = rnd.chooseNegZDouble(from, to)
  }

  implicit val negZFiniteDoubleChooser: Chooser[NegZFiniteDouble] = new Chooser[NegZFiniteDouble] {
    def choose(from: NegZFiniteDouble, to: NegZFiniteDouble)(rnd: Randomizer) = rnd.chooseNegZFiniteDouble(from, to)
  }

  implicit val nonZeroIntChooser: Chooser[NonZeroInt] = new Chooser[NonZeroInt] {
    def choose(from: NonZeroInt, to: NonZeroInt)(rnd: Randomizer) = rnd.chooseNonZeroInt(from, to)
  }

  implicit val nonZeroLongChooser: Chooser[NonZeroLong] = new Chooser[NonZeroLong] {
    def choose(from: NonZeroLong, to: NonZeroLong)(rnd: Randomizer) = rnd.chooseNonZeroLong(from, to)
  }

  implicit val nonZeroFloatChooser: Chooser[NonZeroFloat] = new Chooser[NonZeroFloat] {
    def choose(from: NonZeroFloat, to: NonZeroFloat)(rnd: Randomizer) = rnd.chooseNonZeroFloat(from, to)
  }

  implicit val nonZeroFiniteFloatChooser: Chooser[NonZeroFiniteFloat] = new Chooser[NonZeroFiniteFloat] {
    def choose(from: NonZeroFiniteFloat, to: NonZeroFiniteFloat)(rnd: Randomizer) = rnd.chooseNonZeroFiniteFloat(from, to)
  }

  implicit val nonZeroDoubleChooser: Chooser[NonZeroDouble] = new Chooser[NonZeroDouble] {
    def choose(from: NonZeroDouble, to: NonZeroDouble)(rnd: Randomizer) = rnd.chooseNonZeroDouble(from, to)
  }

  implicit val nonZeroFiniteDoubleChooser: Chooser[NonZeroFiniteDouble] = new Chooser[NonZeroFiniteDouble] {
    def choose(from: NonZeroFiniteDouble, to: NonZeroFiniteDouble)(rnd: Randomizer) = rnd.chooseNonZeroFiniteDouble(from, to)
  }

  implicit val finiteFloatChooser: Chooser[FiniteFloat] = new Chooser[FiniteFloat] {
    def choose(from: FiniteFloat, to: FiniteFloat)(rnd: Randomizer) = rnd.chooseFiniteFloat(from, to)
  }

  implicit val finiteDoubleChooser: Chooser[FiniteDouble] = new Chooser[FiniteDouble] {
    def choose(from: FiniteDouble, to: FiniteDouble)(rnd: Randomizer) = rnd.chooseFiniteDouble(from, to)
  }

}
