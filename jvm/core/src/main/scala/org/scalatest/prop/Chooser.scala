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

  // SKIP-DOTTY-START
  implicit val charChooser: Chooser[Char] = new Chooser[Char] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given charChooser: Chooser[Char] = new Chooser[Char] {
    def choose(from: Char, to: Char)(rnd: Randomizer) = rnd.chooseChar(from, to)
  }

  // SKIP-DOTTY-START
  implicit val byteChooser: Chooser[Byte] = new Chooser[Byte] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given byteChooser: Chooser[Byte] = new Chooser[Byte] {  
    def choose(from: Byte, to: Byte)(rnd: Randomizer) = rnd.chooseByte(from, to)
  }

  // SKIP-DOTTY-START
  implicit val shortChooser: Chooser[Short] = new Chooser[Short] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given shortChooser: Chooser[Short] = new Chooser[Short] { 
    def choose(from: Short, to: Short)(rnd: Randomizer) = rnd.chooseShort(from, to)
  }

  // SKIP-DOTTY-START
  implicit val intChooser: Chooser[Int] = new Chooser[Int] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given intChooser: Chooser[Int] = new Chooser[Int] {  
    def choose(from: Int, to: Int)(rnd: Randomizer) = rnd.chooseInt(from, to)
  }

  // SKIP-DOTTY-START
  implicit val floatChooser: Chooser[Float] = new Chooser[Float] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given floatChooser: Chooser[Float] = new Chooser[Float] {
    def choose(from: Float, to: Float)(rnd: Randomizer) = rnd.chooseFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posFloatChooser: Chooser[PosFloat] = new Chooser[PosFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posFloatChooser: Chooser[PosFloat] = new Chooser[PosFloat] {  
    def choose(from: PosFloat, to: PosFloat)(rnd: Randomizer) = rnd.choosePosFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posFiniteFloatChooser: Chooser[PosFiniteFloat] = new Chooser[PosFiniteFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posFiniteFloatChooser: Chooser[PosFiniteFloat] = new Chooser[PosFiniteFloat] {  
    def choose(from: PosFiniteFloat, to: PosFiniteFloat)(rnd: Randomizer) = rnd.choosePosFiniteFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posZFloatChooser: Chooser[PosZFloat] = new Chooser[PosZFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posZFloatChooser: Chooser[PosZFloat] = new Chooser[PosZFloat] {  
    def choose(from: PosZFloat, to: PosZFloat)(rnd: Randomizer) = rnd.choosePosZFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posZFiniteFloatChooser: Chooser[PosZFiniteFloat] = new Chooser[PosZFiniteFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posZFiniteFloatChooser: Chooser[PosZFiniteFloat] = new Chooser[PosZFiniteFloat] {  
    def choose(from: PosZFiniteFloat, to: PosZFiniteFloat)(rnd: Randomizer) = rnd.choosePosZFiniteFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val doubleChooser: Chooser[Double] = new Chooser[Double] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given doubleChooser: Chooser[Double] = new Chooser[Double] {
    def choose(from: Double, to: Double)(rnd: Randomizer) = rnd.chooseDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posIntChooser: Chooser[PosInt] = new Chooser[PosInt] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posIntChooser: Chooser[PosInt] = new Chooser[PosInt] {  
    def choose(from: PosInt, to: PosInt)(rnd: Randomizer) = rnd.choosePosInt(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posZIntChooser: Chooser[PosZInt] = new Chooser[PosZInt] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posZIntChooser: Chooser[PosZInt] = new Chooser[PosZInt] {  
    def choose(from: PosZInt, to: PosZInt)(rnd: Randomizer) = rnd.choosePosZInt(from, to)
  }

  // SKIP-DOTTY-START
  implicit val longChooser: Chooser[Long] = new Chooser[Long] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given longChooser: Chooser[Long] = new Chooser[Long] {  
    def choose(from: Long, to: Long)(rnd: Randomizer) = rnd.chooseLong(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posLongChooser: Chooser[PosLong] = new Chooser[PosLong] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posLongChooser: Chooser[PosLong] = new Chooser[PosLong] {  
    def choose(from: PosLong, to: PosLong)(rnd: Randomizer) = rnd.choosePosLong(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posZLongChooser: Chooser[PosZLong] = new Chooser[PosZLong] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posZLongChooser: Chooser[PosZLong] = new Chooser[PosZLong] {  
    def choose(from: PosZLong, to: PosZLong)(rnd: Randomizer) = rnd.choosePosZLong(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posDoubleChooser: Chooser[PosDouble] = new Chooser[PosDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posDoubleChooser: Chooser[PosDouble] = new Chooser[PosDouble] {  
    def choose(from: PosDouble, to: PosDouble)(rnd: Randomizer) = rnd.choosePosDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posFiniteDoubleChooser: Chooser[PosFiniteDouble] = new Chooser[PosFiniteDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posFiniteDoubleChooser: Chooser[PosFiniteDouble] = new Chooser[PosFiniteDouble] {  
    def choose(from: PosFiniteDouble, to: PosFiniteDouble)(rnd: Randomizer) = rnd.choosePosFiniteDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posZDoubleChooser: Chooser[PosZDouble] = new Chooser[PosZDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posZDoubleChooser: Chooser[PosZDouble] = new Chooser[PosZDouble] {  
    def choose(from: PosZDouble, to: PosZDouble)(rnd: Randomizer) = rnd.choosePosZDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val posZFiniteDoubleChooser: Chooser[PosZFiniteDouble] = new Chooser[PosZFiniteDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given posZFiniteDoubleChooser: Chooser[PosZFiniteDouble] = new Chooser[PosZFiniteDouble] {  
    def choose(from: PosZFiniteDouble, to: PosZFiniteDouble)(rnd: Randomizer) = rnd.choosePosZFiniteDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negIntChooser: Chooser[NegInt] = new Chooser[NegInt] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negIntChooser: Chooser[NegInt] = new Chooser[NegInt] {  
    def choose(from: NegInt, to: NegInt)(rnd: Randomizer) = rnd.chooseNegInt(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negLongChooser: Chooser[NegLong] = new Chooser[NegLong] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negLongChooser: Chooser[NegLong] = new Chooser[NegLong] {  
    def choose(from: NegLong, to: NegLong)(rnd: Randomizer) = rnd.chooseNegLong(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negFloatChooser: Chooser[NegFloat] = new Chooser[NegFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negFloatChooser: Chooser[NegFloat] = new Chooser[NegFloat] {  
    def choose(from: NegFloat, to: NegFloat)(rnd: Randomizer) = rnd.chooseNegFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negFiniteFloatChooser: Chooser[NegFiniteFloat] = new Chooser[NegFiniteFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negFiniteFloatChooser: Chooser[NegFiniteFloat] = new Chooser[NegFiniteFloat] {  
    def choose(from: NegFiniteFloat, to: NegFiniteFloat)(rnd: Randomizer) = rnd.chooseNegFiniteFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negDoubleChooser: Chooser[NegDouble] = new Chooser[NegDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negDoubleChooser: Chooser[NegDouble] = new Chooser[NegDouble] {  
    def choose(from: NegDouble, to: NegDouble)(rnd: Randomizer) = rnd.chooseNegDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negFiniteDoubleChooser: Chooser[NegFiniteDouble] = new Chooser[NegFiniteDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negFiniteDoubleChooser: Chooser[NegFiniteDouble] = new Chooser[NegFiniteDouble] {  
    def choose(from: NegFiniteDouble, to: NegFiniteDouble)(rnd: Randomizer) = rnd.chooseNegFiniteDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negZIntChooser: Chooser[NegZInt] = new Chooser[NegZInt] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negZIntChooser: Chooser[NegZInt] = new Chooser[NegZInt] {  
    def choose(from: NegZInt, to: NegZInt)(rnd: Randomizer) = rnd.chooseNegZInt(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negZLongChooser: Chooser[NegZLong] = new Chooser[NegZLong] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negZLongChooser: Chooser[NegZLong] = new Chooser[NegZLong] {  
    def choose(from: NegZLong, to: NegZLong)(rnd: Randomizer) = rnd.chooseNegZLong(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negZFloatChooser: Chooser[NegZFloat] = new Chooser[NegZFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negZFloatChooser: Chooser[NegZFloat] = new Chooser[NegZFloat] {  
    def choose(from: NegZFloat, to: NegZFloat)(rnd: Randomizer) = rnd.chooseNegZFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negZFiniteFloatChooser: Chooser[NegZFiniteFloat] = new Chooser[NegZFiniteFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negZFiniteFloatChooser: Chooser[NegZFiniteFloat] = new Chooser[NegZFiniteFloat] {  
    def choose(from: NegZFiniteFloat, to: NegZFiniteFloat)(rnd: Randomizer) = rnd.chooseNegZFiniteFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negZDoubleChooser: Chooser[NegZDouble] = new Chooser[NegZDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negZDoubleChooser: Chooser[NegZDouble] = new Chooser[NegZDouble] {  
    def choose(from: NegZDouble, to: NegZDouble)(rnd: Randomizer) = rnd.chooseNegZDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val negZFiniteDoubleChooser: Chooser[NegZFiniteDouble] = new Chooser[NegZFiniteDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given negZFiniteDoubleChooser: Chooser[NegZFiniteDouble] = new Chooser[NegZFiniteDouble] {  
    def choose(from: NegZFiniteDouble, to: NegZFiniteDouble)(rnd: Randomizer) = rnd.chooseNegZFiniteDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val nonZeroIntChooser: Chooser[NonZeroInt] = new Chooser[NonZeroInt] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given nonZeroIntChooser: Chooser[NonZeroInt] = new Chooser[NonZeroInt] {  
    def choose(from: NonZeroInt, to: NonZeroInt)(rnd: Randomizer) = rnd.chooseNonZeroInt(from, to)
  }

  // SKIP-DOTTY-START
  implicit val nonZeroLongChooser: Chooser[NonZeroLong] = new Chooser[NonZeroLong] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given nonZeroLongChooser: Chooser[NonZeroLong] = new Chooser[NonZeroLong] {  
    def choose(from: NonZeroLong, to: NonZeroLong)(rnd: Randomizer) = rnd.chooseNonZeroLong(from, to)
  }

  // SKIP-DOTTY-START
  implicit val nonZeroFloatChooser: Chooser[NonZeroFloat] = new Chooser[NonZeroFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given nonZeroFloatChooser: Chooser[NonZeroFloat] = new Chooser[NonZeroFloat] {  
    def choose(from: NonZeroFloat, to: NonZeroFloat)(rnd: Randomizer) = rnd.chooseNonZeroFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val nonZeroFiniteFloatChooser: Chooser[NonZeroFiniteFloat] = new Chooser[NonZeroFiniteFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given nonZeroFiniteFloatChooser: Chooser[NonZeroFiniteFloat] = new Chooser[NonZeroFiniteFloat] {  
    def choose(from: NonZeroFiniteFloat, to: NonZeroFiniteFloat)(rnd: Randomizer) = rnd.chooseNonZeroFiniteFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val nonZeroDoubleChooser: Chooser[NonZeroDouble] = new Chooser[NonZeroDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given nonZeroDoubleChooser: Chooser[NonZeroDouble] = new Chooser[NonZeroDouble] {  
    def choose(from: NonZeroDouble, to: NonZeroDouble)(rnd: Randomizer) = rnd.chooseNonZeroDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val nonZeroFiniteDoubleChooser: Chooser[NonZeroFiniteDouble] = new Chooser[NonZeroFiniteDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given nonZeroFiniteDoubleChooser: Chooser[NonZeroFiniteDouble] = new Chooser[NonZeroFiniteDouble] {  
    def choose(from: NonZeroFiniteDouble, to: NonZeroFiniteDouble)(rnd: Randomizer) = rnd.chooseNonZeroFiniteDouble(from, to)
  }

  // SKIP-DOTTY-START
  implicit val finiteFloatChooser: Chooser[FiniteFloat] = new Chooser[FiniteFloat] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given finiteFloatChooser: Chooser[FiniteFloat] = new Chooser[FiniteFloat] {  
    def choose(from: FiniteFloat, to: FiniteFloat)(rnd: Randomizer) = rnd.chooseFiniteFloat(from, to)
  }

  // SKIP-DOTTY-START
  implicit val finiteDoubleChooser: Chooser[FiniteDouble] = new Chooser[FiniteDouble] {
  // SKIP-DOTTY-END
  //DOTTY-ONLY given finiteDoubleChooser: Chooser[FiniteDouble] = new Chooser[FiniteDouble] {  
    def choose(from: FiniteDouble, to: FiniteDouble)(rnd: Randomizer) = rnd.chooseFiniteDouble(from, to)
  }

}
