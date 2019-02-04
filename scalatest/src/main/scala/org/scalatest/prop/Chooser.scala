package org.scalatest.prop

/**
  * A simple one-method trait to allow you to pick values within a range.
  *
  * Quite often in test code, you need to pick a specific value for a given type within a
  * range. This typeclass represents that notion. You can think of it as the
  * generalization of functions in [[Randomizer]] such as `chooseChar`, `chooseFloat`
  * or `choosePosInt`.
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
  * of [[Chooser]]. (Note that such types will also requires instances of [[Generator]] and
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
    * the ordering of `from` or `to` -- that it should behave appropriately if `from` is
    * less than `to` semantically.
    *
    * This function should use the provided [[Randomizer]] in making its choice, and
    * should then return the ''next'' [[Randomizer]]. (Which is returned from all
    * functions on [[Randomizer]].)
    *
    * @param from one end of the target range, inclusive
    * @param to the other end of the target range, inclusive
    * @param rnd the [[Randomizer]] to use for choosing a value
    * @return the selected value, and the next [[Randomizer]]
    */
  def choose(from: T, to: T)(rnd: Randomizer): (T, Randomizer)
}

object Chooser {
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

}
