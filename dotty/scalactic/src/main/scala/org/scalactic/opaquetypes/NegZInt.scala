package org.scalactic.opaquetypes

import org.scalactic.Resources

opaque type NegZInt = Int

object NegZInt {

  def ensuringValid(i: Int): NegZInt = 
    if (i > 0) 
      throw new AssertionError(Resources.invalidNegZInt)
    else i

}