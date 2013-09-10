package org.scalautils

import org.scalatest._
import org.scalautils._

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with TypeCheckedTripleEquals
