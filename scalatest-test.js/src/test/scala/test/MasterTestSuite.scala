package test

import org.scalatest.{FunSuite, Suites}

class MasterTestSuite extends Suites(
  new ASuite,
  new BSuite,
  new CSuite
)

protected[test] class ASuite extends FunSuite {
  test("test A1") {
    assert(true == true)
  }
}

protected[test] class BSuite extends FunSuite {
  test("test B1") {
    assert(true == true)
  }
}

protected[test] class CSuite extends FunSuite {
  ignore("test C1") {
    assert(true == true)
  }
}