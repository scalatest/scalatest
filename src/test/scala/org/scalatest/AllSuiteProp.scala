package org.scalatest

trait AllSuiteProp extends MethodSuiteProp with FunctionSuiteProp {

  override def examples =
    Table(
      "suite",
      suite,
      fixtureSuite,
      spec,
      fixtureSpec, 
      junit3Suite, 
      junitSuite,
      testngSuite, 
      funSuite,
      fixtureFunSuite,
      funSpec,
      fixtureFunSpec,
      featureSpec,
      fixtureFeatureSpec,
      flatSpec,
      fixtureFlatSpec,
      freeSpec,
      fixtureFreeSpec,
      propSpec,
      fixturePropSpec,
      wordSpec,
      fixtureWordSpec, 
      pathFreeSpec, 
      pathFunSpec
    )
  
}