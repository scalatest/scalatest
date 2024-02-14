package org.scalatest.tools

import org.scalajs.testinterface.TestUtils
import org.scalatest.{RunningSuite, Suite}

object SuiteInstantiationHelper {
  def createRunningSuite(className: String, cl: ClassLoader): RunningSuite = {
    lazy val suite = TestUtils.newInstance(className, cl)(Seq.empty).asInstanceOf[Suite]
    RunningSuite(className, () => suite)
  }
}
