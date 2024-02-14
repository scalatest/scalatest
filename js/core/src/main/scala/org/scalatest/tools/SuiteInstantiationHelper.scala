package org.scalatest.tools

import org.scalatest.{RunningSuite, Suite}
import scala.scalajs.reflect.Reflect

private[tools] object SuiteInstantiationHelper {
  def createRunningSuite(className: String): RunningSuite = {
    lazy val suite: Suite = Reflect.lookupInstantiatableClass(className).getOrElse(throw new RuntimeException("Cannot load suite class: " + className)).newInstance().asInstanceOf[Suite]
    RunningSuite(className, () => suite)
  }
}
