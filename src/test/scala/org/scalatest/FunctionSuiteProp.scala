package org.scalatest

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.ShouldMatchers

trait FunctionSuiteProp extends FunSuite with FunctionSuiteExamples with TableDrivenPropertyChecks with ShouldMatchers with SharedHelpers {

}