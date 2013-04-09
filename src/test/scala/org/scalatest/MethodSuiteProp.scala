package org.scalatest

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.ShouldMatchers

trait MethodSuiteProp extends FunSuite with MethodSuiteExamples with TableDrivenPropertyChecks with ShouldMatchers with SharedHelpers {

}