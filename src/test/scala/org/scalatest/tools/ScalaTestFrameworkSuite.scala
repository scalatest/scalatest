/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.tools

import org.scalatest.FunSuite
import org.scalatools.testing.Logger
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.exceptions._

@RunWith(classOf[JUnitRunner])
class ScalaTestFrameworkSuite extends FunSuite{

  test("framework name"){
    assert(new ScalaTestFramework().name === "ScalaTest")
  }

  test("tests contains 2 correct test fingerprint, TestFingerprint and AnnotatedFingerprint"){
    val framework = new ScalaTestFramework
    val fingerprints = framework.tests
    assert(fingerprints.size === 2)

    val testFingerprint =
      fingerprints(0).asInstanceOf[org.scalatools.testing.TestFingerprint]

    assert(testFingerprint.isModule === false)
    assert(testFingerprint.superClassName === "org.scalatest.Suite")
    
    val annotatedFingerprint = 
      fingerprints(1).asInstanceOf[org.scalatools.testing.AnnotatedFingerprint]
    
    assert(annotatedFingerprint.isModule() === false)
    assert(annotatedFingerprint.annotationName === "org.scalatest.WrapWith")
  }

  test("creates runner with given arguments"){
    val framework = new ScalaTestFramework

    import framework.ScalaTestRunner

    val loggers: Array[Logger] = Array(new TestLogger)
    val runner = framework.testRunner(Thread.currentThread.getContextClassLoader, loggers).asInstanceOf[ScalaTestRunner]
    assert(runner.testLoader == Thread.currentThread.getContextClassLoader)
    assert(runner.loggers === loggers)
  }

  private def parsePropsAndTags(rawargs:String) = {
    val translator = new FriendlyParamsTranslator()
    translator.parsePropsAndTags(Array(rawargs).filter(!_.equals("")))
  }

  class TestLogger extends Logger{
    def trace(t:Throwable){}
    def error(msg:String){}
    def warn(msg:String){}
    def info(msg:String){}
    def debug(msg:String){}
    def ansiCodesSupported = false
  }
}
