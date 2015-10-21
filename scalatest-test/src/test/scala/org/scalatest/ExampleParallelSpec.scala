/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest

import org.scalatest.concurrent.SleepHelper

@DoNotDiscover
protected[scalatest] class ExampleParallelSpec extends WordSpec with ParallelTestExecution {

  "Subject 1" should {
    "have behavior 1a" in {
      SleepHelper.sleep(100); succeed
    }
    "have behavior 1b" in {
      info("This is info 1 in 1b")
      info("This is info 2 in 1b")
      SleepHelper.sleep(90); succeed
    }
    "have behavior 1c" in {
      SleepHelper.sleep(80); succeed
    }
  }
  "Subject 2" should {
    "have behavior 2a" in {
      SleepHelper.sleep(70); succeed
    }
    "have behavior 2b" in {
      SleepHelper.sleep(60); succeed
    }
    "have behavior 2c" in {
      SleepHelper.sleep(50); succeed
    }
  }

  //SCALATESTJS-ONLY override def newInstance = new ExampleParallelSpec
}
