/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.concurrent

//import java.util.TimerTask
//import java.util.Timer
import org.scalatest.TimerTask

private[scalatest] class SignalerTimeoutTask(testThread: Thread, interrupt: Signaler) extends TimerTask {
  @volatile var timedOut = false
  @volatile var needToResetInterruptedStatus = false
  override def run(): Unit = {
    timedOut = true
    val beforeIsInterrupted = testThread.isInterrupted()
    interrupt(testThread)
    val afterIsInterrupted = testThread.isInterrupted()
    if(!beforeIsInterrupted && afterIsInterrupted)
      needToResetInterruptedStatus = true
  }
}
