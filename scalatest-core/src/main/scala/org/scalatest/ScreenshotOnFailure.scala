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

import org.scalatest._
import java.io.File

/**
 * Stackable trait that when mixed into a <code>Suite with ScreenshotCapturer</code> ensures a screenshot
 * is captured for every failed test.
 *
 * <p>
 * The screenshot is file is placed in a directory whose name is defined by <code>screenshotDir</code>. By
 * default, <code>screenshotDir</code> returns the value of the <code>java.io.tmpdir</code> system property.
 * To change this, override <code>screenshotDir</code>.
 * </p>
 */
private[scalatest] trait ScreenshotOnFailure extends SuiteMixin { this: Suite with ScreenshotCapturer => 
  
  /**
   * The name of the directory into which screenshots will be captured.
   *
   * <p>
   * By default, this method returns the value of the <code>java.io.tmpdir</code> system property.
   * To change this, override <code>screenshotDir</code>.
   * </p>
   */
  val screenshotDir: String = System.getProperty("java.io.tmpdir")
  
  /**
   * Delegates to <code>super.withFixture</code> to execute the passed <code>NoArgTest</code>, and if the test fails,
   * captures a screenshot to the directory name defined by <code>screenshotDir</code>.
   *
   * <p>
   * This method captures screenshots by invoking <code>captureScreenshot</code>, defined in trait <code>ScreenshotCapturer</code>.
   * If <code>captureScreenshot</code> completes abruptly with an exception, information about that exception, including the full
   * stack trace, is printed to the standard error stream, and the original exception that indicated a failed test is rethrown.
   * </p>
   */
  abstract override def withFixture(test: NoArgTest): Outcome = {
    super.withFixture(test) match {
      case failed: Failed => 
        try captureScreenshot(screenshotDir)
        catch {
          case innerE: Throwable =>
            Console.err.println("Unable to capture screenshot to " + screenshotDir)
            innerE.printStackTrace(Console.err)
        }
        failed
      case other => other
    }
  }
}
