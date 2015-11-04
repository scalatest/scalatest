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
package org.scalatest.testng

import org.testng.IAnnotationTransformer
import org.testng.annotations.ITestAnnotation
import java.lang.reflect.Method
import java.lang.reflect.Constructor

// Making this private[scalatest] so that it is public to Java, to ensure the reflection thing works. But
// doesn't show up in Scaladoc, and not part of the ScalaTest API. This is part of its implementation.
// Probably might work as private[testng], but not sure and right before the release.
private[scalatest] class SingleTestAnnotationTransformer(testName: String) extends IAnnotationTransformer {
  override def transform( annotation: ITestAnnotation, testClass: java.lang.Class[_], testConstructor: Constructor[_], testMethod: Method) {
    if (testName == testMethod.getName) 
      annotation.setGroups(Array("org.scalatest.testng.singlemethodrun.methodname"))  
  }
}
