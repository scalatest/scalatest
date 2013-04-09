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
    if (testName.equals(testMethod.getName)) 
      annotation.setGroups(Array("org.scalatest.testng.singlemethodrun.methodname"))  
  }
}
