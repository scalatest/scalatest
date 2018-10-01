package org.scalatest

import java.lang.annotation.Annotation

object AnnotatedSuiteToTagNames {
  def apply(theSuite: Suite): Set[String] = {
    val suiteTags = for {
      a <- theSuite.getClass.getAnnotations
      tag <- AnnotationClassToTagName(a.annotationType)
    } yield tag

    suiteTags.toSet
  }
}

object AnnotationClassToTagName {
  def apply(annotationClass: Class[_ <: Annotation]): Option[String] = {
    if (annotationClass.isAnnotationPresent(classOf[TagAnnotation])) {
      val tagAnnotation = annotationClass.getAnnotation(classOf[TagAnnotation])
      val annotationValue = tagAnnotation.value()
      if (annotationValue.isEmpty)
        Some(annotationClass.getName)
      else
        Some(annotationValue)
    } else {
      None
    }
  }
}