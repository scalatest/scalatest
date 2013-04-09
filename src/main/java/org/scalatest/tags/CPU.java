package org.scalatest.tags;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.scalatest.TagAnnotation;

@TagAnnotation("cpu")
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface CPU {}
