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
package org.scalatest.tags;

/**
 * Annotation used to tag a test, or suite of tests, as slow.
 *
 * <p>
 * <em>Note: This is actually an annotation defined in Java, not a Scala trait.
 * It must be defined in Java instead of Scala so it will be accessible
 * at runtime. It has been inserted into Scaladoc by pretending it is a
 * trait.</em>
 * </p>
 */

import java.lang.annotation.*;
import org.scalatest.TagAnnotation;

@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface Slow {} 
