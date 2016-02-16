/*
 * Copyright 2001-2016 Artima, Inc.
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

/**
 * ScalaTest's main traits, classes, and other members, including members supporting ScalaTest's DSL for the Scala interpreter.
 */
package object fixture {
  
  @deprecated("fixture.FeatureSpec has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFeatureSpec instead.")
  type FeatureSpec = org.scalatest.fixture.AnyFeatureSpec
  
  @deprecated("fixture.FeatureSpecLike has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFeatureSpecLike instead.")
  type FeatureSpecLike = org.scalatest.fixture.AnyFeatureSpecLike 
  
  @deprecated("fixture.FlatSpec has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFlatSpec instead.")
  type FlatSpec = org.scalatest.fixture.AnyFlatSpec 
  
  @deprecated("fixture.FlatSpecLike has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFlatSpecLike instead.")
  type FlatSpecLike = org.scalatest.fixture.AnyFlatSpecLike 
  
  @deprecated("fixture.FreeSpec has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFreeSpec instead.")
  type FreeSpec = org.scalatest.fixture.AnyFreeSpec  
  
  @deprecated("fixture.FreeSpecLike has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFreeSpecLike instead.")
  type FreeSpecLike = org.scalatest.fixture.AnyFreeSpecLike 
  
  @deprecated("fixture.FunSpec has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFunSpec instead.")
  type FunSpec = org.scalatest.fixture.AnyFunSpec 
  
  @deprecated("fixture.FunSpecLike has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFunSpecLike instead.")
  type FunSpecLike = org.scalatest.fixture.AnyFunSpecLike 
  
  @deprecated("fixture.FunSuite has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFunSuite instead.")
  type FunSuite = org.scalatest.fixture.AnyFunSuite 
  
  @deprecated("fixture.FunSuiteLike has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyFunSuiteLike instead.")
  type FunSuiteLike = org.scalatest.fixture.AnyFunSuiteLike 
  
  @deprecated("fixture.PropSpec has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyPropSpec instead.")
  type PropSpec = org.scalatest.fixture.AnyPropSpec 
  
  @deprecated("fixture.PropSpecLike has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyPropSpecLike instead.")
  type PropSpecLike = org.scalatest.fixture.AnyPropSpecLike 
  
  @deprecated("fixture.WordSpec has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyWordSpec instead.")
  type WordSpec = org.scalatest.fixture.AnyWordSpec 
  
  @deprecated("fixture.WordSpecLike has been deprecated and will be changed to require type Assertion in a future version of ScalaTest. Please use fixture.AnyWordSpecLike instead.")
  type WordSpecLike = org.scalatest.fixture.AnyWordSpecLike 
}
