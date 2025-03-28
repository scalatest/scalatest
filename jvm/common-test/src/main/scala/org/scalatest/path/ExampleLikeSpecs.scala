/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.path

import org.scalatest.DoNotDiscover
import org.scalatest.freespec
import org.scalatest.funspec

class ExampleBaseClass

@DoNotDiscover
protected class ExampleFreeSpecLike extends ExampleBaseClass with freespec.PathAnyFreeSpecLike {
  //SCALATESTJS,NATIVE-ONLY override def newInstance: freespec.PathAnyFreeSpecLike = new ExampleFreeSpecLike
}

@DoNotDiscover
protected class ExampleFunSpecLike extends ExampleBaseClass with funspec.PathAnyFunSpecLike {
  //SCALATESTJS,NATIVE-ONLY override def newInstance: funspec.PathAnyFunSpecLike = new ExampleFunSpecLike
}