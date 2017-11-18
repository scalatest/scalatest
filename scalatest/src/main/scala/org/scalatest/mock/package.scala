/*
 * Copyright 2001-2015 Artima, Inc.
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
 * Deprecated mock support classes.
 */
package object mock {

  @deprecated("Please use org.scalatest.easymock.EasyMockSugar instead")
  type EasyMockSugar = easymock.EasyMockSugar

  @deprecated("Please use org.scalatest.easymock.EasyMockSugar instead")
  val EasyMockSugar = easymock.EasyMockSugar

  @deprecated("Please use org.scalatest.jmock.JMockCycle instead")
  type JMockCycle = jmock.JMockCycle

  @deprecated("Please use org.scalatest.jmock.JMockCycleFixture instead")
  type JMockCycleFixture = jmock.JMockCycleFixture

  @deprecated("Please use org.scalatest.jmock.JMockExpectations instead")
  type JMockExpectations = jmock.JMockExpectations

  @deprecated("Please use org.scalatest.mockito.MockitoSugar instead")
  type MockitoSugar = mockito.MockitoSugar

  @deprecated("Please use org.scalatest.mockito.MockitoSugar instead")
  val MockitoSugar = mockito.MockitoSugar
}