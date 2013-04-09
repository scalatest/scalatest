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
package org.scalatest

import org.scalatest.matchers.ShouldMatchers

class ShellSuite extends Suite with ShouldMatchers {

  def testDefaults() {

    // From default values
    org.scalatest.color should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.durations should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.shortstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.fullstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.stats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.nocolor should have (
      'colorPassed (false),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.nodurations should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.nostacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.nostats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
  }

  def testFromColor() {

    org.scalatest.color.color should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.color.durations should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.color.shortstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.color.fullstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.color.stats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.color.nocolor should have (
      'colorPassed (false),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.color.nodurations should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.color.nostacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.color.nostats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
  }

  def testFromDurations() {

    org.scalatest.durations.color should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.durations.durations should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.durations.shortstacks should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.durations.fullstacks should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.durations.stats should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.durations.nocolor should have (
      'colorPassed (false),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.durations.nodurations should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.durations.nostacks should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.durations.nostats should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
  }

  def testFromShortstacks() {

    org.scalatest.shortstacks.color should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.shortstacks.durations should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.shortstacks.shortstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.shortstacks.fullstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.shortstacks.stats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.shortstacks.nocolor should have (
      'colorPassed (false),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.shortstacks.nodurations should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.shortstacks.nostacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.shortstacks.nostats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
  }

  def testFromFullstacks() {

    org.scalatest.fullstacks.color should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.fullstacks.durations should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.fullstacks.shortstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.fullstacks.fullstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.fullstacks.stats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (true)
    )
    org.scalatest.fullstacks.nocolor should have (
      'colorPassed (false),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.fullstacks.nodurations should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
    org.scalatest.fullstacks.nostacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
    org.scalatest.fullstacks.nostats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (false)
    )
  }

  def testFromStats() {

    org.scalatest.stats.color should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.stats.durations should have (
      'colorPassed (true),
      'durationsPassed (true),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.stats.shortstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (true),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.stats.fullstacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (true),
      'statsPassed (true)
    )
    org.scalatest.stats.stats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.stats.nocolor should have (
      'colorPassed (false),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.stats.nodurations should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.stats.nostacks should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (true)
    )
    org.scalatest.stats.nostats should have (
      'colorPassed (true),
      'durationsPassed (false),
      'shortstacksPassed (false),
      'fullstacksPassed (false),
      'statsPassed (false)
    )
  }
}
