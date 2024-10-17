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
package org.scalatest

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShellSpec extends AnyFunSpec {

  it("test defaults") {

    // From default values
    org.scalatest.color should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.stats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.nocolor should have (
      Symbol("colorPassed") (false),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.nodurations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.nostacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.nostats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
  }

  it("test from color") {

    org.scalatest.color.color should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.color.durations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.color.shortstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.color.fullstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.color.stats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.color.nocolor should have (
      Symbol("colorPassed") (false),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.color.nodurations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.color.nostacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.color.nostats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
  }

  it("test from durations") {

    org.scalatest.durations.color should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations.durations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations.shortstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations.fullstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations.stats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.durations.nocolor should have (
      Symbol("colorPassed") (false),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations.nodurations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations.nostacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.durations.nostats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
  }

  it("test from shortstacks") {

    org.scalatest.shortstacks.color should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks.durations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks.shortstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks.fullstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks.stats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.shortstacks.nocolor should have (
      Symbol("colorPassed") (false),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks.nodurations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks.nostacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.shortstacks.nostats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
  }

  it("test from full stacks") {

    org.scalatest.fullstacks.color should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks.durations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks.shortstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks.fullstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks.stats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (true)
    )
    org.scalatest.fullstacks.nocolor should have (
      Symbol("colorPassed") (false),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks.nodurations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks.nostacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
    org.scalatest.fullstacks.nostats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (false)
    )
  }

  it("test from stats") {

    org.scalatest.stats.color should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.durations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (true),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.shortstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (true),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.fullstacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (true),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.stats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.nocolor should have (
      Symbol("colorPassed") (false),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.nodurations should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.nostacks should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (true)
    )
    org.scalatest.stats.nostats should have (
      Symbol("colorPassed") (true),
      Symbol("durationsPassed") (false),
      Symbol("shortstacksPassed") (false),
      Symbol("fullstacksPassed") (false),
      Symbol("statsPassed") (false)
    )
  }
}
