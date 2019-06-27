package org.scalatest

import scala.concurrent._

import org.scalatest.Succeeded
import org.scalatest.freespec.AsyncFreeSpec

class Repo extends AsyncFreeSpec {

  implicit override val executionContext: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  ('a' to 'z').map { x =>
    x.toString - {
      (1 to 10).map(i => i.toString in Future(Succeeded))
    }
  }
}