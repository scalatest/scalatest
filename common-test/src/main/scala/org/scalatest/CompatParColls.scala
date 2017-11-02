package org.scalatest

/**
  * This compatibility workaround is taken from https://github.com/scala/scala-parallel-collections/issues/22
  */
private[org] object CompatParColls {
  val Converters = {
    import Compat._

    {
      import scala.collection.parallel._

      CollectionConverters
    }
  }

  object Compat {
    object CollectionConverters
  }
}