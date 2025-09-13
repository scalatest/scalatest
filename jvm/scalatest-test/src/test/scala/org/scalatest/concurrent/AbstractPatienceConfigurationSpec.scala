package org.scalatest.concurrent

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.time.{Milliseconds, Span}

class AbstractPatienceConfigurationSpec extends AnyFunSpec {

  describe("A PatienceConfig instance") {

    // SKIP-SCALATESTJS-START
    it("should be serialized and deserialized") {
      val initialPatienceConfig = PatienceConfig(Span(466, Milliseconds))
      val objectStream = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(objectStream)
      out.writeObject(initialPatienceConfig)
      val in = new ObjectInputStream(new ByteArrayInputStream(objectStream.toByteArray))

      val afterSerializationPatienceConfig = in.readObject()

      afterSerializationPatienceConfig mustBe initialPatienceConfig
      out.close()
      in.close()
    }
    // SKIP-SCALATESTJS-END

  }

}
