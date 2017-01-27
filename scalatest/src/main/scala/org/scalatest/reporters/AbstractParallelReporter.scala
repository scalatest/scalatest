package org.scalatest.reporters

import java.io.OutputStream
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import org.scalatest.ResourcefulReporter
import org.scalatest.events.{Event, RunCompleted}

import scala.collection.mutable

/**
  * ScalaTest reporter which will take events generated from parallel test run and apply them through #applySequentially
  * method to real implementation of this abstract class.
  *
  * * @param filename if optional filename is passed, two files will be created. One with nonsorted events as received
  * and one with sorted events, as they are passed to lover instance. Given filename will have suffix appended. Once
  * only ".txt" and once "Sorted.txt".
  *
  * Created by Ľubomír Varga on 27.1.2017.
  */
abstract class AbstractParallelReporter(filename: Option[String]) extends ResourcefulReporter {
  val events: mutable.MutableList[Event] = mutable.MutableList()
  val rawEventStreamFile: Option[OutputStream] = filename.map(f => Files.newOutputStream(Paths.get(f + ".txt")))
  val charset = Charset.forName("UTF8")

  override def apply(event: Event): Unit = {
    events += event
    rawEventStreamFile.map(f => {
      f.write(event.toString.getBytes(charset))
      f.write("\n".getBytes(charset))
    })

    event match {
      case _: RunCompleted =>
        val sortedFile = filename.map(f => Files.newOutputStream(Paths.get(f + "Sorted.txt")))
        events.sortBy(_.ordinal).foreach(
          e => {
            sortedFile.map(_.write(e.toString.getBytes(charset)))
            sortedFile.map(_.write("\n".getBytes(charset)))
            applySequentially(e)
          }
        )
        events.clear()
        sortedFile.map(_.close())
      case _ => {}
    }
  }

  def applySequentially(event: Event): Unit

  override def dispose(): Unit = rawEventStreamFile.map(_.close())
}
