object ScalaVersionHelper {

  val dotty_series = """(?:3|0\.(?:1[8-9]|[2-9])).*""".r
  val scala_series = """2\.(?:1[3-9]|[2-9]).*""".r

  def isStdLibCompat_213(scalaVersion: String): Boolean = scalaVersion match {
    case scala_series() | dotty_series() => true
    case _                               => false
  }

  def isDotty(scalaVersion: String): Boolean = scalaVersion match {
    case dotty_series() => true
    case _              => false
  }
}