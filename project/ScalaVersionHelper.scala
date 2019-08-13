object ScalaVersionHelper {
    def isStdLibCompat_213(scalaVersion: String): Boolean = {
        scalaVersion.startsWith("2.13") ||
        isDotty(scalaVersion)
    }

    def isDotty(scalaVersion: String): Boolean = {
        scalaVersion.startsWith("3.") ||
        scalaVersion.matches("""0\.(?:1[8-]|2).*""")
    }
}