sbt clean
sbt "project scalactic" clean +publishSigned
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean +publishSigned
sbt "project scalacticMacroNative" clean
sbt "project scalacticNative" clean +publishSigned
sbt "project scalacticDotty" clean publishSigned
sbt "project scalacticDottyJS" clean publishSigned
sbt "project scalacticDottyNative" clean publishSigned
sbt "project scalactic" sonaUpload

sbt clean
sbt scalatestCompatible/clean scalatestCompatible/publishSigned
sbt "project scalatest" clean +publishSigned
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean
sbt "project scalatestJS" clean +publishSigned
sbt "project scalacticMacroNative" clean
sbt "project scalacticNative" clean
sbt "project scalatestNative" clean +publishSigned
sbt "project scalatestDotty" clean publishSigned
sbt "project scalatestDottyJS" clean publishSigned
sbt "project scalatestDottyNative" clean publishSigned
sbt "project scalatest" sonaUpload

sbt clean
sbt "project scalatestApp" clean +publishSigned
sbt "project scalatestAppDotty" clean publishSigned
sbt "project scalatestAppDottyJS" clean publishSigned
sbt "project scalatestAppDottyNative" clean publishSigned
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean
sbt "project scalatestJS" clean
sbt "project scalatestAppJS" clean +publishSigned
sbt "project scalacticMacroNative" clean
sbt "project scalacticNative" clean
sbt "project scalatestNative" clean
sbt "project scalatestAppNative" clean +publishSigned
sbt "project scalatestApp" sonaUpload
