sbt clean
sbt "project scalactic" clean +publishSigned
export SCALAJS_VERSION=0.6.33
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean +publishSigned
export SCALAJS_VERSION=1.1.0
sbt "project scalacticMacroJS" clean
sbt ++2.11.12 "project scalacticJS" clean publishSigned
sbt ++2.12.12 "project scalacticJS" clean publishSigned
sbt ++2.13.3 "project scalacticJS" clean publishSigned
sbt ++2.11.12 "project scalacticNative" clean publishSigned
sbt "project scalacticDotty" clean publishSigned
sbt "project scalactic" sonatypeBundleUpload

sbt clean
sbt scalatestCompatible/clean scalatestCompatible/publishSigned
sbt "project scalatest" clean +publishSigned
export SCALAJS_VERSION=0.6.33
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean
sbt "project scalatestJS" clean +publishSigned
export SCALAJS_VERSION=1.1.0
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean
sbt ++2.11.12 "project scalatestJS" clean publishSigned
sbt ++2.12.12 "project scalatestJS" clean publishSigned
sbt ++2.13.3 "project scalatestJS" clean publishSigned
sbt ++2.11.12 "project scalatestNative" clean publishSigned
sbt "project scalatestDotty" clean publishSigned
sbt "project scalatest" sonatypeBundleUpload

sbt clean
sbt "project scalatestApp" clean +publishSigned
export SCALAJS_VERSION=0.6.33
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean
sbt "project scalatestAppJS" clean +publishSigned
export SCALAJS_VERSION=1.1.0
sbt "project scalacticMacroJS" clean
sbt "project scalacticJS" clean
sbt ++2.11.12 "project scalatestAppJS" clean publishSigned
sbt ++2.12.12 "project scalatestAppJS" clean publishSigned
sbt ++2.13.3 "project scalatestAppJS" clean publishSigned
sbt ++2.11.12 "project scalatestAppNative" clean publishSigned
sbt "project scalatest" sonatypeBundleUpload
