sbt scalatestCompatible/clean scalatestCompatible/publishSigned
export SCALAJS_VERSION=0.6.29
sbt "project scalatestApp" clean +publishSigned 
sbt "project scalatestAppJS" clean +publishSigned
export SCALAJS_VERSION=1.0.0-M8
sbt "project scalatestAppJS" clean +publishSigned
sbt ++2.11.12 "project scalatestAppNative" clean publishSigned
sbt scalacticDotty/clean scalacticDotty/publishSigned
sbt scalatestDotty/clean scalatestDotty/publishSigned
sbt sonatypeBundleUpload
