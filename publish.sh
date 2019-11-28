sbt clean scalatestCompatible/clean scalatestCompatible/publishSigned 
sbt +publishSigned

export SCALAJS_VERSION=0.6.31
sbt "project scalatestAppJS" clean +publishSigned
export SCALAJS_VERSION=1.0.0-M8
sbt ++2.11.12 "project scalatestAppJS" clean publishSigned
sbt ++2.12.10 "project scalatestAppJS" clean publishSigned
sbt ++2.13.1 "project scalatestAppJS" clean publishSigned

sbt ++2.11.12 "project scalatestAppNative" clean publishSigned

sbt "project scalacticDotty" clean publishSigned
sbt "project scalatestDotty" clean publishSigned

sbt sonatypeBundleUpload