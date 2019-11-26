sbt scalatestCompatible/clean scalatestCompatible/publishSigned \
scalactic/clean +scalactic/publishSigned \
scalatestModules/clean \
+scalatestModules/publishSigned \
scalatestPom/clean +scalatestPom/publishSigned \
scalatestApp/clean +scalatestApp/publishSigned

export SCALAJS_VERSION=0.6.29
sbt scalacticJS/clean +scalacticJS/publishSigned \
scalatestModulesJS/clean \
+scalatestModulesJS/publishSigned \
scalatestPomJS/clean +scalatestPomJS/publishSigned \
scalatestAppJS/clean +scalatestAppJS/publishSigned

export SCALAJS_VERSION=1.0.0-M8
sbt ++2.11.12 "project scalacticJS" clean publishSigned \
"project scalatestModulesJS" clean publishSigned \
"project scalatestPomJS" clean publishSigned \
"project scalatestAppJS" clean publishSigned

sbt ++2.12.10 "project scalacticJS" clean publishSigned \
"project scalatestModulesJS" clean publishSigned \
"project scalatestPomJS" clean publishSigned \
"project scalatestAppJS" clean publishSigned

sbt ++2.13.1 "project scalacticJS" clean publishSigned \
"project scalatestModulesJS" clean publishSigned \
"project scalatestPomJS" clean publishSigned \
"project scalatestAppJS" clean publishSigned

sbt ++2.11.12 "project scalacticNative" clean publishSigned \
scalatestModulesNative/clean \
scalatestModulesNative/publishSigned \
scalatestPomNative/clean \
scalatestPomNative/publishSigned \
scalatestAppNative/clean \
scalatestAppNative/publishSigned

sbt "project scalacticDotty" clean publishSigned \
scalatestModulesDotty/clean \
scalatestModulesDotty/publishSigned \
scalatestPomDotty/clean \
scalatestPomDotty/publishSigned

sbt sonatypeBundleUpload