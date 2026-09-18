# Security Policy

ScalaTest, Scalactic, and the ScalaTest+ modules are used as build- and
test-time dependencies across a very large number of Scala projects. We take the
security of that supply chain seriously and appreciate reports from the
community.

## Reporting a Vulnerability

**Please do not report security vulnerabilities through public GitHub issues,
pull requests, or discussions.**

Instead, please report it privately through **GitHub Private Vulnerability
Reporting**: open the
[Security Advisories page](https://github.com/scalatest/scalatest/security/advisories)
for this repository and click **"Report a vulnerability."** This opens a private
channel visible only to the maintainers.

When reporting, please include as much of the following as you can:

- The affected component and version(s) (e.g. `scalatest` 3.2.20, `scalactic`,
  a specific `scalatestplus-*` module).
- A description of the issue and its potential impact.
- Steps to reproduce, a proof of concept, or affected code paths.
- Any suggested remediation, if you have one.

## What to Expect

- We will acknowledge your report as promptly as we can.
- We will investigate, keep you informed of progress, and work with you on
  coordinated disclosure.
- Once a fix is available, we will publish a release and, where appropriate, a
  GitHub Security Advisory crediting you (unless you prefer to remain anonymous).

## Supported Versions

Security fixes are provided for the most recent release line. We recommend always
using the latest published versions of ScalaTest, Scalactic, and the ScalaTest+
modules. Fixes are generally not backported to older, unsupported release lines;
where an issue warrants it, we will note the affected and fixed versions in the
corresponding advisory.

## Scope and Threat Model

ScalaTest is overwhelmingly a build-time and test-time dependency. It typically
runs on developer machines and in CI pipelines rather than in production request
paths. The security concerns most relevant to this project are therefore:

- **Release and supply-chain integrity** — the authenticity and provenance of
  the artifacts we publish to Maven Central.
- **Handling of test-time input** — data that flows through the runner and its
  reporters (for example, test names, assertion and exception messages, and
  event payloads), which is not always fully trusted.

Note that the test runner has capabilities that are powerful by design — such as
loading classes from a user-specified runpath and instantiating user-specified
reporter classes by name. Whoever controls the arguments to a test run can cause
code to be loaded and executed; that is expected behavior for a test runner, not
a vulnerability. Reports that amount to "a user who already controls the test
invocation can run code" are outside the scope of this policy.

Thank you for helping keep ScalaTest and its users secure.
