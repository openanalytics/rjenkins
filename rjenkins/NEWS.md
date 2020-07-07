## 0.6.0

### Bug Fixes

* problems with `install_jenkins`
* **DSL:** autoconvert any string to groovy string before formatting
* **DSL:** line handling edge cases

### Features

* **DSL:**: tooling to explicitly model and format Groovy Language features
  such as Groovy Strings and Closures
* **DSL:**: parallel. environment, when directives
* **DSL:**: kubernetes plugin support
* **DSL:**: support I() as an escape hatch to write literal groovy code

## 0.5.0

* github branchsource support
* refactored job creation
* partial support for views
* inline R expressions in pipeline DSL

## 0.4.0

* support for scheduling parametrized builds
* faster implementation of hasJob

## 0.3.0

* support for nested jobs (folders, multibranch projects)

## 0.2.0

* pipeline DSL
* create Multibranch pipeline based on template

## 0.1.0

* ability to get the build queue
* convenience install_jenkins function that matches devtools/remotes
  format and allows to install R packages listed as job artifacts
* allow specifying credentials in multiple ways
* S3-based package structure with object for jenkins server/connection and
  an object for job
* various new functions e.g. to request build history
* ability to create and list jobs
* ability to fetch build logs
