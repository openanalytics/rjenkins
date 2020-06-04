
context("pipeline DSL")

options(rjenkins.indent = 2)

test_that("empty pipeline", {
      
      x <- jenkinsPipeline(pipeline())
      
      expect_identical(x, "pipeline {\n}\n")
      
    })

test_that("step", {
      
      expect_identical(
          step("a", foo = 4, "5", bar = 77, foobar = FALSE),
          "a foo: 4, '5', bar: 77, foobar: false\n"
      )
      
    })

test_that("simple pipeline", {
      
      x <- jenkinsPipeline(
          pipeline(
              agent("any"),
              stages(
                  stage("Build",
                      steps(
                          echo("hello")
                      )
                  )
              )
          )
      )
      
      expect_identical(
          trimws(x),
          trimws(readTestFile("simple.Jenkinsfile")))
      
    })

test_that("complex pipeline", {
      
      x <- jenkinsPipeline(
          pipeline(
              agent("any"),
              options(
                  buildDiscarder(logRotator(numToKeepStr = "3"))
              ),
              triggers(
                  pollSCM("H/15 * * * *")
              ),
              stages(
                  stage("Build",
                      agent(
                          docker(
                              image = "some/image"
                          )
                      ),
                      steps(
                          step("sh", "script.sh")
                      )
                  )
              ),
              post(
                  always(
                      step("archiveArtifacts",
                          artifacts = "*.tar.gz, *.pdf",
                          fingerprint = TRUE)
                  )
              )
          )
      )
      
      expect_identical(
          trimws(x),
          trimws(readTestFile("complex.Jenkinsfile")))
      
    })

test_that("pipeline with inline R", {
      
      x <- jenkinsPipeline(
          pipeline(
              agent("any"),
              stages(
                  stage("Roxygen",
                      steps(
                          R(roxygen2::roxygenize('myPackage'))
                      )
                  ),
                  stage("Build",
                      steps(
                          R(devtools::build('myPackage'))
                      )
                  )
              )
          )
      )
      
      expect_identical(
          trimws(x),
          trimws(readTestFile("R.Jenkinsfile")))
      
    })

test_that("kubernetes pipeline", {
      
      expect_identical(
          trimws(jenkinsPipeline(pipeline(
              agent(
                  kubernetes(
                      yaml = GString(
                          multiLine = TRUE,
                          interpolation = TRUE,
                          readTestFile("kubernetesPod.yaml"))
                  )
              ),
              stages(
                  stage("Run maven",
                      steps(
                          container("maven",
                              step("sh", "mvn -version")
                          ),
                          container("busybox",
                              step("sh", "/bin/busybox")
                          )
                      )
                  )
              )
          ))),
          trimws(readTestFile("kubernetes.Jenkinsfile")))
      
    })
