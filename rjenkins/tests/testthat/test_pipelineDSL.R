
context("pipeline DSL")

testdata <- list()

testdata$empty_pipeline <- "pipeline {
}
"

test_that("empty pipeline", {
      
      x <- jenkinsPipeline(pipeline())
      
      expect_identical(x, testdata$empty_pipeline)
      
    })

test_that("step", {
      
      expect_identical(
          step("a", foo = 4, "5", bar = 77, foobar = FALSE),
          "a foo: 4, '5', bar: 77, foobar: false\n"
      )
      
    })

testdata$simple_pipeline <- "pipeline {
    agent any
    stages {
        stage('Build') {
            steps {
                echo 'hello'
            }
        }
    }
}
"

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
      
      expect_identical(x, testdata$simple_pipeline)
      
    })

testdata$complex_pipeline <- "pipeline {
    agent any
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    triggers {
        pollSCM('H/15 * * * *')
    }
    stages {
        stage('Build') {
            agent {
                docker {
                    image 'some/image'
                }
            }
            steps {
                sh 'script.sh'
            }
        }
    }
    post {
        always {
            archiveArtifacts artifacts: '*.tar.gz, *.pdf', fingerprint: true
        }
    }
}
"

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
      
      expect_identical(x, testdata$complex_pipeline)
      
    })
