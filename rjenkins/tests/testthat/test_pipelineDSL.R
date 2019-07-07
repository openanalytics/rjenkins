
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

testdata$r_pipeline <- "pipeline {
    agent any
    stages {
        stage('Roxygen') {
            steps {
                sh 'R  -e \\'roxygen2::roxygenize(\"myPackage\")\\''
            }
        }
        stage('Build') {
            steps {
                sh 'R  -e \\'devtools::build(\"myPackage\")\\''
            }
        }
    }
}
"

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
      
      expect_identical(x, testdata$r_pipeline)
      
    })
