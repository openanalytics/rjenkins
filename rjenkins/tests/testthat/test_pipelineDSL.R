
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

test_that("complex pipeline", {
      
      skip("needs revision")
      
      pipeline(
          agent("any"),
          environment(
              BUILD_IMAGE = 'registry.openanalytics.eu/private/packamon'
          ),
          stages(
              stage('Build',
                  agent(
                      docker(
                          image = 'registry.openanalytics.eu/private/packamon',
                          args = '-v $WORKSPACE:/workspace -w /workspace --env-file $WORKSPACE/stages.list')
                  )
              )
          )
      )
      
    })

