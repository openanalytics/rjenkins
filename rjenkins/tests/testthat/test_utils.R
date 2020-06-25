
context("utils")

test_that("read test file", {
      
      expect_equal(nchar(readTestFile("simple.Jenkinsfile")), 109)
      
    })
