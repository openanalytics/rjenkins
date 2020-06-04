
context("utils")

test_that("groovy string formatting", {
      
      expect_identical(
          formatGString(GString("groovy", multiLine = FALSE, interpolation = FALSE)),
          "'groovy'")
      
      expect_identical(
          formatGString(GString("groovy", multiLine = TRUE, interpolation = FALSE)),
          "'''groovy'''")
      
      expect_identical(
          formatGString(GString("groovy", multiLine = FALSE, interpolation = TRUE)),
          "\"groovy\"")
      
      expect_identical(
          formatGString(GString("groovy", multiLine = TRUE, interpolation = TRUE)),
          "\"\"\"groovy\"\"\"")
      
    })

test_that("groovy string defaults", {
      
      expect_true(attr(GString("multiLine\nstring"), "multiLine"))
      
      expect_true(attr(GString("interpolation${string}"), "interpolation"))
      
    })

test_that("read test file", {
      
      expect_equal(nchar(readTestFile("simple.Jenkinsfile")), 109)
      
    })
