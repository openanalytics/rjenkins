
context("utils")

test_that("read test file", {
      
      expect_equal(nchar(readTestFile("simple.Jenkinsfile")), 109)
      
    })

test_that("line operations", {
      
      expect_equal(endLines("x"), "x\n")
      
      expect_equal(indentLines("x"), "  x")
      
      expect_equal(endLines(character()), character())
      
      expect_equal(indentLines(character()), character())
      
      expect_equal(
          endLines(indentLines(LETTERS)),
          indentLines(endLines(LETTERS)))
      
    })
