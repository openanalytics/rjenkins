
context("groovy")

options(rjenkins.indent = 2)

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

test_that("groovy closure formatting", {
      
      expect_identical(
          formatGroovyClosure(GroovyClosure()),
          "{\n}\n")
      
      expect_identical(
          formatGroovyClosure(GroovyClosure(1)),
          "{\n  1\n}\n")
      
      expect_identical(
          formatGroovyClosure(GroovyClosure(NULL, 1, NULL)),
          "{\n  1\n}\n")
      
      expect_identical(
          formatGroovyClosure(GroovyClosure(1, parameters = "a")),
          "{ a ->\n  1\n}\n")
      
    })
