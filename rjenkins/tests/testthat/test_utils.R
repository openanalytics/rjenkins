
context("utils")

test_that("groovy string formatting", {
      
      expect_identical(
          formatGString(GString("groovy")),
          "'groovy'")
      
      expect_identical(
          formatGString(GString("groovy", multiLine = TRUE)),
          "'''groovy'''")
      
      expect_identical(
          formatGString(GString("groovy", interpolation = TRUE)),
          "\"groovy\"")
      
      expect_identical(
          formatGString(GString("groovy", multiLine = TRUE, interpolation = TRUE)),
          "\"\"\"groovy\"\"\"")
      
    })
