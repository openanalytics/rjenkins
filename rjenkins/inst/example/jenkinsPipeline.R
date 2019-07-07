
if (interactive()) {
  
  x <- jenkinsPipeline(
      pipeline(
          agent("any"),
          stages(
              stage("Build", steps(echo("hello")))
          )
      )
  )
  
  # shorthand
  x <- pipeline(
      agent("any"),
      stages(
          stage("Build", steps(echo("hello")))
      )
  )
  
}
