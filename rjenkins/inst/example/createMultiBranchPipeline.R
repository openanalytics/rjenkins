
if (interactive()) {
  
  # jc <- jenkinsConnection(...)
  
  if (hasJob(jc, "my-job"))
    deleteJob(getJob(jc, "my-job"))
  
  createMultiBranchPipeline(
      jc,
      name = "my-job",
      branchSource = gitBranchSource(
          remote = "https://path/to/my_repository.git",
          credentialsId = "credentials-to-use"))
  
  createMultiBranchPipeline(jc,
      name = "my-job",
      branchSource = gitHubBranchSource(
          api = "https://api.github.com",
          owner = "OpenAnalytics",
          repository = "rjenkins",
          credentialsId = "oajenkins-github"))
  
}
