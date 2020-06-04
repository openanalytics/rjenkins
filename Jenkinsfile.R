writeLines(
    con = "Jenkinsfile",
    text = rjenkins::pipeline(
        agent(
            kubernetes(
                defaultContainer = "r",
                yaml = paste0("\n", yaml::as.yaml(
                    list(
                        apiVersion = "v1",
                        kind = "Pod",
                        spec = list(
                            containers = list(
                                list(
                                    name = "r",
                                    command = list("cat"),
                                    tty = TRUE,
                                    image = "196229073436.dkr.ecr.eu-west-1.amazonaws.com/openanalytics/r-base:latest"))))))
            )
        ),
        options(
            buildDiscarder(logRotator(numToKeepStr = "3"))
        ),
        triggers(
            pollSCM("H/15 * * * *")
        ),
        stages(
            stage("build",
                steps(
                    sh("R CMD build rjenkins --no-build-vignettes"),
                    step("archiveArtifacts",
                        artifacts = "*.tar.gz, *.pdf",
                        fingerprint = TRUE)
                )
            )
        )
    )
)
