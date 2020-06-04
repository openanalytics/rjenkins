writeLines(
    con = "Jenkinsfile",
    text = rjenkins::pipeline(
        agent(
            kubernetes(
                yaml = paste0("\n", yaml::as.yaml(
                    list(
                        apiVersion = "v1",
                        kind = "Pod",
                        spec = list(
                            containers = c(
                                list(
                                    image = "196229073436.dkr.ecr.eu-west-1.amazonaws.com/oa-infrastructure"))))))
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
                    sh("R CMD build rjenkins")
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
