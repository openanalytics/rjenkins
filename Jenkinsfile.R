packagePath <- "rjenkins"
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
                                    image = "196229073436.dkr.ecr.eu-west-1.amazonaws.com/openanalytics/r-base:latest"),
                                list(
                                    name = "curl",
                                    command = list("cat"),
                                    tty = TRUE,
                                    image = "byrnedo/alpine-curl"))))))
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
                    container("r",
                        sh(sprintf("R CMD build %s --no-build-vignettes", packagePath)),
                        step("archiveArtifacts",
                            artifacts = "*.tar.gz, *.pdf",
                            fingerprint = TRUE))
                )
            ),
            stage("RDepot",
                when(anyOf(branch("master"), branch("develop"))),
                steps(
                    container("curl",
                        step("rdepotSubmit", "https://rdepot-dev.openanalytics.eu", "public", "${env.SCM_CHANGELOG}", 'oa-jenkins')))
            )
        )
    )
)
