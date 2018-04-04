# Some POC code to download and install a package from jenkins
# 
# Author: Daan Seynaeve
###############################################################################

library(rjenkins)
library(httr)
library(xml2)

library(keyringr)
# https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
# Ubuntu 16.04+
# sudo apt install libsecret-tools
# secret-tool store --label='oa jenkins' jenkins ci
# secret-tool store --label='oa jenkins' jenkins ci user dseynaeve

# create connection

conn <- jenkinsConnection(
    host = "https://ci.openanalytics.eu",
    user = "dseynaeve",
    token = decrypt_gk_pw("jenkins ci user dseynaeve"))

## packamon

# set working directory inside a git repo
job <- createPackamonJob(conn)
install_jenkins(conn, job, listJobArtifacts(conn, job)[1])

# FIXME: need some way to always get the latest version

## general use

# list all jobs

jobs <- listJobs(conn)
jobs[which(grepl("rjenkins", jobs))]

# get a job

job <- getJob(conn, "phenoprint-docker")

# print a build log

cat(getBuildLog(job))

# installing packamon

artifacts <- listJobArtifacts(conn, "rjenkins-master")
install_jenkins(conn, "rjenkins-master", pkg = artifacts[1])

# creating a job

configFile <- system.file("extdata", "packamon-job-config.xml", package = "rjenkins")
stopifnot(file.exists(configFile))
res <- createJob(conn, "test-rjenkins", configFile)
stopifnot(hasJob(conn, "test-rjenkins"))