pipeline {
    agent {
        kubernetes {
            yaml '''
            apiVersion: v1
            kind: Pod
            spec:
              containers:
              - name: r
                command:
                - cat
                tty: yes
                image: openanalytics/r-base:latest
              - name: curl
                command:
                - cat
                tty: yes
                image: byrnedo/alpine-curl
            '''
            defaultContainer 'r'
        }
    }
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    triggers {
        pollSCM('H/15 * * * *')
    }
    stages {
        stage('build') {
            steps {
                container('r') {
                    sh 'R CMD build rjenkins --no-build-vignettes'
                }
            }
            post {
                success {
                    archiveArtifacts artifacts: '*.tar.gz, *.pdf', fingerprint: true
                }
            }
        }
        stage('RDepot') {
            when {
                anyOf {
                    branch 'master'
                    branch 'develop'
                }
            }
            steps {
                container('curl') {
                    rdepotSubmit 'https://rdepot-dev.openanalytics.eu', 'public', "${env.SCM_CHANGELOG}", 'jenkins-rdepot-dev-token'
                }
            }
        }
    }
}

