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
              - name: rdepot-cli
                command:
                - cat
                tty: yes
                image: 196229073436.dkr.ecr.eu-west-1.amazonaws.com/oa-infrastructure/rdepot-cli:latest
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
                container('rdepot-cli') {
                    sh 'ls'
                    withCredentials([string(credentialsId: 'jenkins-rdepot-dev-token', variable: 'RDEPOT_TOKEN')]) {
                        sh """
                        rdepot packages submit \
                          -f *.tar.gz \
                          --repo public \
                          --replace false \
                          --host https://rdepot-dev.openanalytics.eu \
                          --token ${env.RDEPOT_TOKEN}
                        """
                    }
                }
            }
        }
    }
}
