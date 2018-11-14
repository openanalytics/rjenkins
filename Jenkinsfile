pipeline {

    agent any
    
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }

    triggers {
        pollSCM('H/15 * * * *')
    }
    
    stages {
        stage('Build') {
            agent {
                docker {
                    image 'registry.openanalytics.eu/private/packamon'
                    args '--env-file $WORKSPACE/stages.list'
                }
            }
            steps {
                sh '/usr/local/bin/packamon.sh'
            }
        }
    }
    post {
        always {
            archiveArtifacts artifacts: '*.tar.gz', fingerprint: true
        }
    }
    
}

