pipeline {

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
            agent any
            archiveArtifacts artifacts: '*.tar.gz', fingerprint: true
        }
    }
    
}

