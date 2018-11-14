pipeline {

    agent none
    
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
        success {
            archiveArtifacts artifacts: '*.tar.gz, *.pdf', fingerprint: true
        }
    }
    
}

