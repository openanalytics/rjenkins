pipeline {

    agent any
    
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }

    triggers {
        pollSCM('H/15 * * * *')
    }
    
    environment {
        RDEPOT_CREDENTIALS = credentials('eae5688d-c858-4757-a17f-65b68ca771da')
    }
    
    stages {
        stage('Build') {
            agent {
                docker {
                    image 'registry.openanalytics.eu/private/packamon'
                    args '--env-file $WORKSPACE/stages.list'
                    reuseNode true
                }
            }
            steps {
                sh '/usr/local/bin/packamon.sh'
            }
        }
    }
    post {
    
    
        always {
            archiveArtifacts artifacts: '*.tar.gz, *.pdf', fingerprint: true
        }
        success {
            sh '''#!/bin/bash
set +x

TOKEN=$(printf "$RDEPOT_CREDENTIALS" | base64 -w 0)

curl -X POST \
  "https://rdepot.openanalytics.eu/api/manager/packages" \
  -H 'Accept: application/json' \
  -H "Authorization: Basic $TOKEN" \
  -H 'Content-Type: multipart/form-data' \
  -F 'uploadRequests[0].fileData=@'"$(ls -1 *.tar.gz | head -1 | tr -d '\n')"';type=application/gzip' \
  -F 'uploadRequests[0].repository=OA local' \
  -F 'uploadRequests[0].changes='"$SCM_CHANGELOG" \
  -F 'uploadRequests[0].replace=true'
  
set -x
            '''
        }

    }
    
}

