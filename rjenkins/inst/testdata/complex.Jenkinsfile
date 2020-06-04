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
          image 'some/image'
        }
      }
      steps {
        sh 'script.sh'
      }
    }
  }
  post {
    always {
      archiveArtifacts artifacts: '*.tar.gz, *.pdf', fingerprint: true
    }
  }
}
