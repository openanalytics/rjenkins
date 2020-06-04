pipeline {
  agent {
    kubernetes {
      yaml '''
      apiVersion: v1
      kind: Pod
      spec:
        containers:
          image: 196229073436.dkr.ecr.eu-west-1.amazonaws.com/oa-infrastructure
      '''
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
        sh 'R CMD build rjenkins'
      }
    }
  }
  post {
    always {
      archiveArtifacts artifacts: '*.tar.gz, *.pdf', fingerprint: true
    }
  }
}

