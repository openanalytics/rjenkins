pipeline {
  agent any
  stages {
    stage('Roxygen') {
      steps {
        sh 'R  -e \'roxygen2::roxygenize("myPackage")\''
      }
    }
    stage('Build') {
      steps {
        sh 'R  -e \'devtools::build("myPackage")\''
      }
    }
  }
}
