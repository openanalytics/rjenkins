pipeline {
  agent any
  stages {
    stage('Roxygen') {
      steps {
        sh 'R -q -e \'roxygen2::roxygenize("myPackage")\''
      }
    }
    stage('Build') {
      steps {
        sh 'R -q -e \'devtools::build("myPackage")\''
      }
    }
  }
}
