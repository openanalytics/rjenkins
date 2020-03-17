pipeline {

    agent any
    
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }

    triggers {
        pollSCM('H/15 * * * *')
    }
    
    parameters {
        string(name: 'NOTIFY_CHANNEL', defaultValue: '@dseynaeve', description: 'Rocket.Chat channel to notify')
    }
    
    environment {
        RDEPOT_HOST = "https://rdepot.openanalytics.eu/api/manager/packages"
        RDEPOT_REPO = "local"
    }
    
    libraries {
        lib("rdepot")
    }
    
    stages {
        stage('Notify') {
            steps {
                rocketSend channel: "${params.NOTIFY_CHANNEL}", message: "Build Started - ${env.JOB_NAME} #${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)", rawMessage: true
            }
        }
        stage('Build') {
            agent {
                docker {
                    image 'registry.openanalytics.eu/private/packamon'
                    reuseNode true
                }
            }
            steps {
                sh 'pwd'
                sh '/usr/local/bin/packamon.sh'
            }
        }
    }
    post {
        always {
            archiveArtifacts artifacts: '*.tar.gz, *.pdf', fingerprint: true
        }
        success {
            rdepotSubmit "${env.RDEPOT_HOST}", "${env.RDEPOT_REPO}", "${env.SCM_CHANGELOG}", 'jenkins-oa'
            rocketSend channel: "${params.NOTIFY_CHANNEL}", message: "Build Successful - ${env.JOB_NAME} #${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)", rawMessage: true
        }
        failure {
            rocketSend channel: "${params.NOTIFY_CHANNEL}", message: "Build Failed - ${env.JOB_NAME} #${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)", rawMessage: true
        }
    }
    
}

