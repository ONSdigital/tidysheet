pipeline {
    agent none
    stages {
        //we might want to do something python like before we run R
        //normally all the files would be in git so we wouldn't be in quotes hell
        stage('Python prework') {
            agent { label "build.python_3.12" }
            steps {
                echo 'Python Container is running'
                sh 'python3 -c \'print("""print("hello from R")""")\' >example.r'
                sh 'cat example.r'
                stash includes: '*.r', name: 'rcode'
            }
        }
        stage('R example') {
            agent { label "build.r_4-4-1" }
            steps {
                echo 'R Container is running'
                unstash 'rcode'
                //the R executable has an alias to make it easy to call
                sh "R -f example.r"
            }
        }
        stage('R test') {
            agent { label "test.r_4-4-1" }
            steps {
                echo 'R Container is runnning'
                unstash 'rcode'
                sh "R -f example.r"
            }
        }
    }
}
