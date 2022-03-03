# Codecheck
## Supported Platforms
Codecheck is currently supported on Linux platforms (Ubuntu 20.04 LTS).

## Dependencies

* openjdk-11-jdk   https://openjdk.java.net/projects/jdk/11
* git   https://git-scm.com
* ant   https://ant.apache.org
* curl  https://curl.se
* unzip
* sbt   https://www.scala-sbt.org
* docker https://www.docker.com
* gcloud CLI SDK https://cloud.google.com
* AWS CLI https://aws.amazon.com/



Open a terminal and install the dependencies
```
sudo apt install openjdk-11-jdk git ant curl unzip
```
Install sbt for Linux (deb) or [follow the instruction for your environment](https://www.scala-sbt.org/download.html)
```
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt-get update
sudo apt-get install sbt
```
Install docker for Linux or [follow the instruction for your environment](https://docs.docker.com/engine/install/)
```
 sudo apt-get update
 
  sudo apt-get install \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
```
Add Docker’s official GPG key
```
 curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
```
Use the following command to set up the stable repository.
```
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```
Update the apt package index, and install the latest version of Docker Engine and containerd
```
 sudo apt-get update
 sudo apt-get install docker-ce docker-ce-cli containerd.io
```

Install Google Cloud CLI for linux or [follow the instruction for your environment](https://cloud.google.com/sdk/docs/install#linux)

Open a terminal and download Google Cloud SDK
```
curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-373.0.0-linux-x86_64.tar.gz
```

Extract the contents of the file to any location on your file system (preferably your Home directory). To replace an existing installation, remove the existing google-cloud-sdk directory and then extract the archive to the same location.
```
tar -xf google-cloud-sdk-373.0.0-linux-x86.tar.gz
```

Run the script (from the root of the folder you extracted to) using the following command
```
./google-cloud-sdk/install.sh
```
To initialize the gcloud CLI, run gcloud init
```
./google-cloud-sdk/bin/gcloud init
```
Install AWS CLI for linux or [follow the instruction for your environment](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)

Open a terminal and download the AWS CLI installation file 

```
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
```

Unzip the installer
```
unzip awscliv2.zip
```

Run the install program
```
sudo ./aws/install
```
Confirm the installation with the following command
```
aws --version
```
Configure AWS CLI [instruction](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-quickstart.html)
* Access key ID

* Secret access key

* AWS Region

* Output format
```
aws configure
```
## Setup the environment and download the Code
Create a /opt/codecheck directory and a subdirectory ext that you own
```
sudo mkdir -p /opt/codecheck/ext
export ME=$(whoami) ; sudo -E chown $ME /opt/codecheck /opt/codecheck/ext
```

Download the codecheck source code using git to clone the repository
```
git clone https://github.com/cayhorstmann/codecheck2
```

## Build the command-line tool
Install jackson-core, jackson-annotations, and jackson-databind jar files

From the root directory of the repository, go to cli directory and make a lib directory
```
cd cli
mkdir lib
```

Install the jar files in the lib directory
```
cd lib
curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-core/2.6.4/jackson-core-2.6.4.jar
curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.6.4/jackson-annotations-2.6.4.jar
curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.6.4/jackson-databind-2.6.4.jar
```
Install checkstyle, hamcrest-core, and junit jar files

From the root directory of the repository, go to comrun directory and next go to bin directory, then make a lib directory
```
cd comrun/bin
mkdir lib
```
Install the jar files in the lib directory
```
cd lib
curl -LOs https://repo1.maven.org/maven2/com/puppycrawl/tools/checkstyle/8.42/checkstyle-8.42.jar
curl -LOs https://repo1.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar
curl -LOs https://repo1.maven.org/maven2/junit/junit/4.13.2/junit-4.13.2.jar
```
From the root directory of the repository, build the command-line tool
```
ant -f cli/build.xml
```
To verify that it works
```
/opt/codecheck/codecheck -t samples/java/example1
```
## Run the web application locally
From the root directory of the repository, run the play-codecheck server

```
sbt run
```
To verify that it works visit this url and upload a problem
```
http://localhost:9000/assets/uploadProblem.html
```
# Docker Deployment
## Build and run the comrun service Docker container
From the root directory of the repository, build the comrun service Docker container
```
docker build --tag codecheck:1.0-SNAPSHOT comrun
```
From the root directory of the repository, run the comrun service Docker container
```
docker run -p 8080:8080 -it codecheck:1.0-SNAPSHOT
```
To verify that it works
```
/opt/codecheck/codecheck -l samples/java/example1 &
```
## Build and run the play-codecheck Docker container
From the root directory of the repository, build the Docker image
```
sbt docker:publishLocal 
```
From the root directory of the repository, run the Docker container
```
docker run -p 9090:9000 -it --add-host host.docker.internal:host-gateway play-codecheck:1.0-SNAPSHOT
```
To verify that it works visit the url and upload a problem.
```
http://localhost:9090/assets/uploadProblem.html
```

### Shutdown both containers
Open a terminal and shutdown both containers
```
docker container kill $(docker ps -q)
```

# Cloud Deployment
## Create production configuration file 
Generate the application secret and store it at production configuration file
```
echo "play.http.secret.key=\"$(head -c 32 /dev/urandom | base64)\"" > conf/production.conf
```

```Here is a guidline for naming your S3 bucket.


echo "com.horstmann.codecheck.comrun.remote=\"http://host.docker.internal:8080/api/upload\"" >> conf/production.conf
```

Update conf/production.conf


```
play.http.secret.key="see above"
com.horstmann.codecheck.comrun.remote="the URL of the comrun service"
com.horstmann.codecheck.s3.accessKey="your AWS credentials"
com.horstmann.codecheck.s3.secretKey=""
com.horstmann.codecheck.s3bucketsuffix="mybucket.mydomain.com"
com.horstmann.codecheck.s3.region=your AWS region such as "us-west-1"
com.horstmann.codecheck.repo.ext=""
```



## Create AWS S3 Policy
Create a [Amazon S3 Bucket](https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingBucket.html)

One of the option for creating a bucket is by using [AWS CLI](https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3api/create-bucket.html)
```
aws s3api create-bucket \
    --bucket my-bucket \
    --region us-east-1
```

Follow the [guideline for naming](https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html) your S3 bucket.
```
Bucket Name:  mybucket.mydomain.com
AWS Region: US West (N. California) us-west-1
Object Ownership: ACLs disabled
Block Public Access settings for this bucket: Off
Bucket Versioning: Disable
Default encryption: Disable
```

Open a terminal and verify that the bucket was created

```
aws s3 ls s3://<BUCKET-NAME>

aws s3 ls
```

Create a [AWS S3 Policy](https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-policy-language-overview.html)
```
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "Statement1",
            "Effect": "Allow",
            "Principal": "*",
            "Action": "s3:GetObject",
            "Resource": "arn:aws:s3:::mybucket.mydomain.com/*"
        },
        {
            "Effect": "Allow",
            "Principal": "*",
Install sbt for Linux (deb) or [follow the instruction for your environment](https://www.scala-sbt.org/download.html)
            "Action": "s3:*",
            "Resource": "arn:aws:s3:::mybucket.mydomain.com"
        },
        {
            "Effect": "Allow",
            "Principal": "*",
            "Action": "s3:*",
            "Resource": "arn:aws:s3:::mybucket.mydomain.com/*"
        }
    ]
}
```
## Authenticate with Google Cloud Container Registry
Authenticate for Linux or [follow the instruction for your environment](https://cloud.google.com/container-registry/docs/advanced-authentication)

Add the user that you use to run Docker commands to the Docker security group
```
sudo usermod -a -G docker ${USER}
```
Log in to gcloud as the user that will run Docker commands.
```
gcloud auth login
```
Configure Docker with the following command
```
gcloud auth configure-docker
```

Your credentials are saved in your user home directory
```
$HOME/.docker/config.json
```

## Comrun Service Cloud Deployment
The comrun service compiles and runs the student's program.
We would be deploying the comrun service to [Google Cloud](https://cloud.google.com/).


Create a [Google Cloud Run](https://console.cloud.google.com/run?project) project and a define a service for comrun.

After creating a project look for the project id 
```
export PROJECT=your Google project id
```

Deploy the Comrun service
```
docker tag codecheck:1.0-SNAPSHOT gcr.io/$PROJECT/comrun
docker push gcr.io/$PROJECT/comrun

gcloud run deploy comrun \
  --image gcr.io/$PROJECT/comrun \
  --port 8080 \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated \
  --min-instances=1 \
  --max-instances=50 \
  --memory=512Mi \
  --concurrency=40
```


You will get a URL for the service.

To verify that the service is properly deployed, You should get a report that was obtained by sending the compile and run jobs to your remote service.
```
export REMOTE_URL=the URL of the comrun service
cd path to/codecheck2 
/opt/codecheck/codecheck -rt samples/java/example1
```
## Play Server Cloud Deployment
The Play Server is a web application that manages submission and assignments.

Here are the number of responsibilities that the Play Server handle

* Display problems, collect submissions from students, and check them
* Manage problems from instructors
* Manage assignments (consisting of multiple problems)
* Interface with learning management systems through the LTI protocol

You can find a listing of the supported REST services in the app/conf/routes file.

We would be deploying the play server to [Google Cloud](https://cloud.google.com/).

Create a [Google Cloud Run](https://console.cloud.google.com/run?project) project and a define a service for play-codecheck.

After creating a project look for the project id 
```
export PROJECT=your Google project id
```

Deploy the Play Server
```
docker tag play-codecheck:1.0-SNAPSHOT gcr.io/$PROJECT/play-codecheck
docker push gcr.io/$PROJECT/play-codecheck

gcloud run deploy play-codecheck \
  --image gcr.io/$PROJECT/play-codecheck \
  --port 9000 \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated \
  --min-instances=1
```


You will get a URL for the service.

To verify that it works visit this url and upload a problem
```
https://service url/assets/uploadProblem.html
```

## Check if the comrun service is running
To verify that the comrun service is running, visit this url

```
https://service url/api/health
```

## Error cgroups: cgroup mountpoint does not exist: unknown
A temporary fix
```
sudo mkdir /sys/fs/cgroup/systemd
sudo mount -t cgroup -o none,name=systemd cgroup /sys/fs/cgroup/systemd
```
## Unknown Host Exception: host.docker.internal
Add this to production.conf
```
com.horstmann.codecheck.storeLocation=
```

## Deployment failed      
## ERROR: (gcloud.run.deploy) 
Cloud Run error: Container failed to start. Failed to start and then listen on the port defined by the PORT environment variable. Logs for this revision might contain more information.

```
com.horstmann.codecheck.storeLocation=""
```
## System Error: java.util.NoSuchElementException: submissionrun1/_run

## How to upload a test file to AWS S3 bucket using the CLI
To upload a file to S3, you’ll need to provide two arguments (source and destination) to the aws s3 cp command.
```
aws s3 cp test.txt s3://<BUCKET-NAME>
```

## Sample problem to upload 
Save the file as test1235.py
```
    ##SOLUTION
n = int(input())
for i in range(1, n + 1):
  if (i % 5 == 0 and i % 3 == 0):
     print("FizzBuzz")
  elif (i % 5 == 0):
     print("Buzz")
  elif (i % 3 == 0):
     print("Fizz")
  else:
     print(i)
```
