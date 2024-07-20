# CodeCheck<sup>®</sup> Build Instructions

## Program Structure

CodeCheck has two parts:

-   A web application that manages submission and assignments. It is
    called `play-codecheck` because it is based on the Play framework.
    It is convenient, but not necessary, to run it in Docker.
-   A Dockerized service that compiles and runs programs, called
    `comrun`.

The `play-codecheck` program has a number of responsibilities:

-   Display problems, collect submissions from students, and check them
-   Manage problems from instructors
-   Manage assignments (consisting of multiple problems)
-   Interface with learning management systems through the LTI protocol

You can find a listing of the supported REST services in the
`app/conf/routes` file.

The `comrun` service is extremely simple. You can find a basic
description in the `comrun/bin/comrun` script.

For local testing of problems, there is also a handy command-line tool.
This tool uses only the part of `play-codecheck` that deals with
checking a problem (in the `com.horstmann.codecheck` package). The tool
is called `codecheck`. It is created by the `cli/build.xml` Ant script.

## Special Steps for Github Codespaces

Make a new Codespace by cloning the repository `cayhorstmann/codecheck2`

Open a terminal. Run 

```
sudo sed -i -e 's/root/ALL/' /etc/sudoers.d/codespace
sudo cat /etc/sudoers.d/codespace
```

and verify that the contents is

```
codespace ALL=(ALL) NOPASSWD:ALL 
```

## Install Codecheck dependencies

These instructions are for Ubuntu 20.04LTS. If you are not running Ubuntu natively, run it in a virtual machine. If you were asked to use Github Codespaces, that should be set up for you. Otherwise, you need to set up your own virtual machine. These instructions should be helpful: https://horstmann.com/pfh/2021/vm.html

Open a terminal and install the dependencies

```
sudo apt update
sudo apt -y install openjdk-11-jdk git ant curl zip unzip
```

Install sbt for Linux (deb) or [follow the instruction for your environment](https://www.scala-sbt.org/download.html)
```
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt update
sudo apt -y install sbt
```
Building the Command Line Tool
------------------------------

Make a directory `/opt/codecheck` that you own:

    sudo mkdir -p /opt/codecheck
    export ME=$(whoami) ; sudo -E chown $ME /opt/codecheck

Clone the repo (unless you are in Codespaces, where it is already cloned)

    git clone https://github.com/cayhorstmann/codecheck2

Get a few JAR files:

    cd codecheck2 # if not already there
    cd cli
    mkdir lib
    cd lib
    curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-core/2.6.4/jackson-core-2.6.4.jar
    curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.6.4/jackson-annotations-2.6.4.jar
    curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.6.4/jackson-databind-2.6.4.jar
    cd ../../comrun/bin
    mkdir lib
    cd lib
    curl -LOs https://repo1.maven.org/maven2/com/puppycrawl/tools/checkstyle/8.42/checkstyle-8.42.jar
    curl -LOs https://repo1.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar
    curl -LOs https://repo1.maven.org/maven2/junit/junit/4.13.2/junit-4.13.2.jar
    cd ../../..

Build the command-line tool:

    ant -f cli/build.xml

Test that it works:

    /opt/codecheck/codecheck -t samples/java/example1

If you omit the `-t`, you get a report with your default browser instead
of the text report.

## Eclipse

If you work on your own machine, I recommend Eclipse as the IDE. If you use Codespaces, skip this section and read about the Visual Studio Code configuration instead.

Install [Eclipse](https://www.eclipse.org/eclipseide/), following the instructions of the provider.

Run

    sbt eclipse
    sbt compile

Then open Eclipse and import the created project.

Make two debugger configurations. Select Run → Debug Configurations,
right-click on Remote Java Application, and select New Configuration.

For the first configuration, specify:

-   Type: Remote Java Application
-   Name:  Play Server
-   Project: `play-codecheck`
-   Connection type: Standard
-   Host: `localhost`
-   Port: 9999

For the second debug configuration, set:

-   Type: Java Application
-   Name: Command Line Tool
-   Main class:

        com.horstmann.codecheck.Main

-   Program arguments:

        /tmp/submission /tmp/problem

-   VM arguments:

        -Duser.language=en
        -Duser.country=US
        -Dcom.horstmann.codecheck.comrun.local=/opt/codecheck/comrun
        -Dcom.horstmann.codecheck.report=HTML
        -Dcom.horstmann.codecheck.debug

-   Environment variable `COMRUN_USER`: your username

## Codespaces and Visual Studio Code

If you use Codespaces, you need to use Visual Studio Code as your IDE. If not, skip this section and follow the section about configuring Eclipse instead.

Run

    sbt eclipse
    sbt compile

Visual Studio Code will read the project configuration from the Eclipse configuration.

Install the Language Support for Java (from Red Hat) and Debugger for Java (from Microsoft) extensions into Visual Studio Code.

In Visual Studio Code, click on the Run and Debug (triangle and bug) icon on the left. Select Run → Add Configuration from the menu. The file `.vscode/launch.json` is opened up. Set it to the following contents:

```
{
  // Use IntelliSense to learn about possible attributes.
  // Hover to view descriptions of existing attributes.
  // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
  "version": "0.2.0",
  "configurations": [
    {
      "type": "java",
      "name": "Debug (Attach)",
      "request": "attach",
      "hostName": "localhost",
      "port": 9999,
      "projectName": "play-codecheck"
    },
    {
      "type": "java",
      "name": "Launch Main",
      "request": "launch",
      "mainClass": "com.horstmann.codecheck.Main",
      "projectName": "play-codecheck",
      "args": "/tmp/submission /tmp/problem",
      "vmArgs": [
        "-Duser.language=en",
        "-Duser.country=US",
        "-Dcom.horstmann.codecheck.comrun.local=/opt/codecheck/comrun",
        "-Dcom.horstmann.codecheck.report=HTML",
        "-Dcom.horstmann.codecheck.debug"
      ],
      "env": { "COMRUN_USER": "codespace" }
    }
  ]
}
```

Sad Codespaces/Visual Studio Code issue: When you install the Java language pack, the terminal is configured to use the version of Java installed with the language pack. That is *not* what we want. *Every time* you open the terminal, do this:

```
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
export PATH=$JAVA_HOME/bin:$PATH
java -version
```

Be sure that you get Java 11, and not Java 17.

Debugging the Command Line Tool
-------------------------------

If you are making changes to the part of CodeCheck that does the actual
code checking, such as adding a new language, and you need to run a
debugger, it is easiest to debug the command line tool.

Make directories for the submission and problem files, and populate them
with samples. For example,

```
rm -rf /tmp/submission /tmp/problem
mkdir /tmp/submission
cp samples/java/example1/*.java /tmp/submission
cp -R samples/java/example1 /tmp/problem
```
Set a breakpoint in app/com/horstmann/codecheck/Main.java and launch the debugger with the Command Line Tool configuration.

Building the Server
-------------------

Run the `play-codecheck` server:

    COMRUN_USER=$(whoami) sbt run

Point the browser to <http://localhost:9000/assets/uploadProblem.html>.
Upload a problem and test it.

Note: The problem files will be located inside the `/opt/codecheck/repo/ext`
directory.

Debugging the Server
--------------------

Run the `play-codecheck` server in debug mode:

    COMRUN_USER=$(whoami) sbt -jvm-debug 9999 run

In Eclipse, select Run → Debug Configurations, select the configuration
you created, and select Debug. Point the browser to a URL such as
<http://localhost:9000/assets/uploadProblem.html>. Set breakpoints as
needed.

## Podman/Docker Installation

Skip this step if you are on Codespaces. Codespaces already has Docker installed.

Install Podman and Podman-Docker:
```
sudo apt-get podman podman-docker
sudo touch /etc/containers/nodocker 
```

Docker Local Testing
--------------------

Build and run the Docker container for the `comrun` service:

    docker build --tag comrun:1.0-SNAPSHOT comrun
    docker run -p 8080:8080 -it comrun:1.0-SNAPSHOT &

Test that it works:

    /opt/codecheck/codecheck -lt samples/java/example1

Create a file `conf/production.conf` holding an [application
secret](https://www.playframework.com/documentation/2.8.x/ApplicationSecret):

    echo "play.http.secret.key=\"$(head -c 32 /dev/urandom | base64)\"" > conf/production.conf
    echo "com.horstmann.codecheck.comrun.remote=\"http://host.docker.internal:8080/api/upload\"" >> conf/production.conf

Do not check this file into version control!

Build the Docker container for the `play-codecheck` server. 

    sbt docker:publishLocal 
    
Ignore the `[error]` labels during the Docker build. They aren't actually errors.

Run the container. If you do this on your own computer:

    docker run -p 9090:9000 -it play-codecheck:1.0-SNAPSHOT
    
Test that it works by pointing your browser to
<http://localhost:9090/assets/uploadProblem.html>.     

On Codespaces:

    docker run -p 9090:9000 -it --add-host host.docker.internal:host-gateway play-codecheck:1.0-SNAPSHOT &

Then locate the Ports tab and open the local address for port 9090. Ignore the nginx error and paste `/assets/uploadProblem.html` after the URL. 

To complete the test locally or on Codespaces, upload a problem: File name `Numbers.java`, file contents:

```
public class Numbers
{
//CALL 3, 4
//CALL -3, 3
//CALL 3, 0
   public double average(int x, int y)
   {
      //HIDE
      return 0.5 * (x + y);
      //SHOW // Compute the average of x and y
   }
}
```

Click the Submit Files button. You should see three passing test cases.

Kill both containers by running this command in the terminal:

    docker container kill $(docker ps -q)    

Cloud Provider Tools
--------------------

Install Google Cloud CLI for Linux or [follow the instruction for your environment](https://cloud.google.com/sdk/docs/install#linux)

Open a terminal and download the Google Cloud SDK
```
curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-373.0.0-linux-x86_64.tar.gz
```

Extract the contents of the file to any location on your file system (preferably your home directory). To replace an existing installation, remove the existing google-cloud-sdk directory and then extract the archive to the same location.
```
tar -xf google-cloud-sdk-373.0.0-linux-x86.tar.gz
```

Run the script (from the root of the folder you extracted to) using the following command
```
./google-cloud-sdk/install.sh
```
To initialize the gcloud CLI, run `gcloud init`
```
./google-cloud-sdk/bin/gcloud init
```
Install the AWS CLI for Linux or [follow the instruction for your environment](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)

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
Configure the AWS CLI [instructions](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-quickstart.html)
* Access key ID
* Secret access key
* AWS Region
* Output format
```
aws configure
```

Comrun Service Deployment (Google Cloud)
-------------------------

If you develop locally (i.e. not on Codespaces), run this command:

    gcloud auth print-access-token | docker login -u oauth2accesstoken --password-stdin gcr.io

There are two parts to the CodeCheck server. We\'ll take them up one at
a time. The `comrun` service compiles and runs student programs,
isolated from the web app and separately scalable.

Here is how to deploy the `comrun` service to Google Cloud.

Make a Google Cloud Run project. Define a service `comrun`.

Then run:

    export PROJECT=your Google project name
    docker tag comrun:1.0-SNAPSHOT gcr.io/$PROJECT/comrun
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

You should get a URL for the service. Make a note of it---it won\'t
change, and you need it in the next steps. To test that the service is
properly deployed, do this:

    export REMOTE_URL=the URL of the comrun service
    cd path to/codecheck2 
    /opt/codecheck/codecheck -rt samples/java/example1

You should get a report that was obtained by sending the compile and run
jobs to your remote service.

Alternatively, you can test with the locally running web app. In
`conf/production.conf`, you need to add

    com.horstmann.codecheck.comrun.remote="comrun host URL/api/upload"

Comrun Service Deployment (AWS)
-------------------------
To automate login you would need to create a profile by running the following command in the terminal

 ```
 aws configure –-profile your-username
 ```

The following information will need to be provided:
```
AWS Access Key ID [None]:
AWS Secret Access Key [None]:
Default region name [None]:
Default output format [None]:
```

You should now have the two files, .aws/credentials and .aws/config,  after configurating your profile. 

The ```.aws/credentials``` file should contain:
```
[your-username]
aws_access_key_id=your-input
aws_secret_access_key=your-input
```

And the ```.aws/config``` file contains:
```
[profile your-username]
region = your-region #example: us-west-2
output = json
```
Then in the terminal run, 
```
export AWS_DEFAULT_PROFILE=your-username
aws sts get-caller-identity --query "Account" --output text #output is your AWS account ID
```
We'd want to set up our environmental variables.
```
ACCOUNT_ID=$(aws sts get-caller-identity --query "Account" --output text)
echo Account ID: $ACCOUNT_ID
REGION=$(aws configure get region)
echo Region: $REGION
```

If ```REGION=$(aws configure get region)``` shows up to be the incorrect region, check out this link https://docs.aws.amazon.com/general/latest/gr/apprunner.html and set your region to the correct one by typing ```REGION = your-region```

From here, we want to create a IAM Accesss Role Name. We will then attach the role to a pre-existing policy, ``` arn:aws:iam::aws:policy/service-role/AWSAppRunnerServicePolicyForECRAccess```
```
export TP_FILE=$(mktemp)
export ROLE_NAME=AppRunnerECRAccessRole
cat <<EOF | tee $TP_FILE
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "Service": "build.apprunner.amazonaws.com"
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
EOF

aws iam create-role --role-name $ROLE_NAME --assume-role-policy-document file://$TP_FILE

rm $TP_FILE

aws iam attach-role-policy --role-name $ROLE_NAME --policy-arn arn:aws:iam::aws:policy/service-role/AWSAppRunnerServicePolicyForECRAccess 
```
Since we have already set up our environmental variables & IAM access role, sign into the ECR repository and create a repository using, 

```
aws ecr get-login-password --region $REGION | docker login --username AWS --password-stdin $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com

ECR_REPOSITORY=ecr-comrun

aws ecr create-repository \
     --repository-name $ECR_REPOSITORY \
     --region $REGION
```

To upload a container image to the ECR repository, we want to tag and push it: 
```
docker images
PROJECT=comrun

docker tag $PROJECT:1.0-SNAPSHOT $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY

docker push $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY
```
To see if we have pushed the docker image into the ECR repository run,
```
aws ecr list-images --repository-name $ECR_REPOSITORY
```
Lastly, to deploy the comrun service to AWS App Runner, Create a temporary file to store in the contents of the source configuration:

```
cat <<EOF | tee $TP_FILE
{
     "ImageRepository": {
         "ImageIdentifier": "$ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY:1.0-SNAPSHOT",
         "ImageRepositoryType": "ECR"
     },
     "AutoDeploymentsEnabled": true,
     "AuthenticationConfiguration": {
         "AccessRoleArn": "arn:aws:iam::$ACCOUNT_ID:role/AppRunnerECRAccessRole"
     }
}
EOF
```

Then deploy the comrun service
```
aws apprunner --region $REGION create-service --service-name comrun --source-configuration file://$TP_FILE
```
Find the service URL and wait until ```aws apprunner --region $REGION list-services``` has the service status as running. 

Finally, run curl with the URL 
```
curl your-URL-link
```

Play Server Deployment
----------------------

Set environment variables and create a user in your Amazon AWS account:

```
ACCOUNT_ID=$(aws sts get-caller-identity --query "Account" --output text)
echo Account ID: $ACCOUNT_ID
REGION=$(aws configure get region)
echo Region: $REGION

USERNAME=codecheck

aws iam create-user --user-name $USERNAME
aws iam create-access-key --user-name $USERNAME

# IMPORTANT: Record AccessKeyId and SecretAccessKey
```

In Amazon S3, create a bucket whose name starts with the four characters `ext.` and an arbitrary suffix, such as `ext.mydomain.com` to hold
the uploaded CodeCheck problems. Set the ACL so that the bucket owner has all access rights and nobody else has any.

```
# Change the suffix below
SUFFIX=mydomain.com

aws s3 mb s3://ext.$SUFFIX

cat <<EOF > CodeCheckS3.json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "s3:*"
            ],
            "Resource": [
                "arn:aws:s3:::ext.$SUFFIX"
            ]
        },
        {
            "Effect": "Allow",
            "Action": [
                "s3:*"
            ],
            "Resource": [
                "arn:aws:s3:::ext.$SUFFIX/*"
            ]
        }
    ]
}
EOF

aws iam create-policy --policy-name CodeCheckS3 --policy-document file://./CodeCheckS3.json

aws iam attach-user-policy --user-name $USERNAME \
  --policy-arn arn:aws:iam::$ACCOUNT_ID:policy/CodeCheckS3
```

If you use CodeCheck with LTI, you need to set up an Amazon Dynamo database. Create the following tables:

| Name                      | Partition key      | Sort key    |
| ------------------------- | ------------------ | ----------- |
| CodeCheckAssignments      | assignmentID       |             |
| CodeCheckLTICredentials   | oauth_consumer_key |             |
| CodeCheckLTIResources     | resourceID         |             |
| CodeCheckSubmissions      | submissionID       | submittedAt |
| CodeCheckWork             | assignmentID       | workID      |
| CodeCheckComments         | assignmentID       | workID      |

The first three tables have no sort key. All types are `String`.

```
aws --region $REGION dynamodb create-table \
    --table-name CodeCheckAssignments \
    --attribute-definitions AttributeName=assignmentID,AttributeType=S \
    --key-schema AttributeName=assignmentID,KeyType=HASH \
    --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1

aws --region $REGION dynamodb create-table \
    --table-name CodeCheckLTICredentials \
    --attribute-definitions AttributeName=oauth_consumer_key,AttributeType=S \
    --key-schema AttributeName=oauth_consumer_key,KeyType=HASH \
    --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1

aws --region $REGION dynamodb create-table \
    --table-name CodeCheckLTIResources \
    --attribute-definitions AttributeName=resourceID,AttributeType=S \
    --key-schema AttributeName=resourceID,KeyType=HASH \
    --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1

aws --region $REGION dynamodb create-table \
    --table-name CodeCheckSubmissions \
    --attribute-definitions AttributeName=submissionID,AttributeType=S AttributeName=submittedAt,AttributeType=S \
    --key-schema AttributeName=submissionID,KeyType=HASH AttributeName=submittedAt,KeyType=RANGE \
    --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1

aws --region $REGION dynamodb create-table \
    --table-name CodeCheckWork \
    --attribute-definitions AttributeName=assignmentID,AttributeType=S AttributeName=workID,AttributeType=S \
    --key-schema AttributeName=assignmentID,KeyType=HASH AttributeName=workID,KeyType=RANGE \
    --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1
    
aws --region $REGION dynamodb create-table \
    --table-name CodeCheckComments \
    --attribute-definitions AttributeName=assignmentID,AttributeType=S AttributeName=workID,AttributeType=S \
    --key-schema AttributeName=assignmentID,KeyType=HASH AttributeName=workID,KeyType=RANGE \
    --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1
    
cat <<EOF > CodeCheckDynamo.json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "dynamodb:PutItem",
                "dynamodb:UpdateItem",
                "dynamodb:DeleteItem",
                "dynamodb:BatchWriteItem",
                "dynamodb:GetItem",
                "dynamodb:BatchGetItem",
                "dynamodb:Scan",
                "dynamodb:Query",
                "dynamodb:ConditionCheckItem"
            ],
            "Resource": [
                "arn:aws:dynamodb:us-west-1:$ACCOUNT_ID:table/CodeCheck*",
                "arn:aws:dynamodb:us-west-1:$ACCOUNT_ID:table/CodeCheck*/index/*"
            ]
        }
    ]
}
EOF

aws iam create-policy --policy-name CodeCheckDynamo --policy-document file://./CodeCheckDynamo.json

aws iam attach-user-policy --user-name $USERNAME \
  --policy-arn arn:aws:iam::$ACCOUNT_ID:policy/CodeCheckDynamo

aws iam list-attached-user-policies --user-name $USERNAME    
```

You need to populate the `CodeCheckLTICredentials` table with at least one pair `oauth_consumer_key` and `shared_secret` (both of type `String`). These can be any values. I recommend to use the admin's email for `oauth_consumer_key` and a random password for `shared_secret`. 

```
USERNAME=codecheck
PASSWORD=$(strings /dev/urandom | grep -E '[^ ]{8}' | head -1)
echo Password: $PASSWORD
aws dynamodb put-item --table-name CodeCheckLTICredentials --item '{"oauth_consumer_key":{"S":"'${USERNAME}'"},"shared_secret":{"S":"'${PASSWORD}'"}}'
```

In your Google Cloud Run project, add another service `play-codecheck`.

Add the following to `conf/production.conf`:

    play.http.secret.key= see above
    com.horstmann.codecheck.comrun.remote="comrun host URL/api/upload"
    com.horstmann.codecheck.aws.accessKey= your AWS credentials
    com.horstmann.codecheck.aws.secretKey=
    com.horstmann.codecheck.s3.bucketsuffix="mydomain.com"
    com.horstmann.codecheck.s3.region=your AWS region such as "us-west-1"
    com.horstmann.codecheck.dynamodb.region=your AWS region such as "us-west-1"
    com.horstmann.codecheck.storeLocation=""

Deploy the `play-codecheck` service:

    export PROJECT=your Google project name
    sbt docker:publishLocal
    docker tag play-codecheck:1.0-SNAPSHOT gcr.io/$PROJECT/play-codecheck
    docker push gcr.io/$PROJECT/play-codecheck

    gcloud run deploy play-codecheck \
      --image gcr.io/$PROJECT/play-codecheck \
      --port 9000 \
      --platform managed \
      --region us-central1 \
      --allow-unauthenticated \
      --min-instances=1

You will get a URL for the service. Now point your browser to
`https://service url/assets/uploadProblem.html`


Play Server Deployment (AWS)
-------------------------
Since we have set up our environmental variables before for our Comrun Service Deployment and IAM Access Role, sign into the ECR repository using this command line (if you already haven't)

```
aws ecr get-login-password --region $REGION | docker login --username AWS --password-stdin $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com
```
Let’s make another ECR repository to store in the play-codecheck service.
```
ECR_REPOSITORY=erc-play-codecheck

aws ecr create-repository \
     --repository-name $ECR_REPOSITORY \
     --region $REGION
```
From here, we want to upload a container image to the ECR repository
```
docker images 
PROJECT=play-codecheck

docker tag $PROJECT:1.0-SNAPSHOT $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY

docker push $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY
```
To see that we have pushed the docker image into the ECR repository run:
```
aws ecr list-images --repository-name $ECR_REPOSITORY
```

Create a temporary file to store in the contents of the source configuration:
```
cat <<EOF | tee $TP_FILE
{
     "ImageRepository": {
         "ImageIdentifier": "$ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_ACCOUNT:latest",
         "ImageRepositoryType": "ECR"
     },
     "AutoDeploymentsEnabled": true,
     "AuthenticationConfiguration": {
         "AccessRoleArn": "arn:aws:iam::$ACCOUNT_ID:role/AppRunnerECRAccessRole"
     }
}
EOF
```
Then deploy the play-codecheck service
```
aws apprunner --region $REGION create-service --service-name comrun --source-configuration file://$TP_FILE
```
Find the service URL and wait until ```aws apprunner --region $REGION list-services``` has the service status as running. 

Finally, run curl with the URL 
```
curl your-URL-link
```