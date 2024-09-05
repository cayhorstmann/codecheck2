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
mkdir /tmp/problem
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

    docker build --tag codecheck:1.0-SNAPSHOT comrun
    docker run -p 8080:8080 -it codecheck:1.0-SNAPSHOT &

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

Comrun Service Deployment on AWS
--------------------------------

[Install the AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)

Confirm the installation with the following command

```
aws --version
```

[Configure the AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-quickstart.html)

```
 aws configure –-profile your-username
```

Set
* Access key ID
* Secret access key
* AWS Region
* Output format

You should now have the two files, `.aws/credentials` and `.aws/config`. 

The ```.aws/credentials``` file should contain:
```
[your-username]
aws_access_key_id=...
aws_secret_access_key=...
```

And the ```.aws/config``` file:
```
[profile your-username]
region = your-region #example: us-west-2
output = json

```

Set environment variables: 

```
export AWS_DEFAULT_PROFILE=your-username
ACCOUNT_ID=$(aws sts get-caller-identity --query "Account" --output text)
echo Account ID: $ACCOUNT_ID
REGION=$(aws configure get region)
echo Region: $REGION
```

If ```REGION=$(aws configure get region)``` shows up to be the incorrect region, check out this link https://docs.aws.amazon.com/general/latest/gr/apprunner.html and set your region to the correct one by typing 

```
REGION = your-region
```

Create an IAM Accesss Role and attach a pre-existing policy for access to container repositories.

```
export TEMPFILE=$(mktemp)
export ROLE_NAME=AppRunnerECRAccessRole
cat <<EOF | tee $TEMPFILE
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

aws iam create-role --role-name $ROLE_NAME --assume-role-policy-document file://$TEMPFILE

rm $TEMPFILE

aws iam attach-role-policy --role-name $ROLE_NAME --policy-arn arn:aws:iam::aws:policy/service-role/AWSAppRunnerServicePolicyForECRAccess 
```

Next we set up a repository for Docker images:

```
aws ecr get-login-password --region $REGION | docker login --username AWS --password-stdin $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com

ECR_REPOSITORY=ecr-comrun

aws ecr create-repository \
     --repository-name $ECR_REPOSITORY \
     --region $REGION
```

Now push the image to that repository:

```
docker images
PROJECT=comrun

docker tag $PROJECT:1.0-SNAPSHOT $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY

docker push $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY
```

To see if we have pushed the docker image into the ECR repository, run:

```
aws ecr describe-images --repository-name $ECR_REPOSITORY --region $REGION
```

Finally, we deploy the comrun service to AWS App Runner:

```
export TEMPFILE=$(mktemp)

cat <<EOF | tee $TEMPFILE
{
     "ImageRepository": {
         "ImageIdentifier": "$ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY:latest",
         "ImageConfiguration": {
            "Port": "8080"
        },
         "ImageRepositoryType": "ECR"
     },
     "AutoDeploymentsEnabled": true,
     "AuthenticationConfiguration": {
         "AccessRoleArn": "arn:aws:iam::$ACCOUNT_ID:role/AppRunnerECRAccessRole"
     }
}
EOF

aws apprunner --region $REGION create-service --service-name comrun --source-configuration file://$TEMPFILE
```

Save the service URL. Then wait until has the service status as  ```RUNNING```. 

 ```
 aws apprunner --region $REGION list-services
 ```

Then test it: 

```
curl service-URL
```

If you get

```
<form action="/api/upload" enctype="multipart/form-data" method="post">
   <div>File: <input type="file" name="job"/></div>
   <input type="submit" value="Upload" />
</form>
```

the comrun service was deployed.

To test that the service is working properly, do this:

    cd path-to-codecheck2-repo
    export REMOTE_URL=service-URL
    /opt/codecheck/codecheck -rt samples/java/example1

You should get a report that was obtained by sending the compile and run
jobs to your remote service.

Alternatively, you can test with the locally running web app. In
`conf/production.conf`, you need to add

    com.horstmann.codecheck.comrun.remote=service-URL/api/upload

Using AWS Data Storage
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

Play Server Deployment (AWS)
----------------------------

Make another ECR repository to store in the play-codecheck service. Note that you need the `ACCOUNT_ID` and `REGION` environment variables from the comrun deployment.


    ECR_REPOSITORY=ecr-play-codecheck

    aws ecr create-repository \
         --repository-name $ECR_REPOSITORY \
         --region $REGION

Add the following to `conf/production.conf`:

    play.http.secret.key= see above
    com.horstmann.codecheck.comrun.remote="comrun host URL/api/upload"
    com.horstmann.codecheck.aws.accessKey= your AWS credentials
    com.horstmann.codecheck.aws.secretKey=
    com.horstmann.codecheck.s3.bucketsuffix="mydomain.com"
    com.horstmann.codecheck.s3.region=your AWS region such as "us-west-1"
    com.horstmann.codecheck.dynamodb.region=your AWS region such as "us-west-1"
    com.horstmann.codecheck.storeLocation=""

Run

    sbt docker:publishLocal

Upload the container image to the ECR repository

    docker images 
    PROJECT=play-codecheck

    docker tag $PROJECT:1.0-SNAPSHOT $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY

    docker push $ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY
    
To see that we have pushed the docker image into the ECR repository run:

    aws ecr describe-images --repository-name $ECR_REPOSITORY --region $REGION

Then deploy the play-codecheck service

```
export TEMPFILE=$(mktemp)

cat <<EOF | tee $TEMPFILE
{
     "ImageRepository": {
         "ImageIdentifier": "$ACCOUNT_ID.dkr.ecr.$REGION.amazonaws.com/$ECR_REPOSITORY:latest",
         "ImageConfiguration": {
            "Port": "9000"
        },
         "ImageRepositoryType": "ECR"
     },
     "AutoDeploymentsEnabled": true,
     "AuthenticationConfiguration": {
         "AccessRoleArn": "arn:aws:iam::$ACCOUNT_ID:role/AppRunnerECRAccessRole"
     }
}
EOF

aws apprunner --region $REGION create-service --service-name $PROJECT --source-configuration file://$TEMPFILE

rm $TEMPFILE

```

Make note of the service URL. Then wait until it has the service status as  `RUNNING`. 

    aws apprunner --region $REGION list-services

You will get a URL for the service. Now point your browser to
`https://service url/assets/uploadProblem.html`
