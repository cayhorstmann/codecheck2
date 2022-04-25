CodeCheck<sup>®</sup> Build Instructions
===============================

Program Structure
-----------------

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



## Download Codecheck codebase
Download the codecheck source code using git to clone the repository
```
git clone https://github.com/cayhorstmann/codecheck2
```

## Install Codecheck dependencies

These instructions are for Ubuntu 20.04LTS.

Open a terminal and install the dependencies
```
sudo apt-get update
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
Install docker for Linux (deb) or [follow the instruction for your environment](https://docs.docker.com/engine/install/)
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
Update the apt package index, and install the latest version of Docker Engine and `containerd`
```
 sudo apt-get update
 sudo apt-get install docker-ce docker-ce-cli containerd.io
```

Install Google Cloud CLI for linux or [follow the instruction for your environment](https://cloud.google.com/sdk/docs/install#linux)

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

## Setup Codecheck environment
Create a /opt/codecheck directory and a subdirectory ext that you own
```
sudo mkdir -p /opt/codecheck/ext
export ME=$(whoami) ; sudo -E chown $ME /opt/codecheck /opt/codecheck/ext
```

Building the Command Line Tool
------------------------------
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
If you omit the `-t`, you get a report with your default browser instead
of the text report.


Debugging the Command Line Tool
-------------------------------

If you are making changes to the part of CodeCheck that does the actual
code checking, such as adding a new language, and you need to run a
debugger, it is easiest to debug the command line tool.

Make directories for the submission and problem files, and populate them
with samples.

In your debug configuration, set:

-   The main class to

        com.horstmann.codecheck.Main

-   Program arguments to

        /path/to/submissiondir /path/to/problemdir

-   VM arguments to

        -Duser.language=en
        -Duser.country=US
        -Dcom.horstmann.codecheck.comrun.local=/opt/codecheck/comrun
        -Dcom.horstmann.codecheck.report=HTML
        -Dcom.horstmann.codecheck.debug

-   The environment variable `COMRUN_USER` to your username

To debug on Windows or MacOS, you have to use the Docker container for
compilation and execution.

    docker build --tag comrun:1.0-SNAPSHOT comrun
    docker run -p 8080:8080 -it comrun:1.0-SNAPSHOT

Point your browser to <http://localhost:8080/api/health> to check that
the container is running.

When debugging, add the VM argument

    -Dcom.horstmann.codecheck.comrun.remote=http://localhost:8080/api/upload

Building the Web Application
----------------------------

Install, following the instructions of the providers,

-   [Eclipse](https://www.eclipse.org/eclipseide/)

Run the `play-codecheck` server:

    sbt run

Point the browser to <http://localhost:9090/assets/uploadProblem.html>.
Upload a problem and test it.

Note: The problem files will be located inside the `/opt/codecheck/ext`
directory.

Debugging the Server
--------------------

Import the project into Eclipse. Run

    sbt eclipse

Then open Eclipse and import the created project.

Make a debugger configuration. Select Run → Debug Configurations,
right-click on Remote Java Application, and select New Configuration.
Specify:

-   Project: `play-codecheck`
-   Connection type: Standard
-   Host: `localhost`
-   Port: 9999

Run the `play-codecheck` server in debug mode:

    COMRUN_USER=$(whoami) sbt -jvm-debug 9999 run

In Eclipse, select Run → Debug Configurations, select the configuration
you created, and select Debug. Point the browser to a URL such as
<http://localhost:9090/assets/uploadProblem.html>. Set breakpoints as
needed.

Docker Deployment
-----------------

Build and run the Docker container for the `comrun` service:

    docker build --tag codecheck:1.0-SNAPSHOT comrun
    docker run -p 8080:8080 -it codecheck:1.0-SNAPSHOT

Test that it works:

    /opt/codecheck/codecheck -l samples/java/example1 &

Create a file `conf/production.conf` holding an [application
secret](https://www.playframework.com/documentation/2.8.x/ApplicationSecret):

    echo "play.http.secret.key=\"$(head -c 32 /dev/urandom | base64)\"" > conf/production.conf
    echo "com.horstmann.codecheck.comrun.remote=\"http://host.docker.internal:8080/api/upload\"" >> conf/production.conf

Do not check this file into version control!

Build and run the Docker container for the `play-codecheck` server:

    sbt docker:publishLocal 
    docker run -p 9090:9000 -it --add-host host.docker.internal:host-gateway play-codecheck:1.0-SNAPSHOT

Test that it works by pointing your browser to
<http://localhost:9090/assets/uploadProblem.html>. Upload a problem.

Kill both containers by running this command in another terminal:

    docker container kill $(docker ps -q)

Comrun Service Deployment {#service-deployment}
-------------------------

There are two parts to the CodeCheck server. We\'ll take them up one at
a time. The `comrun` service compiles and runs student programs,
isolated from the web app and separately scalable.

Here is how to deploy the `comrun` service to Google Cloud.

Make a Google Cloud Run project. Define a service `comrun`.

Then run:

    export PROJECT=your Google project name
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

    com.horstmann.codecheck.comrun.remote= the URL of the comrun service

Play Server Deployment {#server-deployment}
----------------------

In Amazon S3, create a bucket whose name starts with the four characters `ext.` and an arbitrary suffix, such as `ext.mydomain.com` to hold
the uploaded CodeCheck problems. Set the ACL so that the bucket owner has all access rights and nobody else has any.

In your Google Cloud Run project, add another service `play-codecheck`.

Add the following to `conf/production.conf`:

    play.http.secret.key= see above
    com.horstmann.codecheck.comrun.remote=comrun host URL/api/upload
    com.horstmann.codecheck.s3.accessKey= your AWS credentials
    com.horstmann.codecheck.s3.secretKey=
    com.horstmann.codecheck.s3bucketsuffix="mydomain.com"
    com.horstmann.codecheck.s3.region=your AWS region such as "us-west-1"
    com.horstmann.codecheck.repo.ext=""
    com.horstmann.codecheck.storeLocation=""

Deploy the `play-codecheck` service:

    export PROJECT=your Google project name

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
