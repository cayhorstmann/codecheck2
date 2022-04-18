
# Docker Deployment
## Build and run the comrun service Docker container
From the root directory of the repository, build the comrun service Docker container
```
sudo docker build --tag codecheck:1.0-SNAPSHOT comrun
```
From the root directory of the repository, run the comrun service Docker container
```
sudo docker run -p 8080:8080 -it codecheck:1.0-SNAPSHOT
```
To verify that it works
```
/opt/codecheck/codecheck -l samples/java/example1 &
```
## Build and run the play-codecheck Docker container
From the root directory of the repository, build the Docker image
```
sudo sbt docker:publishLocal 
```
From the root directory of the repository, run the Docker container
```
sudo docker run -p 9090:9000 -it --add-host host.docker.internal:host-gateway play-codecheck:1.0-SNAPSHOT
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



## Create AWS S3 bucket
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
Update the production.conf file with /api/upload append at the end of the comrun remote url. 
```
com.horstmann.codecheck.comrun.remote="https://comrun-url/api/upload"

```


## How to upload a test file to AWS S3 bucket using the CLI
To upload a file to S3, you’ll need to provide two arguments (source and destination) to the aws s3 cp command.
```
aws s3 cp test.txt s3://<BUCKET-NAME>
```


## How to upload multiple zip files to AWS S3 bucket using the CLI
Open a terminal and go to the directory where the the zip files are located

Uploads the zip files in the current directory to your AWS bucket
```
for f in $(ls *) ; do aws s3 cp $f s3://ext.yourbucketsuffix.edu; done
```
## How to scale your comrun service
Following the [guideline from Google Cloud](https://cloud.google.com/run/docs/about-instance-autoscaling)

Go to your [google cloud Run](https://console.cloud.google.com/run)

* Click on a comrun service
* Click on Edit and Deploy New Revision
* Under Capacity you can change the Memory and CPU settings
* Under Capacity you can change the Maximum requests per container (Concurrency)
* Under Autoscaling you can change the Minimum and Maximum instances
