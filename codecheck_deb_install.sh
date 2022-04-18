#!/bin/bash

# Install Codecheck dependencies
cd ..
sudo apt-get update
sudo apt install  git ant curl unzip

# Install sbt
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt-get update
sudo apt-get install sbt

# Install docker
# Setup the repository
sudo apt-get update

sudo apt-get install ca-certificates curl gnupg lsb-release

# Add Docker’s official GPG key
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg

# Use the following command to set up the stable repository. 
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# Install Docker Engine

sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io

# Start Docker
sudo systemctl start docker

# Verify that Docker Engine is installed correctly
sudo docker run hello-world


# Install Google Cloud CLI for linux 
# Update DNF with gcloud CLI repo information


#sudo dnf install libxcrypt-compat.x86_64

# Install the gcloud CLI:
#sudo dnf install google-cloud-cli

# Run gcloud init to get started
#gcloud init


# Install AWS CLI
# AWS CLI installation file
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
# Unzip the installer
unzip awscliv2.zip
# Run the install program
sudo ./aws/install
# Confirm the installation
aws --version
# Configure AWS CLI
aws configure

sudo mkdir -p /opt/codecheck/ext
export ME=$(whoami) ; sudo -E chown $ME /opt/codecheck /opt/codecheck/ext

cd codecheck2

# Install jackson-core, jackson-annotations, and jackson-databind jar files
cd cli
mkdir lib

cd lib
curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-core/2.6.4/jackson-core-2.6.4.jar
curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.6.4/jackson-annotations-2.6.4.jar
curl -LOs https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.6.4/jackson-databind-2.6.4.jar

# Install checkstyle, hamcrest-core, and junit jar files
cd ../../

cd comrun/bin
mkdir lib

cd lib
curl -LOs https://repo1.maven.org/maven2/com/puppycrawl/tools/checkstyle/8.42/checkstyle-8.42.jar
curl -LOs https://repo1.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar
curl -LOs https://repo1.maven.org/maven2/junit/junit/4.13.2/junit-4.13.2.jar

cd ../../../
ant -f cli/build.xml

/opt/codecheck/codecheck -t samples/java/example1
