#!/bin/bash

# Install Codecheck dependencies
cd ..
sudo dnf install git ant curl unzip

# Install sbt
sudo rm -f /etc/yum.repos.d/bintray-rpm.repo || true
curl -L https://www.scala-sbt.org/sbt-rpm.repo > sbt-rpm.repo
sudo mv sbt-rpm.repo /etc/yum.repos.d/
sudo dnf install sbt


# Install docker for fedora 35
# Setup the repository
sudo dnf -y install dnf-plugins-core

sudo dnf config-manager \
    --add-repo \
    https://download.docker.com/linux/fedora/docker-ce.repo

# Install Docker Engine
sudo dnf install docker-ce docker-ce-cli containerd.io

# Start Docker
sudo systemctl start docker

# Verify that Docker Engine is installed correctly
sudo docker run hello-world


# Install Google Cloud CLI for linux 
# Update DNF with gcloud CLI repo information
sudo tee -a /etc/yum.repos.d/google-cloud-sdk.repo << EOM
[google-cloud-cli]
name=Google Cloud CLI
baseurl=https://packages.cloud.google.com/yum/repos/cloud-sdk-el8-x86_64
enabled=1
gpgcheck=1
repo_gpgcheck=0
gpgkey=https://packages.cloud.google.com/yum/doc/yum-key.gpg
       https://packages.cloud.google.com/yum/doc/rpm-package-key.gpg
EOM

sudo dnf install libxcrypt-compat.x86_64

# Install the gcloud CLI:
sudo dnf install google-cloud-cli

# Run gcloud init to get started
gcloud init


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
