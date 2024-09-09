FROM ubuntu:24.04

RUN apt-get update && \
    apt-get install -y software-properties-common

RUN apt-get update && apt-get install --no-install-recommends -y \
    wget \
    grep \
    zip \
    unzip \
    sysstat \
    sudo \
    g++ \
    haskell-compiler \
    openjdk-21-jdk-headless \
    libxi6 \
    libxtst6 \
    libxrender1 \
    mono-mcs \
    nodejs \
    polyml \
    libpolyml-dev \
    python3 \
    python3-tk \
    python3-pillow \
    racket \
    scala \
    locales \
    strace \
    rustc \
 && rm -rf /var/lib/apt/lists/*


ENV KOTLIN_VERSION 2.0.20
RUN wget -q "https://github.com/JetBrains/kotlin/releases/download/v${KOTLIN_VERSION}/kotlin-compiler-${KOTLIN_VERSION}.zip" && \
    unzip kotlin-compiler-${KOTLIN_VERSION}.zip -d / && mv /kotlinc /usr/lib/ && rm kotlin-compiler-${KOTLIN_VERSION}.zip
ENV PATH="/usr/lib/kotlinc/bin:${PATH}"  
	
# https://stackoverflow.com/questions/28405902/how-to-set-the-locale-inside-a-debian-ubuntu-docker-container
RUN locale-gen --no-purge en_US.UTF-8
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

# Running as "server" doesn't work on Google Cloud
# RUN useradd -u 1011 -ms /bin/bash server
RUN useradd -u 1012 -ms /bin/bash comrunner
# RUN echo "server ALL = (comrunner) NOPASSWD: ALL" >> /etc/sudoers
ENV COMRUN_USER=comrunner

# https://unix.stackexchange.com/questions/174257/how-to-disable-sudo-related-logging-for-successful-command-execution-under-cento
RUN echo "Defaults:root !syslog, !pam_session" >> /etc/sudoers
RUN echo "Defaults:comrunner !syslog, !pam_session" >> /etc/sudoers

COPY bin /opt/codecheck
RUN chmod 755 /opt/codecheck
#COPY --chmod 755 bin /opt/codecheck
#RUN chmod +x /opt/codecheck/comrun

#RUN --chmod 733 mkdir /tmp/codecheck
RUN mkdir /tmp/codecheck
RUN chmod 733 /tmp/codecheck

# To avoid Unable to open env file: /etc/default/locale: No such file or directory
RUN echo 'LANG="en_US.UTF-8"' >> /etc/default/locale

# USER server
CMD [ "node", "/opt/codecheck/server.js" ]

EXPOSE 8080
