#!/bin/bash

set -e

# Install GraalVM
# https://www.graalvm.org/docs/getting-started/linux/

sudo mkdir /opt/graal
cd /opt/graal
sudo curl -L -O https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-21.1.0/graalvm-ce-java8-linux-amd64-21.1.0.tar.gz
sudo tar -xzf graalvm-ce-java8-linux-amd64-21.1.0.tar.gz

# Install native-image extension
# https://www.graalvm.org/reference-manual/native-image/

sudo apt install build-essential libz-dev zlib1g-dev
sudo env JAVA_HOME=/opt/graal/graalvm-ce-java8-21.1.0 /opt/graal/graalvm-ce-java8-21.1.0/bin/gu install native-image