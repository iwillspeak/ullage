FROM ubuntu:18.04
COPY llvm.list /etc/apt/sources.list.d/
RUN apt-get update
RUN apt-get install -y sudo
RUN apt-get install -y python3 python3-pip python3-virtualenv
RUN apt-get install -y gcc binutils
RUN apt-get install -y --allow-unauthenticated llvm-8