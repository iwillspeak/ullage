FROM ubuntu:18.04
RUN apt-get update && \
	apt-get install -y curl gnupg2 && \
	curl -sS https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
COPY llvm.list /etc/apt/sources.list.d/
RUN apt-get update && \
	apt-get install -y sudo && \
	apt-get install -y python3 python3-pip && \
	apt-get install -y clang binutils && \
	apt-get install -y llvm-8-dev && \
	apt-get install -y zlib1g-dev
