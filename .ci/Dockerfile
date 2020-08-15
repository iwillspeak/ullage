FROM ubuntu:20.04
RUN apt-get update && \
	apt-get install -y wget curl gnupg2 lsb-release software-properties-common && \
	curl -O https://apt.llvm.org/llvm.sh && \
	chmod +x llvm.sh && \
	./llvm.sh 9 && \
	apt-get install -y sudo && \
	apt-get install -y python3 && \
	apt-get install -y llvm-9-dev && \
	apt-get install -y clang binutils && \
	apt-get install -y zlib1g-dev
