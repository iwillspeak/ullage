FROM ubuntu:18.04
RUN apt-get update && apt-get install -y curl gnupg2
RUN curl -sS https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
COPY llvm.list /etc/apt/sources.list.d/
RUN apt-get update && \
	apt-get install -y sudo && \
	apt-get install -y python3 python3-pip && \
	apt-get install -y gcc binutils && \
	apt-get install -y llvm-8
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o rustup.sh && \
	sh rustup.sh -y