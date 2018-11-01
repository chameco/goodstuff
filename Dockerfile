FROM haskell:8.4.3
WORKDIR /opt/server
COPY . /opt/server
RUN apt-get install -y pkg-config
RUN make
ENTRYPOINT ["goodstuff", "web"]