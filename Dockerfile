FROM haskell:8.4.3
WORKDIR /opt/server
COPY . /opt/server
RUN make
ENTRYPOINT ["goodstuff", "web"]