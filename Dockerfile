FROM haskell:8.4.3
WORKDIR /opt/server
COPY . /opt/server
RUN apt-get update && apt-get install -y pkg-config libpcre3 libpcre3-dev postgresql postgresql-client sqlite3 libsqlite3
RUN make
ENTRYPOINT ["goodstuff", "web"]