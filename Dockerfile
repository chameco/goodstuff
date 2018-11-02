FROM haskell:8.4.3
WORKDIR /opt/server
COPY . /opt/server
RUN apt-get update
RUN apt-get install -y pkg-config libpcre3 libpcre3-dev postgresql postgresql-client libpq-dev sqlite3 curl
RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
RUN apt-get install -y nodejs
RUN npm install --global bower
RUN make
ENTRYPOINT ["goodstuff", "web"]