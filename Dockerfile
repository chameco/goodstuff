FROM haskell:8.4.3
RUN make
ENTRYPOINT ["goodstuff", "web"]