FROM haskell:8.4.3
RUN find .
ENTRYPOINT ["goodstuff", "web"]