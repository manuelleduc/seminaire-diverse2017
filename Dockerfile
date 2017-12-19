FROM haskell:8.0.2
COPY . /work
WORKDIR /work
RUN stack setup
RUN stack install
EXPOSE 8080
ENTRYPOINT ["seminaire-diverse2017-servant-exe"]
