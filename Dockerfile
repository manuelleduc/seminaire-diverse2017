FROM haskell
ADD . /project
WORKDIR /project
RUN apt-get update
RUN apt-get install -y xz-utils make
RUN stack setup
RUN stack install
EXPOSE 8080
ENTRYPOINT ["stack exec -- seminaire-diverse2017-servant-exe"]
