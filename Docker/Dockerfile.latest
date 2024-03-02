## Docker File for MVS/CE
# This docker file is for use in github actions
# To build this container yourself use https://github.com/mvs-sysgen/docker-mvsce
FROM mainframed767/hercules:latest
WORKDIR /
USER root
ADD MVSCE.release.tar /
COPY Docker/mvs.sh /
RUN apt-get update && apt-get -yq install --no-install-recommends socat ca-certificates openssl python3 netbase git && apt-get clean && chmod +x /mvs.sh
VOLUME ["/config","/dasd","/printers","/punchcards","/logs", "/certs"]
EXPOSE 3221 3223 3270 3505 3506 8888
ENTRYPOINT ["./mvs.sh"]
