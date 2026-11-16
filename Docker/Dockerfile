## Docker File for MVS/CE
# This docker file is for use in github actions
# To build this container yourself use Docker/Dockerfile.local
FROM mainframed767/hercules:latest
WORKDIR /
USER root
ADD MVSCE.release.tar /
COPY Docker/mvs.sh /
RUN apt-get update && apt-get -yq install --no-install-recommends socat ca-certificates openssl python3 netbase git websockify && apt-get clean && chmod +x /mvs.sh
# Install HercConsole into Hercules httproot
RUN git clone --depth=1 https://github.com/mainframed/HercConsole.git /tmp/hercconsole && \
    HTTPROOT=/home/hercules/herctest/herc4x/share/hercules && \
    mkdir -p $HTTPROOT && \
    cp /tmp/hercconsole/hercconsole.html $HTTPROOT/hercules.html && \
    cp /tmp/hercconsole/hercconsole.js \
       /tmp/hercconsole/hercconsole.css \
       /tmp/hercconsole/tn3270.js \
       /tmp/hercconsole/3270-Regular.ttf \
       /tmp/hercconsole/buttons.svg \
       $HTTPROOT/ && \
    cp -r /tmp/hercconsole/images /tmp/hercconsole/SVG $HTTPROOT/ && \
    chown -R hercules:hercules $HTTPROOT && \
    rm -rf /tmp/hercconsole
VOLUME ["/config","/dasd","/printers","/punchcards","/logs", "/certs"]
EXPOSE 2121 3221 3223 3270 3271 3505 3506 8080 8888 22000-22200
ENTRYPOINT ["./mvs.sh"]
