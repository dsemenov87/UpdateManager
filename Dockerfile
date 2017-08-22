FROM microsoft/dotnet:2.0-runtime

WORKDIR /opt/app

RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends postgresql-client-common postgresql-client-9.6; \
    rm -rf /var/lib/apt/lists/*;

COPY src/MAptekaGet.Core/out/ /opt/app
COPY scripts/ /opt/app/scripts
COPY docker-entrypoint.sh /usr/local/bin/

RUN set -ex; \
    chmod ug+x /usr/local/bin/docker-entrypoint.sh;

ENTRYPOINT ["docker-entrypoint.sh"]

EXPOSE 80

CMD dotnet MAptekaGet.Core.dll --ip 0.0.0.0 --port 80
