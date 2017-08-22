FROM microsoft/dotnet:2.0-runtime

WORKDIR /opt/app

RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends psql; \
    rm -rf /var/lib/apt/lists/*;

COPY src/MAptekaGet.Core/out/ /opt/app
COPY scripts/ /opt/app/scripts

ENTRYPOINT ["docker-entrypoint.sh"]

EXPOSE 80

CMD dotnet MAptekaGet.Core.dll --ip 0.0.0.0 --port 80
