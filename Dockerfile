FROM microsoft/dotnet:2.0-sdk

RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends postgresql-client-common postgresql-client-9.6; \
    rm -rf /var/lib/apt/lists/*;

COPY src/MAptekaGet.Core/MAptekaGet.Core.fsproj /opt/src/
RUN cd /opt/src && dotnet restore

COPY src/MAptekaGet.Core /opt/src
RUN cd /opt/src && dotnet publish -c Release -o /opt/app

COPY initdb.sql /opt/app/
COPY docker-entrypoint.sh /usr/local/bin/

RUN chmod ug+x /usr/local/bin/docker-entrypoint.sh;

ENTRYPOINT ["docker-entrypoint.sh"]

WORKDIR /opt/app

EXPOSE 80

CMD dotnet MAptekaGet.Core.dll --ip 0.0.0.0 --port 80
