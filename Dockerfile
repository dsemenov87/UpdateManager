FROM microsoft/dotnet:2.0-sdk-jessie

ENV PG_MAJOR 9.6

RUN set -ex; \
    echo 'deb http://apt.postgresql.org/pub/repos/apt/ jessie-pgdg main' $PG_MAJOR > /etc/apt/sources.list.d/pgdg.list \
    apt-get update; \
    apt-get install -y --no-install-recommends postgresql-client-common postgresql-client-${PG_MAJOR}; \
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
