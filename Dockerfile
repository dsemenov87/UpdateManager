FROM microsoft/dotnet:2.1-sdk

ENV PG_MAJOR 9.6

RUN set -ex; \
# pub   4096R/ACCC4CF8 2011-10-13 [expires: 2019-07-02]
#       Key fingerprint = B97B 0AFC AA1A 47F0 44F2  44A0 7FCC 7D46 ACCC 4CF8
# uid                  PostgreSQL Debian Repository
	key='B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8'; \
	export GNUPGHOME="$(mktemp -d)"; \
	gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
	gpg --export "$key" > /etc/apt/trusted.gpg.d/postgres.gpg; \
	rm -rf "$GNUPGHOME"; \
	apt-key list; \
  echo 'deb http://apt.postgresql.org/pub/repos/apt/ jessie-pgdg main' $PG_MAJOR > /etc/apt/sources.list.d/pgdg.list;

RUN apt-get update \
    && apt-get install -y --no-install-recommends postgresql-client-common postgresql-client-${PG_MAJOR} \
    && rm -rf /var/lib/apt/lists/*

COPY src/UpdateManager/UpdateManager.fsproj /opt/src/
RUN cd /opt/src && dotnet restore

COPY src/UpdateManager /opt/src
RUN cd /opt/src && dotnet publish -c Release -o /opt/app

COPY initdb.sql /opt/app/
COPY docker-entrypoint.sh /usr/local/bin/

RUN chmod ug+x /usr/local/bin/docker-entrypoint.sh;

ENTRYPOINT ["docker-entrypoint.sh"]

WORKDIR /opt/app

EXPOSE 80

CMD dotnet UpdateManager.dll --ip 0.0.0.0 --port 80
