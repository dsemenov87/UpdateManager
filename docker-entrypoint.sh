#!/usr/bin/env bash
set -e

dbname=mapteka_get
pguri=postgres://postgres:5432
dburi="${pguri}/${dbname}"
user=mapteka_get
initdb_path=/opt/app/initdb.sql

psql=( psql -v ON_ERROR_STOP=1 --username ${POSTGRES_USER} --dbname ${pguri} )

until "${psql[@]}" -c '\l'; do
  >&2 echo "Postgres is unavailable - sleeping"
  sleep 1
done

if "${psql[@]}" -tc "SELECT 1 FROM pg_roles WHERE rolname = '${user}'" | grep -q 1
    then echo "User '${user}' already exists.\n"
    else
        echo "Creating user '${user}'...\n"
        "${psql[@]}" -tc "CREATE USER ${user};"
fi

if "${psql[@]}" -tc "SELECT 1 FROM pg_database WHERE datname = '${dbname}'" | grep -q 1
    then echo "Database '${dbname}' already exists.\n"
    else
        echo "Creating database '${dbname}'...\n"
        "${psql[@]}" ${pguri} <<-EOSQL
        CREATE DATABASE ${dbname}
            WITH
            OWNER = ${user}
            ENCODING = 'UTF8'
            LC_COLLATE = 'en_US.utf8'
            LC_CTYPE = 'en_US.utf8'
            TABLESPACE = pg_default
            CONNECTION LIMIT = -1;

EOSQL
        psql -v ON_ERROR_STOP=1 --username ${POSTGRES_USER} --dbname ${dburi} -tc "CREATE EXTENSION fuzzystrmatch;";
        echo "Running initdb.sql...\n"
        psql -v ON_ERROR_STOP=1 --username ${user} --dbname ${dburi} -f ${initdb_path};
fi

exec "$@"
