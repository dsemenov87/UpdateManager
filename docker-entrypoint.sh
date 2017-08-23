#!/usr/bin/env bash
set -e

dbname=mapteka_get
conninfo=postgres://postgres:5432/"$POSTGRES_DB"
rolname=mapteka_get

psql=( psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname ${conninfo})

if "${psql[@]}" -tc "SELECT 1 FROM pg_roles WHERE rolname = '${rolname}'" | grep -q 1
    then echo "User '${rolname}' already exists.\n"
    else
        echo "Creating user '${rolname}'...\n"
        "${psql[@]}" "CREATE USER ${rolname};"
fi

if "${psql[@]}" -tc "SELECT 1 FROM pg_database WHERE datname = '${dbname}'" | grep -q 1
    then echo "Database '${dbname}' already exists.\n"
    else
        echo "Creating database '${dbname}'...\n"
        "${psql[@]}" <<-EOSQL
        CREATE DATABASE "${dbname}"
            WITH
            OWNER = "${rolname}"
            ENCODING = 'UTF8'
            LC_COLLATE = 'en_US.UTF8'
            LC_CTYPE = 'en_US.UTF8'
            TABLESPACE = pg_default
            CONNECTION LIMIT = -1;
EOSQL
        for f in "ls -v ./scripts/*.sql"; do 
            echo "$0: running $f"
            "${psql[@]}" --dbname ${dbname} -f "$f";
        done
fi

exec "$@"
