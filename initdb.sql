-- DROP TABLE update
CREATE TABLE update (
  name          TEXT NOT NULL,
  major         INT  NOT NULL  DEFAULT 0,
  minor         INT  NOT NULL  DEFAULT 0,
  patch         INT  NOT NULL  DEFAULT 1,
  constraints   TEXT NOT NULL  DEFAULT '',
  summary       TEXT NOT NULL  DEFAULT '',
  description   TEXT NOT NULL  DEFAULT '',
  created       TIMESTAMP      DEFAULT CURRENT_DATE,
  release_notes TEXT NOT NULL  DEFAULT '',
  author        TEXT NOT NULL  DEFAULT '',
  uri           TEXT NOT NULL  DEFAULT '',
  CONSTRAINT pk_update PRIMARY KEY (name, major, minor, patch)
);

INSERT INTO update (name, major, minor, patch) VALUES
  ('HelloWorld', 1, 0, 0);

-- DROP TABLE customer_update
CREATE TABLE customer_update (
  customer_id TEXT    NOT NULL,
  name        TEXT    NOT NULL,
  major       INT     NOT NULL,
  minor       INT     NOT NULL,
  patch       INT     NOT NULL,
  installed   BOOLEAN NOT NULL DEFAULT FALSE,
  CONSTRAINT pk_customer_update PRIMARY KEY (customer_id, name, major, minor, patch),
  CONSTRAINT fk_customer_update FOREIGN KEY (name, major, minor, patch) REFERENCES update (name, major, minor, patch)
);

-- DROP VIEW v_nearby_names
CREATE OR REPLACE VIEW v_nearby_names
AS
SELECT DISTINCT
  u1.name AS of_name,
  u2.name,
  levenshtein(u1.name, u2.name) AS dist
FROM update AS u1
  CROSS JOIN update u2
;
