#!/bin/bash

csvname="$1"
dbname=$(basename "$1" .csv).db
tmpcsv=$(mktemp)

perl \
  -ne's{"([^"]*)"}{do {local $_=$1; tr/,//d; $_}}ge; print unless /,NA,NA/' \
  "$csvname" > "$tmpcsv"

sqlite3 $dbname <<EOF
DROP TABLE IF EXISTS pssa;
CREATE TABLE pssa (
  year     INTEGER  NOT NULL,
  aun      TEXT     NOT NULL,
  county   TEXT     NOT NULL,
  district TEXT     NOT NULL,
  grade    INTEGER  NOT NULL,
  subject  TEXT     NOT NULL,
  a        NUMERIC  NOT NULL,
  p        NUMERIC  NOT NULL,
  b        NUMERIC  NOT NULL,
  bb       NUMERIC  NOT NULL,
  CONSTRAINT pssa_pk PRIMARY KEY (year, district, grade, subject)
);

.separator ","
.import $tmpcsv pssa
EOF

rm "$tmpcsv"
