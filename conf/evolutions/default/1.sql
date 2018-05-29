# COFFEE schema

# --- !Ups

CREATE TABLE COFFEE (
    NAME varchar(255) NOT NULL,
    PRICE numeric(10,2) NOT NULL,
    FARM varchar(255) NOT NULL,
    PRIMARY KEY (NAME)
);

# --- !Downs

DROP TABLE COFFEE;
