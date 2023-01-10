-- Active: 1673272169667@@192.168.58.2@30002@postgresdb@public

DROP TABLE person;

CREATE TABLE
    person(
        id SERIAL PRIMARY KEY NOT NULL,
        name TEXT NOT NULL,
        age INT NOT NULL
    );

INSERT INTO person (name, age) VALUES ('Horl', 30), ('Bert', 33);

SELECT * FROM person;