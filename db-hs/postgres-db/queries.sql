-- Active: 1673272169667@@192.168.58.2@30002@postgresdb@public
DROP      TABLE author;

DROP      TABLE genre;

DROP      TABLE book;

CREATE    TABLE author (
          id SERIAL PRIMARY KEY NOT NULL,
          name_author TEXT
          );

CREATE    TABLE genre (
          id SERIAL PRIMARY KEY NOT NULL,
          name_genre TEXT
          );

CREATE    TABLE book (
          id SERIAL PRIMARY KEY NOT NULL,
          author_id INT REFERENCES author (id) ON DELETE CASCADE,
          genre_id INT REFERENCES genre (id) ON DELETE CASCADE,
          price DECIMAL,
          amount DECIMAL
          );

SELECT    *
FROM      book;

SELECT    column_name
FROM      information_schema.columns
WHERE     table_schema = 'public'
AND       table_name = 'genre';

DROP TABLE genre;