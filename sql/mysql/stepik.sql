CREATE TABLE book (
    book_id INT PRIMARY KEY AUTO_INCREMENT,
    title VARCHAR(50),
    author VARCHAR(30),
    price DECIMAL(8, 2),
    amount INT
);
-- 
INSERT INTO book (book_id, title, author, price, amount)
VALUES(
        1,
        'Мастер и Маргарита',
        'Булгаков М.А.',
        670.99,
        3
    ),
    (
        2,
        'Белая гвардия',
        'Булгаков М.А.',
        540.50,
        5
    ),
    (3, 'Идиот', 'Достоевский Ф.М.', 460.00, 10),
    (
        4,
        'Братья Карамазовы',
        'Достоевский Ф.М.',
        799.01,
        2
    );
SELECT *
FROM book;
-- 
SELECT title,
    author,
    amount,
    ROUND(price * 0.7, 2) AS new_price
FROM book;
-- 
SELECT author,
    title,
    ROUND(
        IF(
            author = 'Булгаков М.А.',
            price * 1.1,
            IF(author = 'Есенин С.А.', price * 1.05, price)
        ),
        2
    ) as new_price
FROM book;
-- 
SELECT author,
    title,
    price
FROM book
WHERE amount < 10;
-- 
SELECT title,
    author,
    price
FROM book
WHERE (
        author = 'Булгаков М.А.'
        OR author = 'Есенин С.А.'
    )
    AND price > 600;
--
SELECT title, author, price, amount FROM book WHERE (price < 500 OR price > 600) AND price * amount >= 5000;
-- 
SELECT title,
    author
FROM book
WHERE (
        price BETWEEN 540.50 AND 800
    )
    AND (amount IN (2, 3, 5, 7));
-- 
SELECT author,
    title
FROM book
WHERE amount BETWEEN 2 AND 14
ORDER BY author DESC,
    title ASC;
-- 
SELECT title,
    author
FROM book
WHERE (
        title LIKE '_% _%'
        OR title LIKE '_% _% _%'
    )
    AND (
        author LIKE '%.С.'
        OR author LIKE '% С.%'
    )
ORDER BY title;
--
SELECT "Дарья Донцова" AS 
author,
CONCAT("Таня Гроттер и ", title) AS title,
ROUND(price * 1.42, 2) AS price
FROM book
ORDER BY price DESC,
    title DESC;
-- 
SELECT DISTINCT amount
from book;
-- 
SELECT (
        DISTINCT author AS 'Автор',
        COUNT(title) AS 'Различных_книг',
        SUM(amount) AS 'Количество_экземпляров'
    )
FROM book
GROUP BY author;
--
SELECT 
author,
MIN(price) AS 'Минимальная_цена',
MAX(price) AS 'Максимальная_цена',
AVG(price) AS 'Средняя_цена'
FROM book
GROUP BY author;
--
SELECT author,
ROUND(Стоимость, 2) as Стоимость,
ROUND(НДС, 2) as НДС,
ROUND(Стоимость_без_НДС, 2) as Стоимость_без_НДС
FROM (
        SELECT Стоимость_без_НДС * 0.18 as НДС,
            with_Стоимость_без_НДС.*
        FROM (
                SELECT Стоимость / 1.18 AS Стоимость_без_НДС,
                    with_Стоимость.*
                FROM (
                        SELECT author,
                            SUM(amount * price) AS Стоимость
                        FROM book
                        GROUP BY author
                    ) AS with_Стоимость
            ) with_Стоимость_без_НДС
    ) with_НДС;
--
SELECT
SUM(DISTINCT price) COUNT(
    DISTINCT price AS Стоимость_всех_книг_по_одному_экз
),
FROM book;
-- 
SELECT author,
    Минимальная_цена,
    Число_книг
FROM (
        SELECT author,
            COUNT(DISTINCT title) AS Количество_произведений,
            MIN(price) AS Минимальная_цена,
            SUM(amount) AS Число_книг,
            MAX(price) AS Максимальная_цена
        FROM book
        WHERE amount > 1
        GROUP BY author
    ) AS ok
WHERE Количество_произведений >= 2
    AND Максимальная_цена > 500;
-- WHERE | HAVING выражение оператор_сравнения (вложенный запрос);
-- WHERE | HAVING выражение, включающее вложенный запрос;
-- WHERE | HAVING выражение [NOT] IN (вложенный запрос);
-- WHERE | HAVING выражение  оператор_сравнения  ANY | ALL (вложенный запрос).
-- 
SELECT author,
    title,
    price
FROM book
WHERE price <= (
        SELECT AVG(price)
        from book
    )
ORDER BY price DESC;
-- 
SELECT author,
    title,
    amount
FROM book
WHERE title IN (
        SELECT MAX(title)
        FROM book
        GROUP BY amount
        HAVING COUNT(title) = 1
    );
-- 
SELECT author,
    title,
    price
FROM book
WHERE price < ALL (
        SELECT MAX(price)
        FROM book
        GROUP BY author
    );
-- 
SELECT author,
    title,
    price,
    amount,
    ROUND(
        (amount * price) / (
            SELECT SUM(amount * price)
            FROM book
        ) * 100,
        2
    ) as income_percent
FROM book
ORDER BY income_percent DESC;
-- Can override *
-- SELECT *,
--             amount * price / 
--             (SELECT SUM(amount*price) FROM book) 
--             AS percentage
--      FROM book 
--      ORDER BY percentage DESC
--      LIMIT 1
-- 
CREATE TABLE supply (
    supply_id INT PRIMARY KEY AUTO_INCREMENT,
    title VARCHAR(50),
    author VARCHAR(30),
    price DECIMAL(8, 2),
    amount INT
);
--
INSERT INTO book (title, author, price, amount) 
VALUES ('Лирика', 'Пастернак Б.Л.', 518.99, 2),
    ('Черный человек', 'Есенин С.А.', 570.20, 6),
    ('Белая гвардия', 'Булгаков М.А.', 540.50, 7),
    ('Идиот', 'Достоевский Ф.М.', 360.80, 3);
-- 
INSERT INTO book (title, author, price, amount)
SELECT title,
    author,
    price,
    amount
FROM supply
WHERE author NOT IN ('Булгаков М.А.', 'Достоевский Ф.М.');
-- 
INSERT INTO book (title, author, price, amount)
SELECT title,
    author,
    price,
    amount
FROM supply
WHERE author NOT IN (
        SELECT author
        FROM book
    );
SELECT *
FROM book;
-- 
UPDATE book
SET price = 0.9 * price
WHERE amount BETWEEN 5 AND 10;
-- 
UPDATE book
SET price = IF(buy = 0, 0.9 * price, price),
    buy = LEAST(buy, amount);
-- 
UPDATE book,
    supply
SET book.amount = book.amount + supply.amount,
    price = (book.price + supply.price) / 2
WHERE book.title = supply.title
    AND book.author = supply.author;
SELECT *
FROM book;
-- 
DELETE FROM supply
WHERE author IN (
        SELECT author
        FROM book
        GROUP BY author
        HAVING SUM(amount) > 10
    );
-- 
CREATE TABLE ordering AS
SELECT author,
    title,
    (
        SELECT AVG(amount)
        FROM book
    ) as amount
FROM book
WHERE amount < (
        SELECT AVG(amount)
        FROM book
    );
-- 
SELECT name,
    city,
    per_diem,
    date_first,
    date_last
FROM trip
WHERE name LIKE '%а %'
ORDER BY date_last DESC;
-- 
SELECT DISTINCT name
FROM trip
WHERE city = 'Москва'
ORDER BY name;
-- 
SELECT city,
    COUNT(trip_id) as Количество
FROM trip
GROUP BY city
ORDER BY city;
-- 
SELECT city,
    COUNT(name) as Количество
FROM trip
GROUP BY city
ORDER BY COUNT(name) DESC
LIMIT 2;
--
SELECT name, city, DATEDIFF(date_last, date_first) + 1 as Длительность
FROM trip
WHERE city NOT IN ('Москва', 'Санкт-Петербург')
ORDER BY Длительность DESC,
    city DESC;
--
SELECT name, city, date_first, date_last
FROM trip
WHERE DATEDIFF(date_last, date_first) = (
        SELECT MIN(DATEDIFF(date_last, date_first))
        FROM trip
    );
--
SELECT name, city, date_first, date_last
FROM trip
WHERE MONTH(date_first) = MONTH(date_last)
ORDER BY city,
    name;
-- 
SELECT name,
    city,
    date_first,
    (DATEDIFF(date_last, date_first) + 1) * per_diem AS Сумма
FROM trip
WHERE MONTHNAME(date_first) IN ('February', 'MARCH')
    AND YEAR(date_first) = 2020
ORDER BY name,
    Сумма DESC;
--
SELECT name,
SUM((DATEDIFF(date_last, date_first) + 1) * per_diem) AS Сумма
FROM trip
GROUP BY name
HAVING COUNT(city) > 3
ORDER BY Сумма DESC;
-- 
CREATE TABLE fine (
    fine_id INT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(30),
    number_plate VARCHAR(6),
    violation VARCHAR(50),
    sum_fine DECIMAL(8, 2),
    date_violation DATE,
    date_payment DATE
);
--
INSERT INTO fine (name, number_plate, violation, sum_fine, date_violation, date_payment ) VALUES
(
    'Баранов П.Е.',
    'Р523ВТ',
    'Превышение скорости(от 40 до 60)',
    NULL,
    '2020-02-14',
    NULL
),
(
    'Абрамова К.А.',
    'О111АВ',
    'Проезд на запрещающий сигнал',
    NULL,
    '2020-02-23',
    NULL
),
(
    'Яковлев Г.Р.',
    'Т330ТТ',
    'Проезд на запрещающий сигнал',
    NULL,
    '2020-03-03',
    NULL
);
-- 
UPDATE fine f,
    traffic_violation tf
SET f.sum_fine = tf.sum_fine
WHERE f.sum_fine IS NULL;
--
-- https://stepik.org/lesson/305762/step/5?unit=287773
-- Важно! В разделе GROUP BY нужно перечислять все НЕАГРЕГИРОВАННЫЕ столбцы (к которым не применяются групповые функции) из SELECT.
-- 
CREATE TABLE prev AS
SELECT name,
    number_plate,
    violation
FROM fine
GROUP BY name,
    number_plate,
    violation
HAVING COUNT(date_violation) >= 2
ORDER BY name,
    number_plate,
    violation;
UPDATE fine,
    prev
SET sum_fine = sum_fine * 2
WHERE name IN (
        SELECT name
        FROM prev
    )
    AND date_payment IS NULL;
--
UPDATE fine, payment
SET fine.date_payment = payment.date_payment
WHERE (
        fine.name,
        fine.number_plate,
        fine.violation,
        fine.date_violation
    ) = (
        payment.name,
        payment.number_plate,
        payment.violation,
        payment.date_violation
    );
UPDATE fine,
    payment
SET fine.sum_fine = fine.sum_fine / 2
WHERE (
        fine.name,
        fine.number_plate,
        fine.violation,
        fine.date_violation
    ) = (
        payment.name,
        payment.number_plate,
        payment.violation,
        payment.date_violation
    )
    AND DATEDIFF(fine.date_payment, fine.date_violation) <= 20;
-- 
CREATE TABLE back_payment AS
SELECT name,
    number_plate,
    violation,
    sum_fine,
    date_violation
FROM fine
WHERE date_payment IS NULL;
DELETE TABLE fine
WHERE DATEDIFF("2020-02-01", date_violation) > 0;
-- 
CREATE TABLE author (
    author_id INT PRIMARY KEY AUTO_INCREMENT,
    name_author VARCHAR(50)
);
--
INSERT INTO author ( name_author) VALUES
('Булгаков М.А.'),
('Достоевский Ф.М.'),
('Есенин С.А.'),
('Пастернак Б.Л.');
-- 
CREATE TABLE book (
    book_id INT PRIMARY KEY AUTO_INCREMENT,
    title VARCHAR(50),
    author_id INT,
    genre_id INT,
    price DECIMAL(8, 2),
    amount INT,
    FOREIGN KEY (author_id) REFERENCES author(author_id),
    FOREIGN KEY (genre_id) REFERENCES genre(genre_id)
);
--
-- author ---> book 
-- one to many
-- author is main, book is connected, or subordinate
-- author - главная
-- book - зависимая
-- 
CREATE TABLE book (
    book_id INT PRIMARY KEY AUTO_INCREMENT,
    title VARCHAR(50),
    author_id INT NOT NULL,
    price DECIMAL(8, 2),
    amount INT,
    FOREIGN KEY (author_id) REFERENCES author (author_id) ON DELETE CASCADE,
    FOREIGN KEY (genre_id) REFERENCES genre(genre_id) ON DELETE
    SET NULL
);
--
INSERT INTO book ( title, author_id, genre_id, price, amount  ) VALUES
('Стихотворения и поэмы', 3, 2, 650.00, 15),
('Черный человек', 3, 2, 570.20, 6),
('Лирика', 4, 2, 518.99, 2);
-- 
SELECT title,
    name_genre,
    price
FROM genre
    INNER JOIN book ON genre.genre_id = book.genre_id
    INNER JOIN author ON author.author_id = book.author_id
WHERE book.amount > 8
ORDER BY price DESC;
--
-- https://www.geeksforgeeks.org/sql-join-set-1-inner-left-right-and-full-joins/
-- LEFT OUTER JOIN
-- INNER + LEFT \ RIGHT
-- RIGHT OUTER JOIN
-- INNER + RIGHT \ LEFT
-- 
SELECT name_genre
FROM genre LEFT JOIN book ON genre.genre_id = book.genre_id
WHERE book.genre_id IS NULL
-- 
SELECT name_city, name_author, '2020-12-31' - INTERVAL FLOOR(RAND() * 14) DAY AS Дата FROM
    city CROSS JOIN author
    ORDER BY name_city, Дата DESC