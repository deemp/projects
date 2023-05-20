# syntax=docker/dockerfile:1

FROM python:3.10-slim-buster

RUN apt-get update \
    && apt-get -y install libpq-dev gcc \
    && pip install psycopg2

WORKDIR /app

COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt

COPY app/main.py .

ENV PYTHONUNBUFFERED=1

ENTRYPOINT python main.py --mq-host ${MQ_HOST} --mq-port ${MQ_PORT} --mq-queue ${MQ_QUEUE} \
     --db-name ${POSTGRES_DB} --db-user ${POSTGRES_USER} --db-pass ${POSTGRES_PASSWORD} \
     --db-host ${DB_HOST} --db-port ${DB_PORT} --db-table-1 ${POSTGRES_TABLE_1} \
     --db-table-2 ${POSTGRES_TABLE_2} --db-column-received ${DB_COLUMN_RECEIVED} \
     --db-column-sent ${DB_COLUMN_SENT}

