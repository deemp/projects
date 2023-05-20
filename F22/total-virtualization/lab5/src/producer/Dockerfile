# syntax=docker/dockerfile:1

FROM python:3.10-slim-buster

WORKDIR /app

COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt

COPY app/main.py .

ENV PYTHONUNBUFFERED=1

ENTRYPOINT python main.py --mq-host ${MQ_HOST} --mq-port ${MQ_PORT} --mq-queue ${MQ_QUEUE}
