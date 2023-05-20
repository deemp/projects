import sys
import os
import json
from datetime import datetime as dt
from contextlib import closing
from time import sleep
import argparse
import psycopg2
import pika


parser = argparse.ArgumentParser()
parser.add_argument("--mq-host", help="RabbitMQ host")
parser.add_argument("--mq-port", help="RabbitMQ port")
parser.add_argument("--mq-queue", help="RabbitMQ queue")
parser.add_argument("--db-name", help="PostgreSQL database name")
parser.add_argument("--db-user", help="PostgreSQL database user")
parser.add_argument("--db-pass", help="PostgreSQL database password")
parser.add_argument("--db-host", help="PostgreSQL database host")
parser.add_argument("--db-port", help="PostgreSQL database port")
parser.add_argument("--db-table-1", help="PostgreSQL database table 1")
parser.add_argument("--db-table-2", help="PostgreSQL database table 2")
parser.add_argument(
    "--db-column-received", help="PostgreSQL database column for received timestamps"
)
parser.add_argument(
    "--db-column-sent", help="PostgreSQL database column for sent timestamps"
)
args = parser.parse_args()


def main():
    with closing(
        pika.BlockingConnection(
            pika.ConnectionParameters(
                host=args.mq_host,
                port=args.mq_port,
                connection_attempts=10,
                retry_delay=5,
            )
        )
    ) as pika_conn:
        with pika_conn.channel() as channel:
            print(f"Connected to RabbitMQ")
            for j in range(10):
                try:
                    print(f"Attempt {j}. Connecting to PostgreSQL")
                    conn = psycopg2.connect(
                        dbname=args.db_name,
                        user=args.db_user,
                        password=args.db_pass,
                        host=args.db_host,
                        port=args.db_port,
                    )
                    print("Connected to PostgreSQL!")

                    def callback(ch, method, properties, body):
                        now = dt.now().isoformat()
                        message = json.loads(body.decode("utf-8"))
                        sent = dt.fromisoformat(message["timestamp"])
                        print(
                            f"[x] Received a message with id {message['id']} of type {message['type']} at {now}. This message was sent at {sent.isoformat()}"
                        )
                        target_db_table = (
                            args.db_table_1 if message["type"] == 1 else args.db_table_2
                        )
                        try:
                            with conn:
                                with conn.cursor() as cursor:
                                    cursor.execute(
                                        f"INSERT INTO {target_db_table} ({args.db_column_received}, {args.db_column_sent}) VALUES (%s, %s)",
                                        [now, sent],
                                    )
                            with conn:
                                with conn.cursor() as cursor:
                                    cursor.execute(
                                        f"SELECT * FROM {target_db_table} order by id desc limit 5"
                                    )
                                    print(f"Last 5 rows of table {target_db_table}")
                                    for row in cursor:
                                        print(row)
                        except (Exception, psycopg2.Error) as error:
                            print("Error communicating with PostgreSQL!", error)

                    for j in range(5):
                        print(f"Attempt {j}. Reading from MQ")
                        try:
                            channel.queue_declare(queue=args.mq_queue)
                            channel.basic_consume(
                                queue=args.mq_queue,
                                on_message_callback=callback,
                                auto_ack=True,
                            )
                            print(" [*] Waiting for messages. To exit press CTRL+C")
                            channel.start_consuming()
                        except KeyboardInterrupt:
                            print("Interrupted!")
                            try:
                                sys.exit(0)
                            except SystemExit:
                                os._exit(0)
                        except Exception as exc:
                            print(exc)
                            print("Retrying")
                            sleep(1)
                        break
                    conn.close()
                except:
                    print("Failed to connect to PostgreSQL!")
                    sleep(5)


if __name__ == "__main__":
    main()
