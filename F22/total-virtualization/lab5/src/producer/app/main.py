from datetime import datetime as dt
import sys
import os
import argparse
from time import sleep
import pika
from contextlib import closing
import json


parser = argparse.ArgumentParser()
parser.add_argument("--mq-host", help="RabbitMQ host")
parser.add_argument("--mq-port", help="RabbitMQ port")
parser.add_argument("--mq-queue", help="RabbitMQ queue")
args = parser.parse_args()

if __name__ == "__main__":
    for i in range(100):
        try:
            with closing(
                pika.BlockingConnection(
                    pika.ConnectionParameters(
                        host=args.mq_host,
                        port=args.mq_port,
                        connection_attempts=10,
                        retry_delay=5,
                    )
                )
            ) as connection:
                with connection.channel() as channel:
                    channel.queue_declare(queue=args.mq_queue)
                    for i in range(10000):
                        try:
                            now = dt.now().strftime("%H:%M:%S")
                            message_type = 1 if i % 2 == 0 else 2
                            message = {
                                "id": i,
                                "timestamp": dt.now().isoformat(),
                                "type": message_type,
                            }
                            channel.basic_publish(
                                exchange="",
                                routing_key=args.mq_queue,
                                body=json.dumps(message),
                            )
                            print(
                                f" [x] Sent message {message['id']} of type {message['type']} at {message['timestamp']} "
                            )
                            sleep(1)
                        except KeyboardInterrupt:
                            print("Interrupted")
                            try:
                                sys.exit(0)
                            except SystemExit:
                                os._exit(0)
                        except Exception as exc:
                            print(exc)
                            print("Stop sending. Retrying to connect to RabbitMQ")
                            break
        except:
            print("Retrying to connect to RabbitMQ")
            sleep(2)
