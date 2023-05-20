import requests as req
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--address", help="Server address")
parser.add_argument("--n-requests", help="Number of requests", type=int)
args = parser.parse_args()

counter = [0, 0, 0]

apache = "apache"

for i in range(args.n_requests):
    try:
        resp = req.get(args.address, timeout=0.2)
        server_id = resp.text[resp.text.index(apache) + len(apache) + 1]
        num = int(server_id)
        counter[num - 1] += 1
    except ValueError:
        print("Server id not found")
    if i > 0 and (i + 1) % 100 == 0 or i == args.n_requests - 1:
        requests_made = i + 1
        print(f"Total planned requests: {args.n_requests}")
        print(f"Total made requests: {requests_made}")
        requests_successful = sum(counter)
        print(f"Total failed requests: {requests_made - requests_successful}")
        print(f"Total successful requests: {requests_successful}")
        print("Requests distribution:")
        for i in range(3):
            print(f"Server {i + 1}: {counter[i]} requests")
