import requests
import json
import os

namespace_url = input("Namespace URL: ")
namespace = input("Namespace name: ")

fetched1 = requests.get(namespace_url).json()
save_dir = f"json/{namespace}"
print(f"Saving to {save_dir}")
x = fetched1["result"]["namespaces"][namespace] or []
l = len(x)
order = []
os.mkdir(save_dir)
for i, ns in enumerate(x):
	name = ns["name"]
	if ns["repo"] != "https://github.com/caddyserver/caddy":
		print(f"[{i+1}/{l}] Skipping {name}, not in Caddy repo")
		continue
	fetched = requests.get(f"{namespace_url}/{name}").json()
	with open(f"{save_dir}/{name}.json", "w") as f:
		f.write(json.dumps(fetched["result"]["structure"]))
	order.append(name)
	print(f"[{i+1}/{l}] Saved {name}")
print("Done!")
for n in order:
	print(f"{n} => \"json/{namespace}/{n}.json\",")