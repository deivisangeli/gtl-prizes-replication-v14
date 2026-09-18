"""Upload the replication package to its Zenodo deposit (authors' tool; not part
of the replication run).

    ZENODO_TOKEN=... python tests/zenodo_upload.py [--publish]

Builds a zip of the committed files (git archive of HEAD), uploads it to the
deposit reserved for this package (DOI 10.5281/zenodo.22721355), replacing any
file of the same name, and prints the deposit's state. Nothing is published
unless --publish is passed: publishing is permanent on Zenodo (files cannot be
removed afterwards, only new versions added), so it is a separate, explicit step.
"""
import os, subprocess, sys, json
import requests

DEPOSIT_ID = 22721355
API = "https://zenodo.org/api"
token = os.environ.get("ZENODO_TOKEN")
if not token:
    sys.exit("set ZENODO_TOKEN (a Zenodo personal access token with deposit:write)")
auth = {"Authorization": f"Bearer {token}"}

root = subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()
os.chdir(root)
sha = subprocess.check_output(["git", "rev-parse", "--short", "HEAD"], text=True).strip()
zip_name = "gtl-prizes-replication-v14.zip"
subprocess.check_call(["git", "archive", "--format=zip", "--prefix=gtl-prizes-replication-v14/",
                       "-o", zip_name, "HEAD"])
size_mb = os.path.getsize(zip_name) / 1e6
print(f"built {zip_name} from {sha}: {size_mb:.1f} MB")

r = requests.get(f"{API}/deposit/depositions/{DEPOSIT_ID}", headers=auth)
r.raise_for_status()
dep = r.json()
print("deposit:", dep.get("title") or dep["metadata"].get("title"), "| state:", dep["state"],
      "| submitted:", dep["submitted"], "| doi:", dep.get("metadata", {}).get("prereserve_doi", {}).get("doi"))
if dep["submitted"]:
    sys.exit("deposit already published; create a new version on Zenodo instead of re-uploading")

for f in dep.get("files", []):
    if f["filename"] == zip_name:
        print("removing previous", zip_name)
        requests.delete(f"{API}/deposit/depositions/{DEPOSIT_ID}/files/{f['id']}", headers=auth).raise_for_status()

bucket = dep["links"]["bucket"]
with open(zip_name, "rb") as fh:
    r = requests.put(f"{bucket}/{zip_name}", data=fh, headers=auth)
r.raise_for_status()
print("uploaded:", r.json()["key"], r.json()["size"], "bytes, checksum", r.json()["checksum"])

if "--publish" in sys.argv:
    r = requests.post(f"{API}/deposit/depositions/{DEPOSIT_ID}/actions/publish", headers=auth)
    r.raise_for_status()
    print("PUBLISHED:", r.json()["doi_url"])
else:
    print("not published (pass --publish to make the record public; this cannot be undone)")
