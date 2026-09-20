"""Upload the replication package to its Zenodo record (authors' tool; not part
of the replication run).

    ZENODO_TOKEN=... python tests/zenodo_upload.py [--publish]

Builds a zip of the committed files (git archive of HEAD) and uploads it to the
package's Zenodo record (concept DOI 10.5281/zenodo.22721355). If the record is
still an unpublished draft, the zip replaces the draft's file. If the record is
already published, a new version is opened, the zip replaces its file, and the
version label is set to the commit. Nothing is published unless --publish is
passed: publishing is permanent on Zenodo (files cannot be removed afterwards,
only new versions added), so it is a separate, explicit step.
"""
import os, subprocess, sys
import requests

DEPOSIT_ID = 22721355
API = "https://zenodo.org/api"
token = os.environ.get("ZENODO_TOKEN")
if not token:
    sys.exit("set ZENODO_TOKEN (a Zenodo personal access token with deposit:write, and deposit:actions to publish)")
auth = {"Authorization": f"Bearer {token}"}

root = subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()
os.chdir(root)
sha = subprocess.check_output(["git", "rev-parse", "--short", "HEAD"], text=True).strip()
zip_name = "gtl-prizes-replication-v14.zip"
subprocess.check_call(["git", "archive", "--format=zip", "--prefix=gtl-prizes-replication-v14/",
                       "-o", zip_name, "HEAD"])
print(f"built {zip_name} from {sha}: {os.path.getsize(zip_name) / 1e6:.1f} MB")

r = requests.get(f"{API}/deposit/depositions/{DEPOSIT_ID}", headers=auth)
r.raise_for_status()
dep = r.json()
print("record:", dep["metadata"].get("title"), "| state:", dep["state"], "| submitted:", dep["submitted"])

if dep["submitted"]:
    # published: open (or reuse) the draft of the next version
    r = requests.post(f"{API}/deposit/depositions/{DEPOSIT_ID}/actions/newversion", headers=auth)
    r.raise_for_status()
    draft_url = r.json()["links"]["latest_draft"]
    dep = requests.get(draft_url, headers=auth).json()
    print("new version draft:", dep["id"])
dep_id = dep["id"]

for f in dep.get("files", []):
    print("removing", f["filename"])
    requests.delete(f"{API}/deposit/depositions/{dep_id}/files/{f['id']}", headers=auth).raise_for_status()
with open(zip_name, "rb") as fh:
    r = requests.put(f"{dep['links']['bucket']}/{zip_name}", data=fh, headers=auth)
r.raise_for_status()
print("uploaded:", r.json()["key"], r.json()["size"], "bytes, checksum", r.json()["checksum"])

meta = dep["metadata"]
meta["version"] = "v14-" + sha
for k in ("doi", "prereserve_doi"):
    meta.pop(k, None)
r = requests.put(f"{API}/deposit/depositions/{dep_id}", headers={**auth, "Content-Type": "application/json"},
                 json={"metadata": meta})
r.raise_for_status()

if "--publish" in sys.argv:
    r = requests.post(f"{API}/deposit/depositions/{dep_id}/actions/publish", headers=auth)
    r.raise_for_status()
    print("PUBLISHED:", r.json()["doi_url"], "| version", meta["version"])
else:
    print("not published (pass --publish to make it public; this cannot be undone)")
os.remove(zip_name)
