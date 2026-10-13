"""Verify the reviewed file manifest, then create a draft with a ZIP and checksum."""
import csv
import hashlib
import json
import os
from pathlib import Path
import urllib.request
import zipfile

root = Path.cwd()
archive = root.parent / 'Make_Thin_Section_V70_Lite.zip'
with (root / 'PACKAGE_SHA256.csv').open(encoding='utf-8-sig', newline='') as f:
    records = list(csv.DictReader(f))
names = []
for row in records:
    file = (root / row['Path']).resolve()
    if root not in file.parents or not file.is_file():
        raise RuntimeError('Invalid manifest path')
    data = file.read_bytes()
    if len(data) != int(row['Bytes']) or hashlib.sha256(data).hexdigest().upper() != row['SHA256'].upper():
        raise RuntimeError('Checksum mismatch: ' + row['Path'])
    names.append(row['Path'])
with zipfile.ZipFile(archive, 'w', zipfile.ZIP_DEFLATED, compresslevel=9) as z:
    for name in names + ['PACKAGE_SHA256.csv']:
        z.write(root / name, 'Make_Thin_Section_V70_Lite/' + name)
checksum = hashlib.sha256(archive.read_bytes()).hexdigest()
checksum_file = root.parent / 'SHA256SUMS.txt'
checksum_file.write_text(checksum + '  ' + archive.name + '\n', encoding='ascii')
repo = os.environ['GITHUB_REPOSITORY']
headers = {'Authorization': 'Bearer ' + os.environ['GH_RELEASE_TOKEN'],
           'Accept': 'application/vnd.github+json', 'X-GitHub-Api-Version': '2022-11-28'}
api = 'https://api.github.com/repos/' + repo
def request(url, data=None, content_type='application/json'):
    req = urllib.request.Request(url, data=data, headers={**headers, 'Content-Type': content_type})
    with urllib.request.urlopen(req) as response:
        return json.load(response)
existing = request(api + '/releases?per_page=100')
if any(r['tag_name'] == 'v0.70' for r in existing):
    raise RuntimeError('v0.70 already exists; refusing to replace a release.')
body = (root / 'RELEASE_NOTES.md').read_text(encoding='utf-8')
body += '\n\nInstallation and verification details are included in START_HERE.md and VALIDATION.md.\n'
release = request(api + '/releases', json.dumps({
    'tag_name': 'v0.70', 'target_commitish': os.environ['GITHUB_SHA'],
    'name': 'Make Thin Section V70 Lite', 'body': body,
    'draft': True, 'prerelease': False}).encode())
upload = release['upload_url'].split('{')[0]
for file, mime in [(archive, 'application/zip'), (checksum_file, 'text/plain')]:
    request(upload + '?name=' + file.name, file.read_bytes(), mime)
with open(os.environ['GITHUB_STEP_SUMMARY'], 'a', encoding='utf-8') as f:
    f.write('Draft release: ' + release['html_url'] + '\n\nZIP bytes: ' + str(archive.stat().st_size) + '\n')
print('Draft ready:', release['html_url'])
