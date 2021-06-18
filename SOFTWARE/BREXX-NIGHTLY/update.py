#!/usr/bin/env python3

import json
import urllib.request
#import os

user = "mainframed"
repo = "brexx-nightly"

_json = json.loads(urllib.request.urlopen(urllib.request.Request(
    'https://api.github.com/repos/{u}/{r}/releases/latest'.format(u=user,r=repo),
     headers={'Accept': 'application/vnd.github.v3+json'},
)).read())

asset = _json['assets'][0]
 #print(json.dumps(_json, indent=4, sort_keys=True))
print(_json['assets'][0]['name'])

urllib.request.urlretrieve(asset['browser_download_url'], asset['name'])

