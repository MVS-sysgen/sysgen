#!/usr/bin/env python3

import json
import urllib.request
import os

user = "mvslovers"
repo = "brexx370"

_json = json.loads(
            urllib.request.urlopen(
                urllib.request.Request(
                    'https://api.github.com/repos/{u}/{r}/releases/latest'.format(u=user,r=repo),
                    headers={'Accept': 'application/vnd.github.v3+json'},
                )
            ).read().decode('utf-8')
        )

asset = _json['assets'][0]

print(_json['tag_name'])
urllib.request.urlretrieve(asset['browser_download_url'], asset['name'])

