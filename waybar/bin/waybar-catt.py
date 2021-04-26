#!/usr/bin/env python3

from html import escape
import datetime
import json
import subprocess

output = json.loads(subprocess.run("catt info -j", shell=True,
                                   capture_output=True,
                                   encoding='utf-8').stdout)
if 'duration' not in output:
    exit(0)

total_length = datetime.timedelta(seconds=int(output['duration']))
current_time = datetime.timedelta(seconds=int(output['current_time']))
subtitle = escape(output['media_metadata']['subtitle'])
title = ''
if 'seriesTitle' in output['media_metadata']:
    title = escape(output['media_metadata']['seriesTitle'])
elif 'title' in output['media_metadata']:
    title = escape(output['media_metadata']['title'])
if 'studio' in output['media_metadata']:
    subtitle += f" ({output['media_metadata']['studio']})"
    status_text = escape(output['status_text'])

data = {}
data['text'] = f'{title} - {subtitle}'
data['tooltip'] = "{} ({}/{})\n{}\n{}".format(
    title, current_time, total_length, subtitle, status_text)
print(json.dumps(data))
