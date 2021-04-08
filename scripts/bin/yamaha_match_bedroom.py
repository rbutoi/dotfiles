#!/usr/bin/env python3
# match volumes

import requests

receiver_api = 'http://192.168.1.17/YamahaExtendedControl/v1/main'
bedroom_api  =  'http://192.168.1.9/YamahaExtendedControl/v1/main'
EXTRA_BEDROOM_FACTOR = 1.3

receiver = requests.get(f'{receiver_api}/getStatus').json()
bedroom  = requests.get(f'{bedroom_api}/getStatus') .json()
bedroom_vol = int(receiver['volume'] / receiver['max_volume']
                  * bedroom['max_volume'] * EXTRA_BEDROOM_FACTOR)
requests.get(f'{bedroom_api}/setVolume?volume={bedroom_vol}')
bedroom_new = requests.get(f'{bedroom_api}/getStatus').json()

print(f'''\
receiver volume: {receiver['actual_volume']['value']}{receiver['actual_volume']['unit']}
bedroom volume before: {bedroom['volume']} / {bedroom['max_volume']}
bedroom volume: {bedroom_new['volume']} / {bedroom_new['max_volume']}
''')
