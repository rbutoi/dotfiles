#!/usr/bin/env python3
# match volumes

import requests

receiver_api = 'http://192.168.1.17/YamahaExtendedControl/v1/main'
bedroom_api  =  'http://192.168.1.107/YamahaExtendedControl/v1/main'
EXTRA_BEDROOM_FACTOR = 1.2

receiver = requests.get(f'{receiver_api}/getStatus').json()
bedroom  = requests.get(f'{bedroom_api}/getStatus') .json()
bedroom_vol = int(receiver['volume'] / receiver['max_volume']
                  * bedroom['max_volume'] * EXTRA_BEDROOM_FACTOR)
requests.get(f'{bedroom_api}/setVolume?volume={bedroom_vol}')
bedroom_new = requests.get(f'{bedroom_api}/getStatus').json()

print(f"receiver: {receiver['actual_volume']['value']}{receiver['actual_volume']['unit']}")
print(f"bedroom : {bedroom_new['volume']} / {bedroom_new['max_volume']}")
if bedroom['volume'] != bedroom_new['volume']:
    print(f"bedroom before: {bedroom['volume']} / {bedroom['max_volume']}")
