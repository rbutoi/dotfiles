#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt
import subprocess

subprocess.run('echo timestamp > /tmp/upgraded_times.csv', shell=True)
subprocess.run('grep upgraded /var/log/pacman.log | cut -c 2-25 | grep -P -- \'-0[45]00\' >> /tmp/upgraded_times.csv', shell=True)

df = pd.read_csv('/tmp/upgraded_times.csv', parse_dates=['timestamp'], date_parser=lambda col: pd.to_datetime(col, utc=True))
gb = df.groupby([df['timestamp'].dt.hour, df['timestamp'].dt.minute]).count().rolling(5).mean().plot()
plt.show()
