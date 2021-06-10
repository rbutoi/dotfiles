#!/usr/bin/env python
# requires pacman packages python-pandas, python-plotly (AUR)

import pandas as pd
import matplotlib.pyplot as plt
# perhaps for interactive browser-based plots? :
# import plotly.express as px

df_og = pd.read_csv('~/google-drive/computer things/yamaha_vol_history.csv',
                 parse_dates=['timestamp'])
df_wd = df_og[df_og['timestamp'].dt.weekday <  5]
df_we = df_og[df_og['timestamp'].dt.weekday >= 5]

# fix something about the lack of data making x axis different sizes
ax = df_wd.groupby([df_wd['timestamp'].dt.hour,
                    df_wd['timestamp'].dt.minute]).mean().rolling(12).mean().plot()
df_we.groupby([df_we['timestamp'].dt.hour,
               df_we['timestamp'].dt.minute]).mean().rolling(12).mean().plot(ax = ax)
plt.show()
