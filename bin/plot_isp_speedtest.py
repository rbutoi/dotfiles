#!/usr/bin/env python
# requires pacman packages python-pandas, python-plotly (AUR)

import pandas as pd
import plotly.express as px

px.line(pd.read_csv('~/google-drive/computer things/isp_speedtest.csv'),
       x='Timestamp', y = ['Download', 'Upload'], title='FiOS over time') \
    .update_layout(barmode='group') \
    .show()
