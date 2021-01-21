'''

Double pendulum (example pulled from Simbody manual)

'''



import sys
import numpy as np
import plotly.io as pio
pio.renderers.default = 'browser'
import plotly.graph_objects as go
import colour
from math import trunc

# Locate Modeller and import.
sys.path.append('/modeller_gh/build')
import Modeller as M

cfg = {}
print("Running...")
SimOut = M.examples.Run_Ex5(cfg)
out_data = np.array(
    SimOut.getDataBase().getDataSetByName("output").getData()
)


fig1 = go.Figure(
    data=[
        go.Scatter(
            x = out_data[:,0],
            y = out_data[:,1],
            mode = 'lines',
            name = 'Energy'
        )
    ]
)
fig1.write_html("ex5_plot.html")
fig1.show()

