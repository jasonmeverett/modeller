'''

Double pendulum (example pulled from Simbody manual)
7
'''



import sys
import numpy as np
import plotly.io as pio
pio.renderers.default = 'browser'
import plotly.graph_objects as go
import colour
from math import trunc
from math import log

# Locate Modeller and import.
sys.path.append('/modeller/build')
import Modeller as M

cfg = {
    "use_viz":False,
    "m0":100,
    "F":5000.0,
    "isp":250.0,
    "t_burn":20.0,
    "thr_kp":-30.0,
    "thr_kd":-8.0,
    "dt":0.01
}
print("Running...")
SimOut = M.examples.Run_Ex8(cfg)
db = SimOut.getDataBase()
out_data = np.array(
    SimOut.getDataBase().getDataSetByName("output").getData()
)

fig1 = go.Figure(
    data=[
        go.Scatter(
            x = out_data[:,0], 
            y = out_data[:,4],
            name = 'actual'
        ),
        go.Scatter(
            x = out_data[:,0], 
            y = out_data[:,5],
            name = 'commanded'
        )
    ],
    layout={
        'template':'plotly_dark',
        'title_text':'throttle responses'
    }
)

dm_dt = cfg["F"]/(9.801 * cfg["isp"])
dm = dm_dt*cfg["dt"]
dm_bysim = (out_data[0,1] - out_data[-1,1])
dm_bycalc = dm_dt*cfg["t_burn"]


dv_bymass = 9.801 * cfg["isp"] * log(out_data[0,3]/out_data[-1,3])
dv_byvel = out_data[-1,2]
print("dV By Mass:  ", dv_bymass)
print("dV By vel:   ", dv_byvel)
print("delta:       ", dv_byvel - dv_bymass)
print("delta (%):   ", abs(dv_bymass - dv_byvel)/dv_bymass*100, "%")

fig1.write_html("ex_8.html")
fig1.show()