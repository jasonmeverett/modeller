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

cfg = {
    "use_viz":True
}
print("Running...")
SimOut = M.examples.Run_Ex6(cfg)
out_data = np.array(
    SimOut.getDataBase().getDataSetByName("output").getData()
)


fig1 = go.Figure(
    data=[
        go.Scatter(
            x=out_data[:,0],
            y=out_data[:,1]
        )
    ]
)

fig1.write_html('ex6_b2vgo.html')


fig2 = go.Figure(
    data=[
        go.Scatter(
            x=out_data[:,0],
            y=out_data[:,3],
            name='TotalEnergy'
        ),
        go.Scatter(
            x=out_data[:,0],
            y=out_data[:,4],
            name='KineticEnergy'
        ),
        go.Scatter(
            x=out_data[:,0],
            y=out_data[:,5],
            name='PotentialEnergy'
        )
    ],
    layout={
        'title_text': 'Energies',
        'template':'plotly_dark'
    }
)

fig2.write_html('ex6_energy.html')

fig1.show()
fig2.show()