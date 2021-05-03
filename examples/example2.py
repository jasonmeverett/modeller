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
sys.path.append('/modeller/build')
import Modeller as M

# Set up a configuration dictionary to pass to the example.
cfg =  { 
    "pendulum1":
    {
        "length_m": 1.0,
        "endpoint_mass_kg":1.0,
        "initial_angular_rate_deg_s": 200.0,
        "damping_coeff": 0.1
    },
    
    "pendulum2":
    {
        "length_m": 1.0,
        "endpoint_mass_kg":1.0,
        "initial_angular_rate_deg_s": 200.0,
        "damping_coeff": 0.1
    },

    "gravity": True,
    "viz_update_rate": 0.01,
    "output_data_rate": 0.01,
    "output_dataset_name": "Ex2_Output",
    "sim_time": 50,
    "integ_accuracy":1e-8
}

# Run the example.
SimOut = M.examples.Run_Ex2(cfg)
DataBase = SimOut.getDataBase()
DataSet = DataBase.getDataSetByName(cfg["output_dataset_name"])
out_data = np.array( DataSet.getData() )   


# Plot the output
traces = []
traces.append(go.Scatter(
    x = out_data[:,1],
    y = out_data[:,2],
    mode = 'lines',
    name = 'Pendulum 1 Position',
    line = { 'width': 2, 'color':'red' }
))

traces.append(go.Scatter(
    x = out_data[:,3],
    y = out_data[:,4],
    mode = 'lines',
    name = 'Pendulum 2 Position',
    line = { 'width': 2, 'color':'white' }
))

fig1 = go.Figure(data=traces,layout = {
    "template":"plotly_dark",
    "title": {
        "text":"Pendulum Positions",
        "xanchor":"center",
        "x":0.5
    },
    "xaxis_title": "X Position (m)",
    "yaxis": {
        "title":"Y Position (m)",
        "scaleanchor":"x",
        "scaleratio":1
    }
})

fig1.write_html("ex2_positions.html")





# Plot the output
traces = []
traces.append(go.Scatter(
    x = out_data[:,0],
    y = out_data[:,5],
    mode = 'lines',
    name = 'Total Energy',
    line = { 'width': 2, 'color':'white' }
))

traces.append(go.Scatter(
    x = out_data[:,0],
    y = out_data[:,6],
    mode = 'lines',
    name = 'Kinetic Energy',
    line = { 'width': 2, 'color':'green' }
))

traces.append(go.Scatter(
    x = out_data[:,0],
    y = out_data[:,7],
    mode = 'lines',
    name = 'Potential Energy',
    line = { 'width': 2, 'color':'blue' }
))

fig2 = go.Figure(data=traces,layout={
    "template":"plotly_dark",
    "title": {
        "text":"Pendulum Energy Dissipation",
        "xanchor":"center",
        "x":0.5
    },
    "xaxis_title": "Time (s)",
    "yaxis_title": "Energy (J)"
})
fig2.write_html("ex2_energy.html")

fig1.show()
fig2.show()
