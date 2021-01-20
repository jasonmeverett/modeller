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
sys.path.append('/workspaces/modeller_gh/build')
import Modeller as M

cfg = {

    "ball1":{
        "mass": np.random.uniform(0.1, 10.0),
        "radius": 0.1,
        "r0": [
            np.random.normal(), 
            np.random.normal(), 
            np.random.normal()
        ],
        "v0": [
            0.2*np.random.normal(), 
            0.2*np.random.normal(), 
            0.2*np.random.normal()
        ]
    },

    "ball2":{
        "mass": np.random.uniform(0.1, 10.0),
        "radius": 0.1,
        "r0": [
            np.random.normal(), 
            np.random.normal(), 
            np.random.normal()
        ],
        "v0": [
            0.2*np.random.normal(), 
            0.2*np.random.normal(), 
            0.2*np.random.normal()
        ]
    },

    "spring1":{
        "k"     : np.random.uniform(100, 500),
        "x0"    : np.random.uniform(0.5, 1.5),
        "c"     : np.random.uniform(1.1, 2.0),
    },

    "spring2":{
        "k"     : np.random.uniform(100, 500),
        "x0"    : np.random.uniform(0.5, 1.5),
        "c"     : np.random.uniform(1.1, 2.0),
    },

    "spring1_cut_time" : 0.6 + 0.01*np.random.normal(),
    "spring2_cut_time" : 1.2 + 0.01*np.random.normal(),

    "sim_time":2.0,
    "use_viz":True,
    "use_grav":False,
    "plot_out":False,
    "plot_energy":True,
    "viz_update_rate": 0.01,
    "output_dataset_name": "state_info",
    "output_data_rate": 0.001,
    "integ_acc":1e-8
}

print("Spring1 cut time: ", cfg["spring1_cut_time"])
print("Spring2 cut time: ", cfg["spring2_cut_time"])

print("Running...")
SimOut = M.examples.Run_Ex3(cfg)
out_data = np.array(
    SimOut.getDataBase().getDataSetByName("state_info").getData()
)


if cfg["plot_energy"]:
    traces = []
    traces.append(
        go.Scatter(
            x = out_data[:,0],
            y = out_data[:,7],
            mode = 'lines',
            name = 'Energy',
        )
    )
    traces.append(
        go.Scatter(
            x = out_data[:,0],
            y = out_data[:,8],
            mode = 'lines',
            name = 'Kinetic',
        )
    )
    traces.append(
        go.Scatter(
            x = out_data[:,0],
            y = out_data[:,9],
            mode = 'lines',
            name = 'Potential',
        )
    )

    fig0 = go.Figure(data=traces)
    fig0.update_layout({
        "title": {
            "text":"Energies",
            "x":0.5
        },
        "xaxis_title": "Time (sec)",
        "yaxis_title":"Energy (J)",
    })
    fig0.write_html("ex3_energy.html")
    fig0.show()


if cfg["plot_out"]:

    # Plot the output
    traces = []
    traces.append(
        go.Scatter3d(
            x = out_data[:,3],
            y = out_data[:,1],
            z = out_data[:,2],
            mode = 'lines',
            name = 'Pendulum 1 Position',
            line = {
                'width': 1
            }
        )
    )

    traces.append(
        go.Scatter3d(
            x = out_data[:,6],
            y = out_data[:,4],
            z = out_data[:,5],
            mode = 'lines',
            name = 'Pendulum 2 Position',
            line = {
                'width': 1
            }
        )
    )

    fig1 = go.Figure(data=traces)
    fig1.update_layout({
        "title": {
            "text":"Pendulum Positions",
            "x":0.5
        },
        "xaxis_title": "X Position (m)",
        "yaxis_title":"Y Position (m)",
        "scene_aspectmode":"data"
    })
    fig1.write_html("ex3_pos.html")
    fig1.show()


