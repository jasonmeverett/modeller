'''

Double pendulum (example pulled from Simbody manual)

'''



import sys
import numpy as np
import plotly.io as pio
pio.renderers.default = 'browser'
import plotly.graph_objects as go

# Locate Modeller and import.
sys.path.append('/workspaces/modeller/build')
import Modeller as M

# Set up a configuration dictionary to pass to the example.
cfg_dict =  { 
    "pendulum1":
    {
        "length_m": 1.0,
        "endpoint_mass_kg":10.0,
        "initial_angular_rate_deg_s": 100.0
    },
    
    "pendulum2":
    {
        "length_m": 0.5,
        "endpoint_mass_kg":1.0,
        "initial_angular_rate_deg_s": 100.0
    },

    "use_viz": False,
    "viz_update_rate": 0.1,
    "output_data_rate": 0.01,
    "sim_time": 100
}

# Run the example.
out_dataset = M.examples.Run_Ex2(cfg_dict)
out_data = np.array(out_dataset.getData())

aaa = 1

# Plot the output
traces = []
traces.append(
    go.Scatter(
        x = out_data[:,1],
        y = out_data[:,2],
        mode = 'lines+markers',
        name = 'Pendulum 1 Position'
    )
)

traces.append(
    go.Scatter(
        x = out_data[:,3],
        y = out_data[:,4],
        mode = 'lines+markers',
        name = 'Pendulum 2 Position'
    )
)

fig = go.Figure(data=traces)
fig.update_layout({
    "title": "Pendulum Positions",
    "xaxis_title": "X Position (m)",
    "yaxis": {
        "title":"Y Position (m)",
        "scaleanchor":"x",
        "scaleratio":1
    }
})
fig.write_html("ex2.html")
fig.show()