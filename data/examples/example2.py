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
sys.path.append('/workspaces/modeller/build')
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

    "use_viz": True,
    "gravity": True,
    "viz_update_rate": 0.01,
    "output_data_rate": 0.01,
    "output_dataset_name": "Ex2_Output",
    "sim_time": 200,
    "integ_accuracy":1e-8
}

# Run the example.
SimOut = M.examples.Run_Ex2(cfg)
DataBase = SimOut.getDataBase()
DataSet = DataBase.getDataSetByName(cfg["output_dataset_name"])
out_data = np.array( DataSet.getData() )   

aaa = 1

# Plot the output
traces = []
traces.append(
    go.Scatter(
        x = out_data[:,1],
        y = out_data[:,2],
        mode = 'lines',
        name = 'Pendulum 1 Position',
        line = {
            'width': 0.5
        }
    )
)

traces.append(
    go.Scatter(
        x = out_data[:,3],
        y = out_data[:,4],
        mode = 'lines',
        name = 'Pendulum 2 Position',
        line = {
            'width': 0.5
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
    "yaxis": {
        "title":"Y Position (m)",
        "scaleanchor":"x",
        "scaleratio":1
    }
})
fig1.write_html("ex2_positions.html")





# Plot the output
traces = []
traces.append(
    go.Scatter(
        x = out_data[:,0],
        y = out_data[:,5],
        mode = 'lines',
        name = 'Total Energy',
        line = {
            'width': 0.5
        }
    )
)

traces.append(
    go.Scatter(
        x = out_data[:,0],
        y = out_data[:,6],
        mode = 'lines',
        name = 'Kinetic Energy',
        line = {
            'width': 0.5
        }
    )
)

traces.append(
    go.Scatter(
        x = out_data[:,0],
        y = out_data[:,7],
        mode = 'lines',
        name = 'Potential Energy',
        line = {
            'width': 0.5
        }
    )
)

fig2 = go.Figure(data=traces)
fig2.update_layout({
    "title": {
        "text":"Pendulum Energy Dissipation",
        "x":0.5
    },
    "xaxis_title": "Time (s)",
    "yaxis_title": "Energy (J)"
})
fig2.write_html("ex2_energy.html")



fig1.show()
fig2.show()


# ------------------------------------------
#               Energy Dissipation Trades
# ------------------------------------------
# cc_range = np.linspace(0.0, 0.3, 31)
# traces = []

# for ii,cc in enumerate(cc_range):
#     print(cc)
#     cfg["pendulum1"]["damping_coeff"] = cc
#     cfg["pendulum2"]["damping_coeff"] = cc
#     SimOut = M.examples.Run_Ex2(cfg)
#     out_data = np.array( SimOut.getDataBase().getDataSetByName(cfg["output_dataset_name"]).getData() )   
#     pc = ( cc - np.min(cc_range) ) / (np.max(cc_range) - np.min(cc_range))
#     traces.append(
#         go.Scatter(
#             x = out_data[:,0],
#             y = out_data[:,5],
#             mode = 'lines',
#             name = 'Total Energy, Damping = %.2f'%(cc),
#             line = {
#                 "color": 'rgb(%d, %d, %d)'%(trunc(255*pc), 0, trunc(255*(1-pc)))
#             }
#         )
#     )


# fig3 = go.Figure(data=traces)
# fig3.update_layout({
#     "title": {
#         "text":"Pendulum Energy Dissipation over %.2f seconds"%(cfg["sim_time"]),
#         "x":0.5
#     },
#     "xaxis_title": "Time (s)",
#     "yaxis_title": "Energy (J)"
# })
# fig3.write_html("ex2_energy_trade.html")
# fig3.show()