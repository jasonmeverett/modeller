'''
 ------------------------------------------------------------------------------------
 
 
         ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗     ███████╗██████╗ 
         ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║     ██╔════╝██╔══██╗
         ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║     █████╗  ██████╔╝
         ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║     ██╔══╝  ██╔══██╗
         ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████╗███████╗██║  ██║
         ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝

                                      Ex1
                                      ---
                     A simple prescribed-acceleration oscillator.
 
 ------------------------------------------------------------------------------------
'''


 
import sys
import numpy as np
import plotly.io as pio
pio.renderers.default = 'browser'
import plotly.graph_objects as go

# Locate Modeller and import.
sys.path.append('/modeller_gh/build')
import Modeller as M

# Set up a configuration dictionary to pass to the example.
cfg_dict = {
    "amplitude": 5.0,
    "frequency_Hz": 1.0,
    "data_output_frequency": 0.01,
    "integ_accuracy": 1e-8,
    "total_sim_time": 10.0
}

# Run the example.
out_data = np.array(M.examples.Run_Ex1(cfg_dict))


# Plot the output
traces = []
traces.append(
    go.Scatter(
        x = out_data[:,0],
        y = out_data[:,1],
        mode = 'lines+markers',
        name = 'Position'
    )
)

traces.append(
    go.Scatter(
        x = out_data[:,0],
        y = out_data[:,2],
        mode = 'lines+markers',
        name = 'Velocity'
    )
)

traces.append(
    go.Scatter(
        x = out_data[:,0],
        y = out_data[:,3],
        mode = 'lines+markers',
        name = 'Acceleration'
    )
)

fig = go.Figure(data=traces)
fig.show()
