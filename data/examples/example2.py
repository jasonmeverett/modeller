'''

Simple force oscillator

'''



import sys
import numpy as np
import plotly.io as pio
import plotly.graph_objects as go

# Locate Modeller and import.
sys.path.append('/workspaces/modeller/build')
import Modeller as M

# Set up a configuration dictionary to pass to the example.
cfg_dict = { }

# Run the example.
out_data = np.array(M.examples.Run_Ex2(cfg_dict))


# # Plot the output
# traces = []
# traces.append(
#     go.Scatter(
#         x = out_data[:,0],
#         y = out_data[:,1],
#         mode = 'lines+markers',
#         name = 'Position'
#     )
# )

# traces.append(
#     go.Scatter(
#         x = out_data[:,0],
#         y = out_data[:,2],
#         mode = 'lines+markers',
#         name = 'Velocity'
#     )
# )

# traces.append(
#     go.Scatter(
#         x = out_data[:,0],
#         y = out_data[:,3],
#         mode = 'lines+markers',
#         name = 'Acceleration'
#     )
# )

# fig = go.Figure(data=traces)
# fig.write_html('first_figure.html')
