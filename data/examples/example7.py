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

cfg = {
    "use_viz":False
}
print("Running...")
SimOut = M.examples.Run_Ex7(cfg)