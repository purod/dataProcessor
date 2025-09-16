# %%
# Auto-reload modules when they change
%load_ext autoreload
%autoreload 2
# Enable inline plotting for Jupyter notebooks
%matplotlib inline

# show all rows or the first 10 rows, for all, you use None
pd.set_option("display.max_rows",10)   

# Set default figure size and DPI
mpl.rcParams['figure.dpi'] = 200         # Higher DPI = better quality
mpl.rcParams['savefig.dpi'] = 300        # When saving figures
mpl.rcParams['figure.figsize'] = (4, 2)  # Default size for all figures
mpl.rcParams['font.size'] = 12           # Default font size

# modules
import pyreadr
import pandas as pd
import seaborn as sns

import matplotlib as mpl
import matplotlib.pyplot as plt

from src.profilereport_config import custom_config

profile = ProfileReport(
    airport, config=custom_config,
    title="Airport Data Profiling Report"      
)
profile.to_file("../outputs/plots/airport_report.html")
