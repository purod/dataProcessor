# %%
# Auto-reload modules when they change
%load_ext autoreload
%autoreload 2
# Enable inline plotting for Jupyter notebooks
%matplotlib inline

# show all rows or the first 10 rows, for all, you use None
pd.set_option("display.max_rows",10)   


from src.profilereport_config import custom_config


profile = ProfileReport(
    airport, config=custom_config,
    title="Airport Data Profiling Report"      
)
profile.to_file("../outputs/plots/airport_report.html")
