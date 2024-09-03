# %%
import synapseclient
import pandas as pd

syn = synapseclient.Synapse()
syn.login()

sage_data = syn.get(entity='syn53830116', version=8)
sage_data = pd.read_csv(sage_data.path)
# %%
sage_data['concept'] = sage_data['concept'].astype(str)
var_names = []
for i in range(len(sage_data['concept'].unique())):
    if 'remfragmentationindex' in sage_data['concept'].unique()[i]:
        var_names.append(sage_data['concept'].unique()[i])

lat_var_names = []
for i in range(len(sage_data['concept'].unique())):
    if 'remonsetlatency' in sage_data['concept'].unique()[i]:
        lat_var_names.append(sage_data['concept'].unique()[i])
# %%
rfi_name = var_names[2]
lat_name = lat_var_names[2]

sage_data_rfi = sage_data[sage_data['concept'] == rfi_name]
sage_data_lat = sage_data[sage_data['concept'] == lat_name]
# %%
sage_data_rfi.to_csv('sage_data_rfi.csv', index=False)
sage_data_lat.to_csv('sage_data_lat.csv', index=False)

# %%
