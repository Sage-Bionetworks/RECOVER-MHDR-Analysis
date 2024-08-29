
# %%
import os
import glob
import synapseclient

from synapseclient import Project, Folder, File, Link
syn = synapseclient.Synapse()
syn.login()
# folder where the file will reside in Synapse
target_synapse_path = 'syn61834887'


# Define the directory containing the CSV files
directory = "/home/ec2-user/RECOVER-MHDR-Analysis/Duke_team/miss_day_time/"

# Create a pattern to match all CSV files
pattern = os.path.join(directory, "*.csv")

# Find all CSV files in the directory
csv_files = glob.glob(pattern)

# Print the list of CSV files
for file in csv_files:
    test_entity = synapseclient.File(file, description='first upload', parent=target_synapse_path)
    test_entity = syn.store(test_entity)

# %%
