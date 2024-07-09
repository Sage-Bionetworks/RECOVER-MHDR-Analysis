import pandas as pd

START_TIME = pd.Timestamp('2023-09-01')
END_TIME = pd.Timestamp('2024-02-25')

time_chunk_15min = pd.date_range(start="00:00", end="23:49", freq='15min').time