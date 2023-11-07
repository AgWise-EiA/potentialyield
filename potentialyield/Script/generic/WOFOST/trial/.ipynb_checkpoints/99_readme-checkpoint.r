## wofost modelling for trials
# Do this: 
# 1. Open script 2_make_settings.r and make all settings (paths etc)
# 2. Open script 1_run_all and run it. It will source the other scripts
# 3. Check results in the specified output directory


# A short description of the procedure: 
# 1. After initial preparations, input data for all trials are read and compiled. 
# Also a data.frame (called xy.df) with metadata for all trials is compiled 
# (one row per trial)

# 2. Wofost requires lists of data/parameters on crop, soil, weather and model control. 
# A function is defined; It does the following for one trial: i) for crop, soil and control,
# lists with default parameters are generated, and then modified by  data and 
# settings availableforthe trial in question, and ii) a weather data list is prepared 
# by picking weather data from the data.frame containing weather data from all trials).

# 3. The function is run for all trials by parallel processing using dapply 
# (instead of a foreach loop, because dapply was considerably faster). The 
# predicted yield (wso) is added to xy.df

# 4. results are mapped plotted and exported.


## Advanced:
# The file 00_metarun.r can be modified and used, for example to run Wosost with 
# a range different parameters (e.g. all combinations of selected tsum1 and tsum2 values)
# for all trials and then compile results
