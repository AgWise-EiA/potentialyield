source('/home/jovyan/agwise-potentialyield/dataops/potentialyield/Script/useCases/UseCase_Rwanda_DSSAT/Input_scripts/1_dssat_extdata_agwiseMZ.R')

dssat.extdata(jobs = 1, sdate = '1981-01-01', edate = '1982-12-31', ex.name = 'RW_DSSAT_Maize_Rawsoilweather',
              path.to.extdata = '/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_DSSAT/extdata',
              path.weather.data = '/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_DSSAT/Climate_data',
              Tmax = 'Tmax.data.coordinates_Rwanda.csv', Tmin = 'Tmin.data.coordinates_Rwanda.csv', 
              SRad = 'S.Rad.data.coordinates_Rwanda.csv', rain.data = 'Rainfall.data.coordinates_Rwanda.csv',
              lonlat.data = 'coordinates_Rwanda.csv')

source('/home/jovyan/agwise-potentialyield/dataops/potentialyield/Script/useCases/UseCase_Rwanda_DSSAT/Input_scripts/2_dssat_expfileCGLabs_AgWiseMZ.R')
dssat.expfile(ex.name='RW_DSSAT_Maize_Rawsoilweather', jobs=30, path.to.extdata='/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_DSSAT/extdata')

source('/home/jovyan/agwise-potentialyield/dataops/potentialyield/Script/useCases/UseCase_Rwanda_DSSAT/Input_scripts/3_dssat_execCGLabs_AgWiseMZ.R')
dssat.exec(ex.name='RW_DSSAT_Maize_Rawsoilweather', jobs=10, path.to.extdata='/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_DSSAT/extdata')

source('/home/jovyan/agwise-potentialyield/dataops/potentialyield/Script/useCases/UseCase_Rwanda_DSSAT/Input_scripts/4_ProcessSpatialDSSATOutputData_AgWiseMZ.R')
merge_DSSAT_ouput(ex.name='RW_DSSAT_Maize_Rawsoilweather')


data <- readRDS("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_DSSAT/Output_data/RW_DSSAT_test_maize.rds")

rainfall <- readRDS('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/result/geo_4cropModel/Rainfall/Rainfall_4CM_trial_CHIRPS.RDS')


