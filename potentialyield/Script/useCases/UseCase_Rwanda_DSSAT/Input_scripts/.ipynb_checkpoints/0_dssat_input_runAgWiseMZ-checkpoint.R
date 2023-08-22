source('/home/jovyan/agwise/testingSpace/DSSATtest/Input_scripts/1_dssat_extdata_agwiseMZ.R')
dssat.extdata(jobs = 1, sdate = '1981-01-01', edate = '2020-12-31', ex.name = 'RW_DSSAT_Maize_Rawsoilweather',
              path.to.extdata = '/home/jovyan/agwise/testingSpace/DSSATtest/Input_data/extdata',
              path.weather.data = '/home/jovyan/agwise/testingSpace/DSSATtest/Input_data/Climate_data',
              Tmax = 'Tmax.data.coordinates_Rwanda.csv', Tmin = 'Tmin.data.coordinates_Rwanda.csv', 
              SRad = 'S.Rad.data.coordinates_Rwanda.csv', rain.data = 'Rainfall.data.coordinates_Rwanda.csv',
              lonlat.data = 'coordinates_Rwanda.csv')

source('/home/jovyan/agwise/testingSpace/DSSATtest/Input_scripts/2_dssat_expfileCGLabs_AgWiseMZ.R')
dssat.expfile(ex.name='RW_DSSAT_Maize_LONG_S2', jobs=30, path.to.extdata='/home/jovyan/agwise/testingSpace/DSSATtest/Input_data/extdata')

source('/home/jovyan/agwise/testingSpace/DSSATtest/Input_scripts/3_dssat_execCGLabs_AgWiseMZ.R')
dssat.exec(ex.name='RW_DSSAT_Maize_LONG_S2', jobs=30, path.to.extdata='/home/jovyan/agwise/testingSpace/DSSATtest/Input_data/extdata')

source('/home/jovyan/agwise/testingSpace/DSSATtest/Input_scripts/4_ProcessSpatialDSSATOutputData_AgWiseMZ.R')
merge_DSSAT_ouput(country='Rwanda', useCaseName='RAB',Crop='Potato', ex.name='RW_DSSAT_Potato_LONG_S2')






