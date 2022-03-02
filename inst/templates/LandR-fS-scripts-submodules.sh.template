#!/bin/bash

## TODO: add your study area submodule
# git submodule add https://github.com/<USERNAME>/<REPO> modules/<REPO>

## climate data
git submodule add https://github.com/PredictiveEcology/canClimateData modules/canClimateData

## LandR
git submodule add https://github.com/PredictiveEcology/Biomass_borealDataPrep modules/Biomass_borealDataPrep
git submodule add https://github.com/PredictiveEcology/Biomass_core modules/Biomass_core
git submodule add https://github.com/PredictiveEcology/Biomass_regeneration modules/Biomass_regeneration
git submodule add https://github.com/PredictiveEcology/Biomass_speciesData modules/Biomass_speciesData
git submodule add https://github.com/PredictiveEcology/Biomass_speciesFactorial modules/Biomass_speciesFactorial
git submodule add https://github.com/PredictiveEcology/Biomass_speciesParameters modules/Biomass_speciesParameters
git submodule add https://github.com/ianmseddy/gmcsDataPrep modules/gmcsDataPrep

## fireSense
git submodule add https://github.com/PredictiveEcology/fireSense modules/fireSense
git submodule add https://github.com/PredictiveEcology/fireSense_EscapeFit modules/fireSense_EscapeFit
git submodule add https://github.com/PredictiveEcology/fireSense_EscapePredict modules/fireSense_EscapePredict
git submodule add https://github.com/PredictiveEcology/fireSense_IgnitionFit modules/fireSense_IgnitionFit
git submodule add https://github.com/PredictiveEcology/fireSense_IgnitionPredict modules/fireSense_IgnitionPredict
git submodule add https://github.com/PredictiveEcology/fireSense_SpreadFit modules/fireSense_SpreadFit
git submodule add https://github.com/PredictiveEcology/fireSense_SpreadPredict modules/fireSense_SpreadPredict
git submodule add https://github.com/PredictiveEcology/fireSense_dataPrepFit modules/fireSense_dataPrepFit
git submodule add https://github.com/PredictiveEcology/fireSense_dataPrepPredict modules/fireSense_dataPrepPredict

## yield curves
git submodule add https://github.com/PredictiveEcology/Biomass_speciesFactorial modules/Biomass_speciesFactorial
#git submodule add https://github.com/PredictiveEcology/Biomass_yieldTables modules/Biomass_yieldTables

####################################################################################################

cd modules

cd canClimateData && git checkout development
#cd ../<REPO> && git checkout development ## TODO: change branches for your study area module if needed

cd ../Biomass_borealDataPrep && git checkout development
cd ../Biomass_core && git checkout development
cd ../Biomass_regeneration && git checkout development
cd ../Biomass_speciesData && git checkout development
cd ../Biomass_speciesParameters && git checkout development
cd ../gmcsDataPrep && git checkout development

cd ../Biomass_speciesFactorial && git checkout development
#cd ../Biomass_yieldTables && git checkout development

cd ../fireSense && git checkout development
#cd ../fireSense_EscapeFit && git checkout development
#cd ../fireSense_EscapePredict && git checkout development
cd ../fireSense_IgnitionFit && git checkout development
cd ../fireSense_IgnitionPredict && git checkout development
cd ../fireSense_SpreadFit && git checkout development
cd ../fireSense_SpreadPredict && git checkout development
#cd ../fireSense_dataPrepFit && git checkout development
cd ../fireSense_dataPrepPredict && git checkout development

cd ../..

