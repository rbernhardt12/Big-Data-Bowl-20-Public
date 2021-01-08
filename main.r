### NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# October 2020 

## Main R-Runner File 

MainDir = getwd()  

## Production Options (ProdOpts)
ScriptName = paste(MainDir , "/prod_opts.r" , sep = "")
source(ScriptName)

## Import Data 
ScriptName = paste(MainDir , "/Cleaning/import_data.r" , sep = "")
source(ScriptName)

## Clean Data 
ScriptName = paste(MainDir , "/Cleaning/clean_data.r" , sep = "")
source(ScriptName)

## Visualize Data 

# Create Visualizations 
ScriptName = paste(MainDir , "/Analysis/visualize.r" , sep = "")
source(ScriptName)

## Analyze Data 
ScriptName = paste(MainDir , "/Analysis/feature_analysis1.r" , sep = "")
source(ScriptName)

## End 
clear() 
print("Main File Successfully Completed") 
