# Defining the reference class "model"
# This reference class is a wrapper to save models created through instat or climatic object methods.
# The fields are the properties every climate_data object will have.

# method_name       : The name of the instat or climate object method this model came from 
# method_parameters : The list parameters used in the method call
# function_name     : The name of the R function used, i.e. the type of model outputs 
# output_list       : A list containing the outputs to all the models created by the method


intstat_model <- setRefClass("intstat_model", 
                       fields = list(method_name = "character", method_parameters = "list", 
                                     function_name = "character", output_list = "list")
)

intstat_model$methods(initialize = function(call_name, call_parameters, R_model_type, models) {
  
  method_name <<- call_name
  method_parameters <<- call_parameters
  function_name <<- R_model_type 
  output_list <<- models
  
}
)