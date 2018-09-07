source("./R files/jd3_rslts.R")

if (! isGeneric("saDecomposition")){
  setGeneric(name="saDecomposition", def = function( object, id, ... ){standardGeneric("saDecomposition")})
  lockBinding("saDecomposition", .GlobalEnv)
}

if (! isGeneric("result" )){
  setGeneric(name="result", def = function( object, id, ... ){standardGeneric("result")})
  lockBinding("result", .GlobalEnv)
}

if (!isGeneric("dictionary")){
  setGeneric(name="dictionary", def = function( object, ... ){standardGeneric("dictionary")})
  lockBinding("dictionary", .GlobalEnv)
}

setClass(
  Class="JD3_Object",
  representation = representation(internal = "jobjRef" )
)

setClass(
  Class="JD3_ProcResults",
  contains = "JD3_Object"
)

setMethod("dictionary", "JD3_ProcResults", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_dictionary(.jclass(object@internal))
  }
  
})

setMethod("result", signature = c(object="JD3_ProcResults", id="character"), function(object, id){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_data(object@internal, id)}
})

lockBinding("result", .GlobalEnv)
