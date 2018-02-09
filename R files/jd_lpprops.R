source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_LP_Properties",
  contains = "JD3_ProcResults"
)



setMethod("show", "JD3_LP_Properties", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    show(proc_data(object@internal, "sweights"))
  }
})

jd3_lp_properties<-function(horizon, degree=3, kernel=c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"), endpoints=c("DAF", "CC", "LC", "QL", "CQ"), ic=4.5){
  d<-2/(sqrt(pi)*ic)
  kernel=match.arg(kernel)
  endpoints=match.arg(endpoints)
  jrslt<-.jcall("demetra/r/LocalPolynomialFilters", "Ldemetra/r/FiltersToolkit$FiniteFilters;", "filterProperties", as.integer(horizon), as.integer(degree), kernel, endpoints, d)
  new (Class = "JD3_LP_Properties", internal = jrslt)
}


