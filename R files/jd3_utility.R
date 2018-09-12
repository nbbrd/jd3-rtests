source("./R files/jd3_rslts.R")

jd3_calendar<-function(group, contrasts=TRUE, period, startYear, startPeriod=1, length=0){
 if (length == 0)
   length = period*20
 jdom<-tsdomain_r2jd(period, startYear, startPeriod, length)
 jm<-.jcall("demetra/r/GenericCalendars", "Ldemetra/maths/MatrixType;", "td", jdom, as.integer(group), contrasts)
 return(matrix_jd2r(jm))
}