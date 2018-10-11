source("./R files/jd3_init.R")
source("./R files/jd3_holidays.R")

westernHolidays<-function(){
  jhol<-jd3_holidays()
  add(jhol, "NewYear")
  add(jhol, "MayDay")
  add(jhol, "AllSaintsDay")
  add(jhol, "Christmas")
  add(jhol, "EasterMonday")
  add(jhol, "Ascension")
  add(jhol, "WhitMonday")
  return (jhol)
}
