if(!require(rJava)){
  install.packages("rJava")
}
library("rJava")
.jinit()
.jaddClassPath("./Java/demetra-design-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-datatypes-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-core-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-benchmarking-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-benchmarking-datatypes-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-modelling-datatypes-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-modelling-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-highfreq-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-sa-datatypes-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-sacore-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-r-1.0.0-SNAPSHOT.jar")

jd_clobj<-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
