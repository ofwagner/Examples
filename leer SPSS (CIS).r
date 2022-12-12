library(foreign)

# load data base as list
sep2017.spss <- read.spss("sep2017.sav", to.data.frame=FALSE, use.value.labels=FALSE)
# convert list to data frame
sep2017 <- as.data.frame(sep2017.spss)
# copy all variable labels in separated list
sep2017_vars <- attr(sep2017.spss, "variable.labels")
# copy all value labels as separated list
sep2017_label <- attr(sep2017.spss, "label.table")

sep2017_names <- names(sep2017)


sep2017_2<-sep2017%>%as.data.frame%>%.[3:162]
names(sep2017_2) <- paste (sep2017_vars[3:162],sep2017_names[3:162], sep="-")
