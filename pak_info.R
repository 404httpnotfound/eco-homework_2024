##################
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about
#-searching and installing packages
#-getting the name of all functions of a package
#-getting help of the package
#-in particular, doing the above mentioned to the packages that come with Tidyverse
##################

#run the first three lines before proceeding, to make sure that the package you wanna install has the full name you think
packsearchname<-"tidyverse"
library(packagefinder)
findPackage(packsearchname)
#check the packagefinder's output carefully, use the correct name for the following step if the name you used for search was incorrect
packname<-packsearchname
#installing and loading the package
install.packages(packname,dependencies=TRUE)
library(packname,character.only=TRUE)
#gets the list of functions of the loaded package
allfunctions<-ls(paste("package",packname,sep=":"))
print(allfunctions)
#for more information about the functions, seach in the help page
help(packname)
#the following part is supposed to only work with tidyverse, it allows you to explore the packages that came along with tidyverse
tidypacks<-tidyverse_packages()
print(tidypacks)
#choose a name from the output of the previous line, and explore
sub.packname<-"broom"
library(sub.packname,character.only=TRUE)
#gets the list of functions of the loaded package
sub.allfunctions<-ls(paste("package",sub.packname,sep=":"))
print(sub.allfunctions)
#for more information about the functions, seach in the help page
help(sub.packname)
