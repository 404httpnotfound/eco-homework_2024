##################
#Author: Xin Ming
#created on:
#xm123456@mail.ustc.edu.cn
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about
#creating empty database
#writing data into database
#querying data from database
##################
##loading required packages
library(RSQLite)
library(ade4)
library(shinyNotes)
##creating empty .sqlite database 
doubs_db<-dbConnect(RSQLite::SQLite(), "Doubs.sqlite")
##loading data and extracting the element dfs from the doubs dataset
data(doubs)
doubs
env<-doubs$env
fishes<-doubs$fish
site_coordinates<-doubs$xy
fish_names<-doubs$species
#storing data into the database
db.write_table(doubs_db,env,table="env")
db.write_table(doubs_db,fishes,table="fishes")
db.write_table(doubs_db,site_coordinates,table="site_coordinates")
db.write_table(doubs_db,fish_names,table="fish_names")
#making sure that the data are indeed stored.
dbDisconnect(doubs_db)
doubs_db
doubs_db<-dbConnect(RSQLite::SQLite(), "Doubs.sqlite")
dbGetQuery(doubs_db,"SELECT * FROM env")
dbGetQuery(doubs_db,"SELECT * FROM fishes")
dbGetQuery(doubs_db,"SELECT * FROM site_coordinates")
dbGetQuery(doubs_db,"SELECT * FROM fish_names")
dbDisconnect(doubs_db)
