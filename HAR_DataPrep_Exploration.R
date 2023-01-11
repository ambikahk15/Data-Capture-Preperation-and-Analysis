#****************************************************************************
# Data Capture and Preparations-Assignment1
# Team: The Data Cranes   
# Members: Ambika Kapanaiah
#          Priyank Sharma
#          Anu Maria George
#          Yingqi Chen
#****************************************************************************

##############################Start of the code#################################
#Setting working directory
rm(list = ls())

#setting the working directory for the current script to execute
#getting the directory of the current script
#this following has to be executed twice if the script has not invoked R studio.
#hence executing twice to set the working directory to the script path
for(i in 1:2){
  Wrkng_Dir <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Wrkng_Dir}
#Appending data files folder to the path
dataFile_Path <- file.path(Wrkng_Dir, "InputData")
dataFile_Path
#getting the file names under InputData folder of captured data
filenames <- list.files(file.path(Wrkng_Dir, "InputData"))
filenames
#calculating the number of files in InputData folder
numfiles <- length(filenames) 
numfiles


##########################1. import data from CSV----##########################

#Importing files to individual data frames
df_in_list <- list()
for(file_index in 1:length(filenames)){
  assign(paste("df_",sub(".txt","",filenames[file_index]),sep=""),
         as.data.frame(read.csv(paste(dataFile_Path,"/",filenames[file_index],sep=""),
                                header=F,stringsAsFactors=FALSE,
                                na.strings=c(""," ","NA"))))}

#creating list of data fraes for easy access in further steps
df_in_list <- list(df_subj1_act1,df_subj1_act2,df_subj1_act3,df_subj1_act4,
                   df_subj2_act1,df_subj2_act2,df_subj2_act3,df_subj2_act4,
                   df_subj3_act1,df_subj3_act2,df_subj3_act3,df_subj3_act4)


#reomve dfs from GlobalEnv as we now have stored them in list to free up memory
rm(df_subj1_act1,df_subj1_act2,df_subj1_act3,df_subj1_act4,
   df_subj2_act1,df_subj2_act2,df_subj2_act3,df_subj2_act4,
   df_subj3_act1,df_subj3_act2,df_subj3_act3,df_subj3_act4)

##########################2. Find the Data markers----##########################
#data marakers stored in Finlmrkr_mtrx for 12 data frames
i <- 0L
all_markers <-  c()
maxrow_nums <-  0L
Finlmrkr_mtrx <- matrix(,12,2)
for(i in 1:length(df_in_list)){
  all_markers[[i]] <- which(df_in_list[[i]]$V1 == "Sample")
  maxrow_nums <- max(diff(all_markers[[i]]))
  mrkr_index <- which(diff(all_markers[[i]])==maxrow_nums)
  Finlmrkr_mtrx[i,1] <- all_markers[[i]][mrkr_index]
  Finlmrkr_mtrx[i,1] <- Finlmrkr_mtrx[i,1]+1
  Finlmrkr_mtrx[i,2] <- all_markers[[i]][mrkr_index+1]}

##########################3. Rename variables----###############################
#each data frame columns renamed. 
column_names <- c("Sample_ID","Accmtr_X","Accmtr_Y","Accmtr_Z","Temprture",
                  "Gyromtr_X","Gyromtr_Y","Gyromtr_Z")
for(i in 1:length(df_in_list)){
  colnames(df_in_list[[i]]) <- column_names  }

##########################4. Remove specific data----###########################
#Discarding accmeter X,Y,Z and Temp  columns
for(i in 1:length(df_in_list)){
  df_in_list[[i]]=df_in_list[[i]][c("Sample_ID", "Gyromtr_X","Gyromtr_Y","Gyromtr_Z")]}

#############5. Obtain desired data and store the desired data into data frames----
#using data markers from Finlmrkr_mtrx[] 
#re-writing the data back to the list of data frames.
i <- 1
row_n1 <- 0L
row_n2 <- 0L
for(i in 1:length(df_in_list)){
  row_n1 <- Finlmrkr_mtrx[i,1]
  row_n2 <- Finlmrkr_mtrx[i,2]-1
  df_in_list[[i]]=df_in_list[[i]][row_n1:row_n2,]}

#################6. Remove variables from the environment variables----#########
#reoving all data except list of data frame and data markers.
rm(dataFile_Path,column_names,file_index,filenames,i,mrkr_index,numfiles,
   row_n1,row_n2,Wrkng_Dir,all_markers)

##################7. Remove empty cells or NAs----##############################
#As empty cells were already replaced with NA while reading csv file
#removing rows with NA
for(i in 1:length(df_in_list)){
  df_in_list[[i]] <- df_in_list[[i]][rowSums(is.na(df_in_list[[i]])) == 0, ]}

############################8. Check data Structure----#########################
#checking for special characters or strings and converting to numeric values
for(i in 1:length(df_in_list)){
  for(r in nrow(df_in_list[[i]])){
    for(c in 1:ncol(df_in_list[[i]])){
      df_in_list[[i]][r,c] <- gsub("[^[:alnum:]///' ]", "", df_in_list[[i]][r,c])
      df_in_list[[i]][r,c] <- gsub("*ÿ", "", df_in_list[[i]][r,c])
      df_in_list[[i]][r,c] <- gsub("*ü", "", df_in_list[[i]][r,c])}}}

##################9. Signal Calibration#########################################
#calculating mean of all columns for the resting period,
#storing means based on axis(gx,gy,gz) as mean_restx, mean_resty, mean_restz
rows <- 0L
mean_restx <- c()
mean_resty <- c()
mean_restz <- c()
for(i in 1:length(df_in_list)){
  rows=nrow(df_in_list[[i]])%/%5
  rowstart <- Finlmrkr_mtrx[i,1]
  rowend <- rows+rowstart
  mean_restx[i]=mean(as.numeric(df_in_list[[i]][rowstart:rowend,]$Gyromtr_X))
  mean_resty[i]=mean(as.numeric(df_in_list[[i]][rowstart:rowend,]$Gyromtr_Y))
  mean_restz[i]=mean(as.numeric(df_in_list[[i]][rowstart:rowend,]$Gyromtr_Z))}

#Remove the rest period data from the entire data
#for subj1 act1
subj1_act1_calibx=as.numeric(df_in_list[[1]]$Gyromtr_X)-mean_restx[1]
subj1_act1_caliby=as.numeric(df_in_list[[1]]$Gyromtr_Y)-mean_resty[1]
subj1_act1_calibz=as.numeric(df_in_list[[1]]$Gyromtr_Z)-mean_restz[1]
#for subj1 act2
subj1_act2_calibx=as.numeric(df_in_list[[2]]$Gyromtr_X)-mean_restx[2]
subj1_act2_caliby=as.numeric(df_in_list[[2]]$Gyromtr_Y)-mean_resty[2]
subj1_act2_calibz=as.numeric(df_in_list[[2]]$Gyromtr_Z)-mean_restz[2]
#for subj1 act3
subj1_act3_calibx=as.numeric(df_in_list[[3]]$Gyromtr_X)-mean_restx[3]
subj1_act3_caliby=as.numeric(df_in_list[[3]]$Gyromtr_Y)-mean_resty[3]
subj1_act3_calibz=as.numeric(df_in_list[[3]]$Gyromtr_Z)-mean_restz[3]
#for subj1 act4
subj1_act4_calibx=as.numeric(df_in_list[[4]]$Gyromtr_X)-mean_restx[4]
subj1_act4_caliby=as.numeric(df_in_list[[4]]$Gyromtr_Y)-mean_resty[4]
subj1_act4_calibz=as.numeric(df_in_list[[4]]$Gyromtr_Z)-mean_restz[4]

#for subj2 act1
subj2_act1_calibx=as.numeric(df_in_list[[5]]$Gyromtr_X)-mean_restx[5]
subj2_act1_caliby=as.numeric(df_in_list[[5]]$Gyromtr_Y)-mean_resty[5]
subj2_act1_calibz=as.numeric(df_in_list[[5]]$Gyromtr_Z)-mean_restz[5]
#for subj2 act2
subj2_act2_calibx=as.numeric(df_in_list[[6]]$Gyromtr_X)-mean_restx[6]
subj2_act2_caliby=as.numeric(df_in_list[[6]]$Gyromtr_Y)-mean_resty[6]
subj2_act2_calibz=as.numeric(df_in_list[[6]]$Gyromtr_Z)-mean_restz[6]
#for subj2 act3
subj2_act3_calibx=as.numeric(df_in_list[[7]]$Gyromtr_X)-mean_restx[7]
subj2_act3_caliby=as.numeric(df_in_list[[7]]$Gyromtr_Y)-mean_resty[7]
subj2_act3_calibz=as.numeric(df_in_list[[7]]$Gyromtr_Z)-mean_restz[7]
#for subj2 act4
subj2_act4_calibx=as.numeric(df_in_list[[8]]$Gyromtr_X)-mean_restx[8]
subj2_act4_caliby=as.numeric(df_in_list[[8]]$Gyromtr_Y)-mean_resty[8]
subj2_act4_calibz=as.numeric(df_in_list[[8]]$Gyromtr_Z)-mean_restz[8]

#for subj3 act1
subj3_act1_calibx=as.numeric(df_in_list[[9]]$Gyromtr_X)-mean_restx[9]
subj3_act1_caliby=as.numeric(df_in_list[[9]]$Gyromtr_Y)-mean_resty[9]
subj3_act1_calibz=as.numeric(df_in_list[[9]]$Gyromtr_Z)-mean_restz[9]
#for subj3 act2
subj3_act2_calibx=as.numeric(df_in_list[[10]]$Gyromtr_X)-mean_restx[10]
subj3_act2_caliby=as.numeric(df_in_list[[10]]$Gyromtr_Y)-mean_resty[10]
subj3_act2_calibz=as.numeric(df_in_list[[10]]$Gyromtr_Z)-mean_restz[10]
#for subj3 act3
subj3_act3_calibx=as.numeric(df_in_list[[11]]$Gyromtr_X)-mean_restx[11]
subj3_act3_caliby=as.numeric(df_in_list[[11]]$Gyromtr_Y)-mean_resty[11]
subj3_act3_calibz=as.numeric(df_in_list[[11]]$Gyromtr_Z)-mean_restz[11]
#for subj3 act4
subj3_act4_calibx=as.numeric(df_in_list[[12]]$Gyromtr_X)-mean_restx[12]
subj3_act4_caliby=as.numeric(df_in_list[[12]]$Gyromtr_Y)-mean_resty[12]
subj3_act4_calibz=as.numeric(df_in_list[[12]]$Gyromtr_Z)-mean_restz[12]

###################10.....Plot the data----#####################################
par(mfrow = c(1,4), oma = c(1, 1, 1, 1))
#Subj1 Activity1 plotting
plot(unlist(subj1_act1_calibx),type="b",lwd=2,
     ylim=range(subj1_act1_calibx),col="red",
     xlab="Activity1",ylab="Gyroscope data")
mtext(side=3,"Subject1 with all Activities",cex=1,font=1)
lines(unlist(subj1_act1_caliby),col="green")
lines(unlist(subj1_act1_calibz),col="blue")
legend("topleft",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Subj1 Activity2 plotting
plot(unlist(subj1_act2_calibx),type="b",lwd=2,
     ylim=range(subj1_act2_calibx),col="red",
     xlab="Activity2",
     ylab="Gyroscope data")
lines(unlist(subj1_act2_caliby),col="green")
lines(unlist(subj1_act2_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
lty=1,lwd=2,pch=21,col=c("red","green","blue"),
text.col=c("red","green","blue"))

#Subj1 Activity3 plotting
plot(unlist(subj1_act3_calibx),type="b",lwd=2,
     ylim=range(subj1_act3_calibx),col="red",
     xlab="Activity3",ylab="Gyroscope data")
lines(unlist(subj1_act3_caliby),col="green")
lines(unlist(subj1_act3_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
lty=1,lwd=2,pch=21,col=c("red","green","blue"),
text.col=c("red","green","blue"))

#Subj1 Activity4 plotting
plot(unlist(subj1_act4_calibx),type="b",lwd=2,
     ylim=range(subj1_act4_calibx),col="red",
     xlab="Activity4",ylab="Gyroscope data")
lines(unlist(subj1_act4_caliby),col="green")
lines(unlist(subj1_act4_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
lty=1,lwd=2,pch=21,col=c("red","green","blue"),
text.col=c("red","green","blue"))

#Begin Subj2 plotting
par(mfrow = c(1,4), oma = c(1, 1, 1, 1))

#Subj2 Activity1 plotting
plot(unlist(subj2_act1_calibx),type="b",lwd=2,
     ylim=range(subj2_act1_calibx),col="red",
     xlab="Activity1",ylab="Gyroscope data")
mtext(side=3,"Subject2 with all Activities",cex=1,font=1)
lines(unlist(subj2_act1_caliby),col="green")
lines(unlist(subj2_act1_calibz),col="blue")
legend("topleft",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Subj2 Activity2 plotting
plot(unlist(subj2_act2_calibx),type="b",lwd=2,
     ylim=range(subj2_act2_calibx),col="red",
     xlab="Activity2",
     ylab="Gyroscope data")
lines(unlist(subj2_act2_caliby),col="green")
lines(unlist(subj2_act2_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Subj2 Activity3 plotting
plot(unlist(subj2_act3_calibx),type="b",lwd=2,
     ylim=range(subj2_act3_calibx),col="red",
     xlab="Activity3",ylab="Gyroscope data")
lines(unlist(subj2_act3_caliby),col="green")
lines(unlist(subj2_act3_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Subj2 Activity4 plotting
plot(unlist(subj2_act4_calibx),type="b",lwd=2,
     ylim=range(subj2_act4_calibx),col="red",
     xlab="Activity4",ylab="Gyroscope data")
lines(unlist(subj2_act4_caliby),col="green")
lines(unlist(subj2_act4_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Begin Subj3 plotting.
par(mfrow = c(1,4), oma = c(1, 1, 1, 1))

#Subj3 Activity1 plotting
plot(unlist(subj3_act1_calibx),type="b",lwd=2,
     ylim=range(subj3_act1_calibx),col="red",
     xlab="Activity1",ylab="Gyroscope data")
mtext(side=3,"Subject3 with all Activities",cex=1,font=1)
lines(unlist(subj3_act1_caliby),col="green")
lines(unlist(subj3_act1_calibz),col="blue")
legend("topleft",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Subj3 Activity2 plotting
plot(unlist(subj3_act2_calibx),type="b",lwd=2,
     ylim=range(subj3_act2_calibx),col="red",
     xlab="Activity2",
     ylab="Gyroscope data")
lines(unlist(subj3_act2_caliby),col="green")
lines(unlist(subj3_act2_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Subj3 Activity3 plotting
plot(unlist(subj3_act3_calibx),type="b",lwd=2,
     ylim=range(subj3_act3_calibx),col="red",
     xlab="Activity3",ylab="Gyroscope data")
lines(unlist(subj3_act3_caliby),col="green")
lines(unlist(subj3_act3_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))

#Subj3 Activity4 plotting
plot(unlist(subj3_act4_calibx),type="b",lwd=2,
     ylim=range(subj3_act4_calibx),col="red",
     xlab="Activity4",ylab="Gyroscope data")
lines(unlist(subj3_act4_caliby),col="green")
lines(unlist(subj3_act4_calibz),col="blue")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("red","green","blue"),
       text.col=c("red","green","blue"))
   
###################11. Get only the ~30 seconds of each activity----############
#Dividing the data amount  by 5 to get each 10 secs data , removing the first and last 10 secs data
rows <- 0L
actstrt_endMrtrx = matrix(,12,2)

for(i in 1:length(df_in_list)){
  rows=nrow(df_in_list[[i]])%/%5
  actstart <- rows+1
  actend <- actstart+(rows*3)  
  actstrt_endMrtrx[i,1] <- actstart
  actstrt_endMrtrx[i,2] <- actend
  df_in_list[[i]] <- df_in_list[[i]][actstart:actend,]  }

#12.Window data.------##########################################################

#matrix for 6 slices of data each of 5 secs for acts 1 to 4 in subj1
for(act in 1:4){
  totrows <- nrow(df_in_list[[act]])
  slicerows <- totrows%/%6
  rw1 <- 1
  rw2 <- slicerows
  for(i in 1:6){
    assign(paste("matx",i,"_subj1","_act",act,sep=""),
           cbind(df_in_list[[act]][rw1:rw2,2],df_in_list[[act]][rw1:rw2,3],
                 df_in_list[[act]][rw1:rw2,4]))
    rw1 <- rw2+1
    rw2 <- rw1+slicerows}}

List_subj1_Act1 <- list(matx1_subj1_act1,matx2_subj1_act1,matx3_subj1_act1,
                        matx4_subj1_act1,matx5_subj1_act1,matx6_subj1_act1)
List_subj1_Act2 <- list(matx1_subj1_act2,matx2_subj1_act2,matx3_subj1_act2,
                        matx4_subj1_act2,matx5_subj1_act2,matx6_subj1_act2)
List_subj1_Act3 <- list(matx1_subj1_act3,matx2_subj1_act3,matx3_subj1_act3,
                        matx4_subj1_act3,matx5_subj1_act3,matx6_subj1_act3)
List_subj1_Act4 <- list(matx1_subj1_act4,matx2_subj1_act4,matx3_subj1_act4,
                        matx4_subj1_act4,matx5_subj1_act4,matx6_subj1_act4)

List_subj1 <- list(List_subj1_Act1,List_subj1_Act2,
                   List_subj1_Act3,List_subj1_Act4)

#matrix for 6 slices of data each of 5 secs for acts 5 to 8(1 to 4) in subj2 and storing in list
for(act in 5:8){
  totrows <- nrow(df_in_list[[act]])
  slicerows <- totrows%/%6
  rw1 <- 1
  rw2 <- slicerows
  for(i in 1:6){
    assign(paste("matx",i,"_subj2","_act",act,sep=""),
           cbind(df_in_list[[act]][rw1:rw2,2],df_in_list[[act]][rw1:rw2,3],
                 df_in_list[[act]][rw1:rw2,4]))
    rw1 <- rw2+1
    rw2 <- rw1+slicerows}}


List_subj2_act5 <- list(matx1_subj2_act5,matx2_subj2_act5,matx3_subj2_act5,
                        matx4_subj2_act5,matx5_subj2_act5,matx6_subj2_act5)
List_subj2_act6 <- list(matx1_subj2_act6,matx2_subj2_act6,matx3_subj2_act6,
                        matx4_subj2_act6,matx5_subj2_act6,matx6_subj2_act6)
List_subj2_act7 <- list(matx1_subj2_act7,matx2_subj2_act7,matx3_subj2_act7,
                        matx4_subj2_act7,matx5_subj2_act7,matx6_subj2_act7)
List_subj2_act8 <- list(matx1_subj2_act8,matx2_subj2_act8,matx3_subj2_act8,
                        matx4_subj2_act8,matx5_subj2_act8,matx6_subj2_act8)

List_subj2 <- list(List_subj2_act5,List_subj2_act6,
                   List_subj2_act7,List_subj2_act8)


#matrix for 6 slices of data each of 5 secs for acts 9 to 12(1 to 4) in subj3

for(act in 9:12){
  totrows <- nrow(df_in_list[[act]])
  slicerows <- totrows%/%6
  rw1 <- 1
  rw2 <- slicerows
  for(i in 1:6){
    assign(paste("matx",i,"_subj3","_act",act,sep=""),
           cbind(df_in_list[[act]][rw1:rw2,2],df_in_list[[act]][rw1:rw2,3],
                 df_in_list[[act]][rw1:rw2,4]))
    rw1 <- rw2+1
    rw2 <- rw1+slicerows}}

List_subj3_act9 <- list(matx1_subj3_act9,matx2_subj3_act9,matx3_subj3_act9,
                        matx4_subj3_act9,matx5_subj3_act9,matx6_subj3_act9)
List_subj3_act10 <- list(matx1_subj3_act10,matx2_subj3_act10,matx3_subj3_act10,
                         matx4_subj3_act10,matx5_subj3_act10,matx6_subj3_act10)
List_subj3_act11 <- list(matx1_subj3_act11,matx2_subj3_act11,matx3_subj3_act11,
                         matx4_subj3_act11,matx5_subj3_act11,matx6_subj3_act11)
List_subj3_act12 <- list(matx1_subj3_act12,matx2_subj3_act12,matx3_subj3_act12,
                         matx4_subj3_act12,matx5_subj3_act12,matx6_subj3_act12)

List_subj3 <- list(List_subj3_act9,List_subj3_act10,
                   List_subj3_act11,List_subj3_act12)

#Final list having all the subject list
Finlst_AllSubj <- list(List_subj1,List_subj2,List_subj3)

                   
############13.mean and SD for each participant(all acts) within each window####

# mean and SD for subj1--actv 1 to 4 and slice 1 to 6 each:
gx_mean_Subj1 <-  list()
gx_stdev_Subj1 <- list()
gy_mean_Subj1 <- list()
gy_stdev_Subj1 <- list()
gz_mean_Subj1 <- list()
gz_stdev_Subj1 <- list()
gxmeanSubj1_act1 <- list()
gxmeanSubj1_act2 <- list()
gxmeanSubj1_act3 <- list()
gxmeanSubj1_act4 <- list()
gymeanSubj1_act1 <- list()
gymeanSubj1_act2 <- list()
gymeanSubj1_act3 <- list()
gymeanSubj1_act4 <- list()
gzmeanSubj1_act1 <- list()
gzmeanSubj1_act2 <- list()
gzmeanSubj1_act3 <- list()
gzmeanSubj1_act4 <- list()
gxSDSubj1_act1 <- list()
gxSDSubj1_act2 <- list()
gxSDSubj1_act3 <- list()
gxSDSubj1_act4 <- list()
gySDSubj1_act1 <- list()
gySDSubj1_act2 <- list()
gySDSubj1_act3 <- list()
gySDSubj1_act4 <- list()
gzSDSubj1_act1 <- list()
gzSDSubj1_act2 <- list()
gzSDSubj1_act3 <- list()
gzSDSubj1_act4 <- list()

for(act in 1:4){
  for(slice in 1:6){
    gx_mean_Subj1[[slice]] <- mean(na.omit(as.numeric(List_subj1[[act]][[slice]][,1])))
    gx_stdev_Subj1[[slice]] <- sd(na.omit(as.numeric(List_subj1[[act]][[slice]][,1])))
    gy_mean_Subj1[[slice]]  <- mean(na.omit(as.numeric(List_subj1[[act]][[slice]][,2])))
    gy_stdev_Subj1[[slice]]  <- sd(na.omit(as.numeric(List_subj1[[act]][[slice]][,2])))
    gz_mean_Subj1[[slice]]  <- mean(na.omit(as.numeric(List_subj1[[act]][[slice]][,3])))
    gz_stdev_Subj1[[slice]]  <- sd(na.omit(as.numeric(List_subj1[[act]][[slice]][,3])))
  }
  assign(paste("gxmeanSubj1_act",act,sep=""),gx_mean_Subj1)
  assign(paste("gymeanSubj1_act",act,sep=""),gy_mean_Subj1)
  assign(paste("gzmeanSubj1_act",act,sep=""),gy_mean_Subj1)
  assign(paste("gxSDSubj1_act",act,sep=""),gx_stdev_Subj1)
  assign(paste("gySDSubj1_act",act,sep=""),gy_stdev_Subj1)
  assign(paste("gzSDSubj1_act",act,sep=""),gz_stdev_Subj1)
}

#consolidated mean for gx gy gz for act1 to act4 for subj1
gx_mean_Subj1 <- list(gxmeanSubj1_act1,gxmeanSubj1_act2,gxmeanSubj1_act3,
                    gxmeanSubj1_act4)
gy_mean_Subj1 <- list(gymeanSubj1_act1,gymeanSubj1_act2,gymeanSubj1_act3,
                    gymeanSubj1_act4)
gz_mean_Subj1 <- list(gzmeanSubj1_act1,gzmeanSubj1_act2,gzmeanSubj1_act3,
                    gzmeanSubj1_act4)
#consolidated SD for gx gy gz for act1 to act4 for subj1
gxSD_subj1 <- list(gxSDSubj1_act1,gxSDSubj1_act2,gxSDSubj1_act3,
                   gxSDSubj1_act4)
gySD_subj1 <- list(gySDSubj1_act1,gySDSubj1_act2,gySDSubj1_act3,
                   gySDSubj1_act4)
gzSD_subj1 <- list(gzSDSubj1_act1,gzSDSubj1_act2,gzSDSubj1_act3,
                   gzSDSubj1_act4)

# mean and SD for subj2--actv 1 to 4 and slice 1 to 6 each:

gx_mean_Subj2 <-  list()
gx_stdev_Subj2 <- list()
gy_mean_Subj2 <- list()
gy_stdev_Subj2 <- list()
gz_mean_Subj2 <- list()
gz_stdev_Subj2 <- list()
gxmeanSubj2_act1 <- list()
gxmeanSubj2_act2 <- list()
gxmeanSubj2_act3 <- list()
gxmeanSubj2_act4 <- list()
gymeanSubj2_act1 <- list()
gymeanSubj2_act2 <- list()
gymeanSubj2_act3 <- list()
gymeanSubj2_act4 <- list()
gzmeanSubj2_act1 <- list()
gzmeanSubj2_act2 <- list()
gzmeanSubj2_act3 <- list()
gzmeanSubj2_act4 <- list()
gxSDSubj2_act1 <- list()
gxSDSubj2_act2 <- list()
gxSDSubj2_act3 <- list()
gxSDSubj2_act4 <- list()
gySDSubj2_act1 <- list()
gySDSubj2_act2 <- list()
gySDSubj2_act3 <- list()
gySDSubj2_act4 <- list()
gzSDSubj2_act1 <- list()
gzSDSubj2_act2 <- list()
gzSDSubj2_act3 <- list()
gzSDSubj2_act4 <- list()

for(act in 1:4){
  for(slice in 1:6){
    gx_mean_Subj2[[slice]] <- mean(na.omit(as.numeric(List_subj2[[act]][[slice]][,1])))
    gx_stdev_Subj2[[slice]] <- sd(na.omit(as.numeric(List_subj2[[act]][[slice]][,1])))
    gy_mean_Subj2[[slice]]  <- mean(na.omit(as.numeric(List_subj2[[act]][[slice]][,2])))
    gy_stdev_Subj2[[slice]]  <- sd(na.omit(as.numeric(List_subj2[[act]][[slice]][,2])))
    gz_mean_Subj2[[slice]]  <- mean(na.omit(as.numeric(List_subj2[[act]][[slice]][,3])))
    gz_stdev_Subj2[[slice]]  <- sd(na.omit(as.numeric(List_subj2[[act]][[slice]][,3])))}
  assign(paste("gxmeanSubj2_act",act,sep=""),gx_mean_Subj2)
  assign(paste("gymeanSubj2_act",act,sep=""),gy_mean_Subj2)
  assign(paste("gzmeanSubj2_act",act,sep=""),gy_mean_Subj2)
  assign(paste("gxSDSubj2_act",act,sep=""),gx_stdev_Subj2)
  assign(paste("gySDSubj2_act",act,sep=""),gy_stdev_Subj2)
  assign(paste("gzSDSubj2_act",act,sep=""),gz_stdev_Subj2)}

#consolidated mean for gx gy gz for act1 to act4 for subj2
gxmean_Subj2 <- list(gxmeanSubj2_act1,gxmeanSubj2_act2,gxmeanSubj2_act3,
                    gxmeanSubj2_act4)
gymean_Subj2 <- list(gymeanSubj2_act1,gymeanSubj2_act2,gymeanSubj2_act3,
                    gymeanSubj2_act4)
gzmean_Subj2 <- list(gzmeanSubj2_act1,gzmeanSubj2_act2,gzmeanSubj2_act3,
                    gzmeanSubj2_act4)

#consolidated SD for gx gy gz for act1 to act4 for subj2
gxSD_Subj2 <- list(gxSDSubj2_act1,gxSDSubj2_act2,gxSDSubj2_act3,
                   gxSDSubj2_act4)
gySD_Subj2 <- list(gySDSubj2_act1,gySDSubj2_act2,gySDSubj2_act3,
                   gySDSubj2_act4)
gzSD_Subj2 <- list(gzSDSubj2_act1,gzSDSubj2_act2,gzSDSubj2_act3,
                   gzSDSubj2_act4)


# mean and SD for subj3--actv 1 to 4 and slice 1 to 6 each:
gx_mean_Subj3 <-  list()
gx_stdev_Subj3 <- list()
gy_mean_Subj3 <- list()
gy_stdev_Subj3 <- list()
gz_mean_Subj3 <- list()
gz_stdev_Subj3 <- list()
gxmeanSubj3_act1 <- list()
gxmeanSubj3_act2 <- list()
gxmeanSubj3_act3 <- list()
gxmeanSubj3_act4 <- list()
gymeanSubj3_act1 <- list()
gymeanSubj3_act2 <- list()
gymeanSubj3_act3 <- list()
gymeanSubj3_act4 <- list()
gzmeanSubj3_act1 <- list()
gzmeanSubj3_act2 <- list()
gzmeanSubj3_act3 <- list()
gzmeanSubj3_act4 <- list()
gxSDSubj3_act1 <- list()
gxSDSubj3_act2 <- list()
gxSDSubj3_act3 <- list()
gxSDSubj3_act4 <- list()
gySDSubj3_act1 <- list()
gySDSubj3_act2 <- list()
gySDSubj3_act3 <- list()
gySDSubj3_act4 <- list()
gzSDSubj3_act1 <- list()
gzSDSubj3_act2 <- list()
gzSDSubj3_act3 <- list()
gzSDSubj3_act4 <- list()


for(act in 1:4){
  for(slice in 1:6){
    gx_mean_Subj3[[slice]] <- mean(na.omit(as.numeric(List_subj3[[act]][[slice]][,1])))
    gx_stdev_Subj3[[slice]] <- sd(na.omit(as.numeric(List_subj3[[act]][[slice]][,1])))
    gy_mean_Subj3[[slice]]  <- mean(na.omit(as.numeric(List_subj3[[act]][[slice]][,2])))
    gy_stdev_Subj3[[slice]]  <- sd(na.omit(as.numeric(List_subj3[[act]][[slice]][,2])))
    gz_mean_Subj3[[slice]]  <- mean(na.omit(as.numeric(List_subj3[[act]][[slice]][,3])))
    gz_stdev_Subj3[[slice]]  <- sd(na.omit(as.numeric(List_subj3[[act]][[slice]][,3])))}
  assign(paste("gxmeanSubj3_act",act,sep=""),gx_mean_Subj3)
  assign(paste("gymeanSubj3_act",act,sep=""),gy_mean_Subj3)
  assign(paste("gzmeanSubj3_act",act,sep=""),gy_mean_Subj3)
  assign(paste("gxSDSubj3_act",act,sep=""),gx_stdev_Subj3)
  assign(paste("gySDSubj3_act",act,sep=""),gy_stdev_Subj3)
  assign(paste("gzSDSubj3_act",act,sep=""),gz_stdev_Subj3)}


#consolidated mean for gx gy gz for act1 to act4 for subj3
gxmean_Subj3 <- list(gxmeanSubj3_act1,gxmeanSubj3_act2,gxmeanSubj3_act3,
                    gxmeanSubj3_act4)
gymean_Subj3 <- list(gymeanSubj3_act1,gymeanSubj3_act2,gymeanSubj3_act3,
                    gymeanSubj3_act4)
gzmean_Subj3 <- list(gzmeanSubj3_act1,gzmeanSubj3_act2,gzmeanSubj3_act3,
                    gzmeanSubj3_act4)


#consolidated SD for gx gy gz for act1 to act4 for subj3
gxSD_Subj3 <- list(gxSDSubj3_act1,gxSDSubj3_act2,gxSDSubj3_act3,
                   gxSDSubj3_act4)
gySD_Subj3 <- list(gySDSubj3_act1,gySDSubj3_act2,gySDSubj3_act3,
                   gySDSubj3_act4)
gzSD_Subj3 <- list(gzSDSubj3_act1,gzSDSubj3_act2,gzSDSubj3_act3,
                   gzSDSubj3_act4)

###################14. Box Plot for statistical metrics----#####################

#box plot considering mean values of all the subjects in each activity
par(mfrow = c(1,4),mar=c(1,1,1,1))

#gx gy and gz for activity1 for all subj
boxplot(unlist(gxmeanSubj1_act1,gxmeanSubj2_act1,gxmeanSubj3_act1), 
        unlist(gymeanSubj1_act1,gymeanSubj2_act1,gymeanSubj3_act1), 
        unlist(gzmeanSubj1_act1,gzmeanSubj2_act1,gzmeanSubj3_act1),
        names=c("gxact1","gyact1","gzact1"),
        at = c(1,2,3),
        las = 1,
        col = c("pink","green","blue"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity1",
        ylab = "Gyroscope data")
legend("topleft",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("pink","green","blue"),
       text.col=c("red","green","blue"))

#gx gy and gz for activity2 for all subj
boxplot(unlist(gxmeanSubj1_act2,gxmeanSubj2_act2,gxmeanSubj3_act2), 
        unlist(gymeanSubj1_act2,gymeanSubj2_act2,gymeanSubj3_act2), 
        unlist(gzmeanSubj1_act2,gzmeanSubj2_act2,gzmeanSubj3_act2),
        names=c("gxact2","gyact2","gzact2"),
        at = c(1,2,3),
        las = 1,
        col = c("purple","orange","grey"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity2",
        ylab = "Gyroscope data")
legend("topleft",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("purple","orange","grey"),
       text.col=c("red","green","blue"))
title(main="Box plot for all the activities(mean)", col.main="blue",
      xlab="Activity1", cex.lab=0.1)

#gx gy and gz for activity3 for all subj
boxplot(unlist(gxmeanSubj1_act3,gxmeanSubj2_act3,gxmeanSubj3_act3), 
        unlist(gymeanSubj1_act3,gymeanSubj2_act3,gymeanSubj3_act3), 
        unlist(gzmeanSubj1_act3,gzmeanSubj2_act3,gzmeanSubj3_act3),
        names=c("gxact3","gyact3","gzact3"),
        at = c(1,2,3),
        las = 1,
        col = c("green","blue","pink"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity3",
        ylab = "Gyroscope data")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("green","blue","pink"),
       text.col=c("red","green","blue"))


#gx gy and gz for activity4 for all subj
boxplot(unlist(gxmeanSubj1_act4,gxmeanSubj2_act4,gxmeanSubj3_act4), 
        unlist(gymeanSubj1_act4,gymeanSubj2_act4,gymeanSubj3_act4), 
        unlist(gzmeanSubj1_act4,gzmeanSubj2_act4,gzmeanSubj3_act4),
        names=c("gxact4","gyact4","gzact4"),
        at = c(1,2,3),
        las = 1,
        col = c("grey","yellow","pink"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity4",
        ylab = "Gyroscope data")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("grey","yellow","pink"),
       text.col=c("red","green","blue"))

#box plot considering SD values of all the subjects in each activity
par(mfrow = c(1,4),mar=c(1,1,1,1))

#gx gy and gz for activity1 for all subj
boxplot(unlist(gxSDSubj1_act1,gxSDSubj2_act1,gxSDSubj3_act1), 
        unlist(gySDSubj1_act1,gySDSubj2_act1,gySDSubj3_act1), 
        unlist(gzSDSubj1_act1,gzSDSubj2_act1,gzSDSubj3_act1),
        names=c("gxact1","gyact1","gzact1"),
        at = c(1,2,3),
        las = 1,
        col = c("pink","green","blue"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity1",
        ylab = "Gyroscope data")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("pink","green","blue"),
       text.col=c("red","green","blue"))


#gx gy and gz for activity2 for all subj
boxplot(unlist(gySDSubj1_act2,gySDSubj2_act2,gySDSubj3_act2), 
        unlist(gySDSubj1_act2,gySDSubj2_act2,gySDSubj3_act2), 
        unlist(gzSDSubj1_act2,gzSDSubj2_act2,gzSDSubj3_act2),
        names=c("gxact2","gyact2","gzact2"),
        at = c(1,2,3),
        las = 1,
        col = c("purple","orange","grey"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity2",
        ylab = "Gyroscope data")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("purple","orange","grey"),
       text.col=c("red","green","blue"))
title(main="Box plot for all the activities(StdDev)", col.main="blue",
      xlab="Activity1", cex.lab=0.1)

#gx gy and gz for activity3 for all subj
boxplot(unlist(gxSDSubj1_act3,gxSDSubj2_act3,gxSDSubj3_act3), 
        unlist(gySDSubj1_act3,gySDSubj2_act3,gySDSubj3_act3), 
        unlist(gzSDSubj1_act3,gzSDSubj2_act3,gzSDSubj3_act3),
        names=c("gxact3","gyact3","gzact3"),
        at = c(1,2,3),
        las = 1,
        col = c("green","blue","pink"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity3",
        ylab = "Gyroscope data")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("green","blue","pink"),
       text.col=c("red","green","blue"))

#gx gy and gz for activity4 for all subj
boxplot(unlist(gxSDSubj1_act4,gxSDSubj2_act4,gxSDSubj3_act4), 
        unlist(gySDSubj1_act4,gySDSubj2_act4,gySDSubj3_act4), 
        unlist(gzSDSubj1_act4,gzSDSubj2_act4,gzSDSubj3_act4),
        names=c("gxact4","gyact4","gzact4"),
        at = c(1,2,3),
        las = 1,
        col = c("grey","yellow","pink"),
        border = "brown",
        horizontal = F,
        notch = F,
        xlab = "Activity4",
        ylab = "Gyroscope data")
legend("topright",legend=c("gx","gy","gz"),
       lty=1,lwd=2,pch=21,col=c("grey","yellow","pink"),
       text.col=c("red","green","blue"))

####################################End of the code############################

