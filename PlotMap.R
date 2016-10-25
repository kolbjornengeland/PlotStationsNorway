


load_data_covers<-function(GisFolder){

  #Omriss av Norge
  Norge<<-readShapeSpatial(paste(GisFolder,'norge.shp',sep=''))
  
  # Shape fil med grenser for nedbÃ¸rsflet
  Mycatchments<<-readShapeSpatial(paste(GisFolder,'Hydrologi_TotalNedborfeltMalestasjon.shp',sep=''))
  
  # Shape fil med mÃ¥lestasjoner
  MyStations<<-readShapeSpatial(paste(GisFolder,'Hydrologi_MaleserieMalestasjon.shp',sep=''))
  spar<<-read.table("inst/feltparametre_flomstasjoner145.txt",sep="\t",header=TRUE)
  regine_nr<-as.integer(spar[,2]/100000000)
  main_nr<-(spar[,2]-regine_nr*100000000)/1000
  mnumbers<-(regine_nr*10000+main_nr)
# Sort the list according to station number
  spar<-spar[order(mnumbers),]
                        
# Select only streamflow stations
  MyStations<-MyStations[MyStations$stParaKode==1001,]
                        
  #Select only unique versions (some are duplicated)
  MyStations<-MyStations[!(duplicated(MyStations$stSamletID)),]
 
# Hack since station 24.8 in the flood forecasting system is 24.3 in the catchment list file 
#  Mycatchments[Mycatchments$stID=='24.3',]$stSamletID <- 002400008000
#  Mycatchments[Mycatchments$stID=='24.3',]$stID <- '24.8'
####################################################################                                    
  forecasting_c<<-Mycatchments[which((as.integer(substr(Mycatchments$stSamletID,1,4))*10000+as.integer(substr(Mycatchments$stSamletID,5,9)))%in%mnumbers),]
  forecasting_c<<-forecasting_c[order(forecasting_c$stSamletID),]                        
  forecasting_s<<-MyStations[which((as.integer(substr(MyStations$stSamletID,1,4))*10000+as.integer(substr(MyStations$stSamletID,5,9)))%in%mnumbers),]
  forecasting_s<<-forecasting_s[order(forecasting_s$stSamletID),]
}




plot_map_points<-function(cvalues,v_index=NA,c_index=NA,p_index=NA,s_index=NA,cbins=NA,legtitle=NA){
windows(7,8)
par(mar=c(0,0,1,0))
plot(Norge)
# To match the station IDs between the point cover and the values
forecasting_ss<-forecasting_s[match(paste(rownames(corr_all),'.0',sep=''),forecasting_s$stID),]

if(!is.na(cbins)){
nco<-length(cbins)-1
farve = brewer.pal(nco, "Spectral")
farve<-farve[nco:1]
legendtext<-c("Not significant",paste('<',cbins[2:length(cbins)]))
}
else{
cbinst<<-unique(cvalues[,c_index])
ncol<-length(cbinst)
farve = brewer.pal(nco, "Spectral")
farve<-farve[nco:1]
legendtext<-as.character(cbinst)
}

point_color<-rep(NA,dim(forecasting_ss)[1])

if(!is.na(c_index)){
# color of the ignisificant points
point_color[which(cvalues[,v_index] <= cvalues[,c_index])]<-"grey"
# color of the significant points
for(i in 1 : (length(cbins)-1))
point_color[which((cvalues[,v_index] > cvalues[,c_index]) & (cvalues[,v_index] > cbins[i] & cvalues[,v_index] <= cbins[i+1]) )]<-farve[i]
}

if(is.na(cbins)){

# color of the significant points
for(i in 1 : (length(cbinst)))
point_color[which(cvalues[,v_index] == cbins[i])]<-farve[i]
print(point_color)
print(cvalues[,v_index])
}



points(forecasting_ss,pch=19,cex=1.5, col=point_color)
legend('bottomright',pch=19, col=c("grey",farve),legend=legendtext,title=legtitle)




if(is.na(c_index)&is.na(s_index)&is.na(p_index)){
# plot the insignificant points
loc_tmp1 <- which(cvalues[,v_index] <= cbins[1])
points(forecasting_ss[loc_tmp1,],pch=19,cex=1.5, col="grey")

# plot the significant points
for(i in 1 : (length(cbins)-1)){
loc_tmp <- which((cvalues[,v_index] > cbins[i] & cvalues[,v_index] <= cbins[i+1]) )
points(forecasting_ss[loc_tmp,],pch=19,cex=1.5, col=farve[i])
}
legend('bottomright',pch=19, col=c("grey",farve),legend=c("Not significant",paste('<',cbins[2:length(cbins)])),title=legtitle)
}


if(!is.na(p_index)){
# plot the insignificant points
loc_tmp1 <- which(cvalues[,p_index] <= 0.10 )
points(forecasting_ss[loc_tmp1,],pch=19,cex=1.5, col="grey")

# plot the significant points
for(i in 1 : (length(cbins)-1)){
loc_tmp <- which((cvalues[,p_index] < 0.10) & (cvalues[,v_index] > cbins[i] & cvalues[,v_index] <= cbins[i+1]) )
points(forecasting_ss[loc_tmp,],pch=19,cex=1.5, col=farve[i])
}
legend('bottomright',pch=19, col=c("grey",farve),legend=c("Not significant",paste('<',cbins[2:length(cbins)])),title=legtitle)
}


}





#setwd('//nve/fil/h/HM/Interne Prosjekter/ensembleprognoser/script')




#Histogram av middelhøyde
#hist(spar[,18],xlab="Median høyde",ylab="Antall",main="")


#ptfiles<-list.files("../pt_1957-2014")

#pt_temp<-matrix(unlist(strsplit(ptfiles,'[.]')),nrow=145,byrow=TRUE)

#ptnumbers<-as.numeric(pt_temp[,1])*10000+(as.numeric(pt_temp[,2]))
# Sort the list according to station number
#ptfiles<-ptfiles[order(ptnumbers)]



#ns<-length(ptfiles)
#ave_temp<-matrix(ncol=13,nrow=ns)
#ave_prec<-matrix(ncol=13,nrow=ns)
#for(i in 1 : ns){
#  pt<-read.table(paste("../pt_1957-2014/",ptfiles[i],sep=""))
#  ave_temp[i,13]=mean(pt[,5])
#  ave_prec[i,13]=mean(pt[,4])
#  ave_temp[i,1:12]=as.numeric(by(pt[,5],pt[,2],mean))
#  ave_prec[i,1:12]=as.numeric(by(pt[,4],pt[,2],mean))
#}




#Need to use this since the GIS data has used station number ##162.4 in stead of 162.8
#mnumbers[135]=162*10000+4


#forecasting_c<-Mycatchments[which((as.integer(substr(Mycatchments$stSamletID,1,4))*10000+as.integer(substr(Mycatchments$stSamletID,5,9)))%in%mnumbers),]
#forecasting_c<-forecasting_c[order(forecasting_c$stSamletID),]


#forecasting_s<-MyStations[which((as.integer(substr(MyStations$stSamletID,1,4))*10000+as.integer(substr(MyStations$stSamletID,5,9)))%in%mnumbers),]
#forecasting_s<-forecasting_s[order(forecasting_s$stSamletID),]

#x_min<-(-10000)
#x_max<-max(summary(Norge)$bbox[3])
#y_min<-min(summary(Norge)$bbox[2])
#y_max<-max(summary(Norge)$bbox[4])


# Plotter kart
