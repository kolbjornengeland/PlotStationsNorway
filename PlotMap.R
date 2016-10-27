


load_data_covers<-function(GisFolder,ccfile="inst/feltparametre_flomstasjoner145.txt"){
  if (!require('maptools')) {
    stop('The package maptools was not installed')
  }
  
  
  #Omriss av Norge
  Norge<<-readShapeSpatial(paste(GisFolder,'norge.shp',sep=''))
  
  # Shape fil med grenser for nedbørsflet
  Mycatchments<<-readShapeSpatial(paste(GisFolder,'Hydrologi_TotalNedborfeltMalestasjon.shp',sep=''))
  
  # Shape fil med målestasjoner
  MyStations<<-readShapeSpatial(paste(GisFolder,'Hydrologi_MaleserieMalestasjon.shp',sep=''))
  spar<<-read.table(ccfile,sep="\t",header=TRUE)
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




plot_map_points<-function(cvalues,mappoints=forecasting_s,mapborders=Norge,pname='Spectral',pinv=TRUE,v_index=NA,c_index=NA,p_index=NA,cbins=NA,legtitle=NA){
  if (!require('maptools')) {
    stop('The package maptools was not installed')
  }
  if (!require('RColorBrewer')) {
      stop('The package RColorBrewer was not installed')
  }  
windows(7,8)
par(mar=c(0,0,1,0))
plot(mapborders)
# To match the station IDs between the point cover and the values
forecasting_ss<-mappoints[match(paste(rownames(corr_all),'.0',sep=''),mappoints$stID),]

if(!is.na(cbins)){
nco<-length(cbins)-1
farve = brewer.pal(nco, pname)
if(pinv)farve<-farve[nco:1]
legendtext<-c("Not significant",paste('<',cbins[2:length(cbins)]))
if(is.na(c_index)&is.na(p_index))legendtext<-paste('<',cbins[1:length(cbins)])
}
else{
cbinst<<-sort(unique(cvalues[,v_index]))
nco<-length(cbinst)
farve = brewer.pal(nco, "Spectral")
farve<-farve[nco:1]
legendtext<-as.character(cbinst)
}

point_color<-rep(NA,dim(forecasting_ss)[1])

# We have a colomn of critical threshold that defines significance
if(!is.na(c_index)){
# color of the ignisificant points
point_color[which(cvalues[,v_index] <= cvalues[,c_index])]<-"grey"
# color of the significant points
for(i in 1 : (length(cbins)-1))
point_color[which((cvalues[,v_index] > cvalues[,c_index]) & (cvalues[,v_index] > cbins[i] & cvalues[,v_index] <= cbins[i+1]) )]<-farve[i]
lcol=c("grey",farve)
}


# No bins are given, unique colors to each value!
if(is.na(cbins)){
# color of the significant points
for(i in 1 : (length(cbinst)))
point_color[which(cvalues[,v_index] == cbinst[i])]<-farve[i]
lcol=farve
}

# No sinificance test is given. We give all values below the minimum value in cbins grey color
if(is.na(c_index)&is.na(p_index)&!is.na(cbins)){
  # color of the lowest points
  point_color[which(cvalues[,v_index] <= cbins[1])]<-"grey"
  # color of the significant points
  for(i in 1 : (length(cbins)-1))
    point_color[which((cvalues[,v_index] > cbins[i] & cvalues[,v_index] <= cbins[i+1]) )]<-farve[i]
  lcol=c("grey",farve)
}




if(!is.na(p_index)){
  # color of the ignisificant points
  point_color[which(cvalues[,p_index] >= 0.10 )]<-"grey"
  # color of the significant points
  for(i in 1 : (length(cbins)-1))
    point_color[which((cvalues[,p_index] < 0.10) & (cvalues[,v_index] > cbins[i] & cvalues[,v_index] <= cbins[i+1]) )]<-farve[i]
  lcol=c("grey",farve)
}





points(forecasting_ss,pch=19,cex=1.5, col=point_color)
legend('bottomright',pch=19, col=lcol,legend=legendtext,title=legtitle)
}
