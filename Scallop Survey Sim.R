# load the library
library(biomod2)
library(raster)
library(readr)
library(plyr)
library(sp)
library(automap)
library(gstat)
library(randomForest)
library(colorRamps)
library(maptools)
library(rgdal)

options(scipen = 999)


setwd("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation")

tr = 50
biomod_run_num = 20

#COAST = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/GIS/US_Coastlines/US_Coast.shp")

# --------load our species data----
scallop = read.csv("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/NGOM161209.csv")
#scallop = read.csv("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/NGOM16.csv")

DataSpecies = scallop[,c(7,8,9)]
DataSpecies$expcatnum = ifelse(DataSpecies$abundance > 15, 1, 0)
names(DataSpecies)[1] = 'Y_WGS84'
names(DataSpecies)[2] = 'X_WGS84'
names(DataSpecies)[4] = 'scallop'

# the name of studied species
myRespName <-  'scallop'
# the presence/absences data for our species
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]



#----Load FVCOM Data----
#   0       -     431
#jan 1978       Dec 2013

FVCOM = read_csv("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/FVCOM data17full.csv", col_names  = T)


#substations = read.csv("FVCOM.StationsDIST.csv")
substations = read.csv("FVCOM.Stations.Surveyclip2.csv")
#clip = c(514,513) # Select statistical areas to subset FVCOM data
#substations = subset(substations, area %in% clip)

#substations = subset(substations, distance_offshore<0.265)
#plot(substations$latitude ~ substations$longitude)

FVCOM = FVCOM[is.element(FVCOM$stationID, substations$id),]


FVCOM = FVCOM[!is.na(FVCOM$depth) & !is.na(FVCOM$sediment), ]

#make FVCOM range match scallop data
#FVCOM = subset(FVCOM, longitude>= min(scallop$long_start) & longitude<= max(scallop$long_start))
#FVCOM = subset(FVCOM, latitude>= min(scallop$lat_start) & latitude<= max(scallop$lat_start))


#plot(FVCOM$latitude ~ FVCOM$longitude)
#space=rep(0,(nrow(FVCOM)))

#FVCOM = cbind(space,FVCOM)

#yr <- 1978:2013

#pull out temp, salinity, u, and v data from correct year
ts1978=FVCOM[,c(2:7, 8:19, 440:451, 872:883, 1304:1315)]
ts1979=FVCOM[,c(2:7, 20:31, 452:463, 884:895, 1316:1327)]
ts1980=FVCOM[,c(2:7, 32:43, 464:475, 896:907, 1328:1339)]
ts1981=FVCOM[,c(2:7, 44:55, 476:487, 908:919, 1340:1351)]
ts1982=FVCOM[,c(2:7, 56:67, 488:499, 920:931, 1352:1363)]
ts1983=FVCOM[,c(2:7, 68:79, 500:511, 932:943, 1364:1375)]
ts1984=FVCOM[,c(2:7, 80:91, 512:523, 944:955, 1376:1387)]
ts1985=FVCOM[,c(2:7, 92:103, 524:535, 956:967, 1388:1399)]
ts1986=FVCOM[,c(2:7, 104:115, 536:547, 968:979, 1400:1411)]
ts1987=FVCOM[,c(2:7, 116:127, 548:559, 980:991, 1412:1423)]
ts1988=FVCOM[,c(2:7, 128:139, 560:571, 992:1003, 1424:1435)]
ts1989=FVCOM[,c(2:7, 140:151, 572:583, 1004:1015, 1436:1447)]
ts1990=FVCOM[,c(2:7, 152:163, 584:595, 1016:1027, 1448:1459)]
ts1991=FVCOM[,c(2:7, 164:175, 596:607, 1028:1039, 1460:1471)]
ts1992=FVCOM[,c(2:7, 176:187, 608:619, 1040:1051, 1472:1483)]
ts1993=FVCOM[,c(2:7, 188:199, 620:631, 1052:1063, 1484:1495)]
ts1994=FVCOM[,c(2:7, 200:211, 632:643, 1064:1075, 1496:1507)]
ts1995=FVCOM[,c(2:7, 212:223, 644:655, 1076:1087, 1508:1519)]
ts1996=FVCOM[,c(2:7, 224:235, 656:667, 1088:1099, 1520:1531)]
ts1997=FVCOM[,c(2:7, 236:247, 668:679, 1100:1111, 1532:1543)]
ts1998=FVCOM[,c(2:7, 248:259, 680:691, 1112:1123, 1544:1555)]
ts1999=FVCOM[,c(2:7, 260:271, 692:703, 1124:1135, 1556:1567)]
ts2000=FVCOM[,c(2:7, 272:283, 704:715, 1136:1147, 1568:1579)]
ts2001=FVCOM[,c(2:7, 284:295, 716:727, 1148:1159, 1580:1591)]
ts2002=FVCOM[,c(2:7, 296:307, 728:739, 1160:1171, 1592:1603)]
ts2003=FVCOM[,c(2:7, 308:319, 740:751, 1172:1183, 1604:1615)]
ts2004=FVCOM[,c(2:7, 320:331, 752:763, 1184:1195, 1616:1627)]
ts2005=FVCOM[,c(2:7, 332:343, 764:775, 1196:1207, 1628:1639)]
ts2006=FVCOM[,c(2:7, 344:355, 776:787, 1208:1219, 1640:1651)]
ts2007=FVCOM[,c(2:7, 356:367, 788:799, 1220:1231, 1652:1663)]
ts2008=FVCOM[,c(2:7, 368:379, 800:811, 1232:1243, 1664:1675)]
ts2009=FVCOM[,c(2:7, 380:391, 812:823, 1244:1255, 1676:1687)]
ts2010=FVCOM[,c(2:7, 392:403, 824:835, 1256:1267, 1688:1699)]
ts2011=FVCOM[,c(2:7, 404:415, 836:847, 1268:1279, 1700:1711)]
ts2012=FVCOM[,c(2:7, 416:427, 848:859, 1280:1291, 1712:1723)]
ts2013=FVCOM[,c(2:7, 428:439, 860:871, 1292:1303, 1724:1735)]

#add year column
ts1978$year = 1978
ts1979$year = 1979
ts1980$year = 1980
ts1981$year = 1981
ts1982$year = 1982
ts1983$year = 1983
ts1984$year = 1984
ts1985$year = 1985
ts1986$year = 1986
ts1987$year = 1987
ts1988$year = 1988
ts1989$year = 1989
ts1990$year = 1990
ts1991$year = 1991
ts1992$year = 1992
ts1993$year = 1993
ts1994$year = 1994
ts1995$year = 1995
ts1996$year = 1996
ts1997$year = 1997
ts1998$year = 1998
ts1999$year = 1999
ts2000$year = 2000
ts2001$year = 2001
ts2002$year = 2002
ts2003$year = 2003
ts2004$year = 2004
ts2005$year = 2005
ts2006$year = 2006
ts2007$year = 2007
ts2008$year = 2008
ts2009$year = 2009
ts2010$year = 2010
ts2011$year = 2011
ts2012$year = 2012
ts2013$year = 2013


#Calculate max yearly temperature, min salinity, & mean current speed

x = list(ts1978=ts1978, ts1979=ts1979, ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
         ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
         ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
         ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
         ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
         ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
         ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,ts2013=ts2013)

x = x[32:36]

#year=1978

for (i in (1:length(x))){
  
  y=x[[i]]
  
  t=y[,c(7:18)]
  s=y[,c(19:30)]
  u=as.matrix(y[,c(31:42)])
  v=as.matrix(y[,c(43:54)])
  
  cs=(sqrt((u^2) + (v^2)))
  
  mt = apply(t, 1, mean)
  ms = apply(s, 1, mean)
  mcs = apply(cs, 1, mean)
  
  y=cbind((y[,c(55,1:6)]),mt,ms,mcs)
  row.names(y)<-NULL
  
  names(y)[8:10]=c("temperature_b","salinity_b","Current_b")
  
  x[[i]]=y
}

t = x
s = x
c = x

for (i in 1:length(x)){
  y=x[[i]]
  
  t[[i]]=y[,c(4,5,8)]
  s[[i]]=y[,c(4,5,9)]
  c[[i]]=y[,c(4,5,10)]
}

t = join_all(t, by = c("latitude", "longitude"))
s = join_all(s, by = c("latitude", "longitude"))
c = join_all(c, by = c("latitude", "longitude"))

dt = t
ds = s
dc = c


t$tmed = apply(t[-c(1,2)], 1, median, na.rm = TRUE)
s$smed = apply(s[-c(1,2)], 1, median, na.rm = TRUE)
c$cmed = apply(c[-c(1,2)], 1, median, na.rm = TRUE)

g = x[[1]]
dg =g

t = cbind(g[,c(4:7)],t[,c(1,2,11)])
s = cbind(g[,c(4:7)],s[,c(1,2,11)])
c = cbind(g[,c(4:7)],c[,c(1,2,11)])



#--------Load historic/current Environmental data--------

d <- raster("C:/Users/Mike/Documents/Scallop/Data/GIS/Depth/ne_atl_crm_v1.asc")
SA = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/SA.shp")

btmed = t[,c(2,1,7)];names(btmed)[3] = 'temp'
bsmed = s[,c(2,1,7)];names(bsmed)[3] = 'sal'
bcmed = c[,c(2,1,7)];names(bcmed)[3] = 'cur'
depth = g[,c(5,4,6)];names(depth)[3] = 'dep'
sediment = g[,c(5,4,7)]

     e = extent(-70.752980, -70.242089, 42.333522,  42.951969 )
     nr = tr*1.01
     nc = as.numeric(tr*(sqrt((e[1] - e[2])^2)/sqrt((e[3] - e[4])^2)))
         

     
colnames(btmed) <- c('y', 'x', 'z')
coordinates(btmed) =~ x + y
variogram = autofitVariogram(z ~ 1, btmed)
plot(variogram)
g = gstat(formula = z ~ 1, model = variogram$var_model, data = btmed, maxdist = 0.15)  
xrange = range(btmed$x)
yrange = range(btmed$y)
grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
                  latitude = seq(from = yrange[1], to = yrange[2], by = .004))
gridded(grid) = ~longitude + latitude
p = predict(g, newdata = grid)
btmed = data.frame(p);btmed = btmed[c(1:3)]
r <- raster(e, ncol=nc, nrow=nr)
bt <- rasterize(btmed[, c(1,2)], r, btmed[,3], fun=mean)
bt <- mask(bt, SA)

colnames(bsmed) <- c('y', 'x', 'z')
#e = extent(bsmed[,c(1,2)])
coordinates(bsmed) =~ x + y
variogram = autofitVariogram(z ~ 1, bsmed)
plot(variogram)
g = gstat(formula = z ~ 1, model = variogram$var_model, data = bsmed, maxdist = 0.15)  
xrange = range(bsmed$x)
yrange = range(bsmed$y)
grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
                  latitude = seq(from = yrange[1], to = yrange[2], by = .004))
gridded(grid) = ~longitude + latitude
p = predict(g, newdata = grid)
bsmed = data.frame(p);bsmed = bsmed[c(1:3)]
r <- raster(e, ncol=nc, nrow=nr)
bs <- rasterize(bsmed[, c(1,2)], r, bsmed[,3], fun=mean)
bs <- mask(bs, SA)

colnames(bcmed) <- c('y', 'x', 'z')
#e = extent(bcmed[,c(1,2)])
coordinates(bcmed) =~ x + y
variogram = autofitVariogram(z ~ 1, bcmed)
plot(variogram)
g = gstat(formula = z ~ 1, model = variogram$var_model, data = bcmed, maxdist = 0.15)  
xrange = range(bcmed$x)
yrange = range(bcmed$y)
grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
                  latitude = seq(from = yrange[1], to = yrange[2], by = .004))
gridded(grid) = ~longitude + latitude
p = predict(g, newdata = grid)
bcmed = data.frame(p);bcmed = bcmed[c(1:3)]
r <- raster(e, ncol=nc, nrow=nr)
bc <- rasterize(bcmed[, c(1,2)], r, bcmed[,3], fun=mean)
bc <- mask(bc, SA)


depth <- crop(d, e)
depth <- resample(depth, r)
depth = depth*-1
r <- raster(e, ncol=nc, nrow=nr)
depth <- mask(depth, SA)
plot(depth)

myExpl = stack(bt,bs,bc,depth)


mypal <- colorRampPalette(c("black", "blue", "purple","yellow"), bias=1)

#word.tif('envir_plot')
plot(myExpl, col = mypal(100))
#dev.off()

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)



rm(list=setdiff(ls(), c("myBiomodData", "scallop" , "FVCOM", "myResp" , "myExpl", 
                        "myRespXY", "myRespName",'bt','bs','bc','depth', 'e','nc','nr',
                        'biomod_run_num')))




#----biomod ensemble modeling----
myBiomodData


plot(myBiomodData)

# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 3. Computing the models... this gives all the models
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
 # models = c('GLM','GBM','ANN',"RF","MAXENT.Tsuruoka"),#for presence-absence cutoff 5
  #models = c('GLM','GBM','GAM', 'CTA','ANN','MARS','RF','MAXENT.Tsuruoka'),#for presence-absence cutoff 10
 #models = c('GLM','GBM','GAM', 'ANN','RF'),#for presence-absence cutoff 15'
 models = c('GLM','GBM','GAM', 'RF'),#for presence-absence cutoff 15 mean not max/min
  models.options = myBiomodOption,
  NbRunEval=biomod_run_num,
  DataSplit=80,
  Prevalence=0.5,
  VarImport=3,
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))


myBiomodModelOut

# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
# print the dimnames of this object
dimnames(myBiomodModelEval)

# let's print the TSS scores of all models
myBiomodModelEval["TSS","Testing.data",,,]

# let's print the ROC scores of all selected models
myBiomodModelEval["ROC","Testing.data",,,]

# print variable importances
get_variables_importance(myBiomodModelOut)

# combine individual models to build some kind of meta-model... # exclude all models with TSS < 0.5
myBiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.5),
  prob.mean = T,
  prob.cv = T,
  prob.ci = T,
  prob.ci.alpha = 0.05,
  prob.median = T,
  committee.averaging = T,
  prob.mean.weight = T,
  prob.mean.weight.decay = 'proportional' )

# print summary
myBiomodEM

# get evaluation scores
get_evaluations(myBiomodEM)

# projection over the globe under current conditions
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = F,
  output.format = '.grd')

# summary of crated oject
myBiomodProj

## files created on hard drive
list.files("GuloGulo/proj_current/")

## make some plots sub-selected by str.grep argument
plot(myBiomodProj)




# ##------------Model Response Curves-------------------
# #models = c('GLM','GBM','GAM', 'ANN','RF')
# 
# myModel <- BIOMOD_LoadModels(myBiomodModelOut, models='GBM')
# myRespPlot2D <- response.plot2(models  = myModel,
#                                Data = get_formal_data(myBiomodModelOut,'expl.var'),
#                                show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                do.bivariate = FALSE,
#                                fixed.var.metric = 'mean',
#                                col = c("blue", "red"),
#                                legend = TRUE,
#                                data_species = get_formal_data(myBiomodModelOut,'resp.var'))
# 
# 
# 
# GBML1 = myRespPlot2D$layer.1; write.csv(GBML1, "GBML1.csv")
# GBML2 = myRespPlot2D$layer.2; write.csv(GBML2, "GBML2.csv")
# GBML3 = myRespPlot2D$layer.3;write.csv(GBML3, "GBML3.csv" )
# GBML4 = myRespPlot2D$ne_atl_crm_v1;write.csv(GBML4,"GBML4.csv")
# 
# 
# 
# matplot(L1$layer.1,L1,type="l",lty=1,col="#00000020")
# 
#  tt = dim(myRespPlot2D)
#  ttt = dimnames(myRespPlot2D)
# 
# myRespPlot3D <- response.plot2(models  = myModel[1],
#                                Data = get_formal_data(myBiomodModelOut,'expl.var'),
#                                show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                do.bivariate = TRUE,
#                                fixed.var.metric = 'median',
#                                data_species = get_formal_data(myBiomodModelOut,'resp.var'),
#                                display_title=FALSE)



##------------choose models to include based on evaluation scores----

k = t(as.data.frame(myBiomodModelEval))
wh = seq(from =1 , to =nrow(k) , by=4);k=k[wh,]
k = as.data.frame(k);k$combo = k$TSS + k$ROC;k=k[order(-k$combo),]

md = data.frame(model = rep(NA,nrow(k)),run = rep(NA,nrow(k)))
for (j in 1:nrow(k)){
  ff = strsplit(row.names(k[j,]), '[._]')[[1]]
  md[j,1] = ff[3]
  md[j,2] = gsub("RUN", "", ff[length(ff)-1]) 
  } 

evals = cbind(md,k); rownames(evals) <- c()

head(evals)

modelChoose = rep(NA, 50)
for (j in 1:50){
  mod = evals[j,1]
  ru = as.numeric(evals[j,2])
  gg = 1
  if (mod == 'GLM')gg=3
  if (mod == 'GBM')gg=2
  if (mod == 'GAM')gg=1
  if (mod == 'RF')gg=0
  wh = (ru*4)- gg
  modelChoose[j] = wh
}


# glm = k[seq(from =1 , to =nrow(k) , by=4),]
# gbm = k[seq(from =2 , to =nrow(k) , by=4),]
# rf = k[seq(from =3, to =nrow(k) , by=4),]
# maxent = k[seq(from =4 , to =nrow(k) , by=4),]



# ##get mean TSS and ROC scores for models and also plot them
# biomod_evaluations = models_scores_graph(myBiomodModelOut,metrics = c("ROC", "TSS"), by='models')
# biomod_evaluations = biomod_evaluations$data
# names(biomod_evaluations)[2] = 'mean ROC'; names(biomod_evaluations)[3] = 'mean TSS'
# ##write.csv(biomod_evaluations, "Evaluations15mean.csv")
# 
# # # # ###plot individual model type
# plot(myBiomodProj, str.grep = 'GLM')
# plot(myBiomodProj, str.grep = 'GBM')
# plot(myBiomodProj, str.grep = 'GAM')
# plot(myBiomodProj, str.grep = 'CTA')
# plot(myBiomodProj, str.grep = 'ANN')
# plot(myBiomodProj, str.grep = 'SRE')
# plot(myBiomodProj, str.grep = 'FDA')
# plot(myBiomodProj, str.grep = 'MARS')
# plot(myBiomodProj, str.grep = 'RF')
# plot(myBiomodProj, str.grep = 'MAXENT.Tsuruoka')


##mean of all runs selected models
d = myBiomodProj@proj@val@layers
d = stack(d)
dd = d[[modelChoose]]


# word.tif('ensemble plot')
# par(mfrow=c(3,4))
# par(mar=c(2,2,0.5,0.5))
# 
# 
# plot(d[[1]],legend=FALSE, axes=FALSE); axis(2)
# plot(d[[2]],legend=FALSE, axes=FALSE); 
# plot(d[[3]],legend=FALSE, axes=FALSE);
# plot(d[[4]],legend=FALSE, axes=FALSE); 
# plot(d[[5]],legend=FALSE, axes=FALSE); axis(2)
# plot(d[[6]],legend=FALSE, axes=FALSE); 
# plot(d[[7]],legend=FALSE, axes=FALSE);
# plot(d[[8]],legend=FALSE, axes=FALSE); 
# plot(d[[9]],legend=FALSE, axes=FALSE); axis(2);axis(1)
# plot(d[[10]],legend=FALSE, axes=FALSE); axis(1)
# plot(d[[11]],legend=FALSE, axes=FALSE); axis(1)
# plot(d[[12]],legend=FALSE, axes=FALSE); axis(1)
# 
# 
# dev.off()
# 
# 
# par(mfrow=c(1,1))
# 
# word.tif('ensemble plot scale')
# plot(d[[2]], legend.only=T) 
# dev.off()

#modelPick = c(3,4,7,8,10,11,13,14,15,16,19,20,23,28,31,32,33,34,38,40,42,44:48)#visually assess models and pick
#ddd = subset(dd,modelPick)

modelWeights = evals[(modelChoose),4]
mr = weighted.mean(dd, modelWeights)
plot(mr)

#mr = mean(dd) #simple mean
#plot(mr)

#writeRaster(mr, "biomodoutput522", format = "ascii")

#------BIOMOD Outputs & Match to Scallop data----


#mr <- raster("biomodoutput532.asc")
#word.tif('Meta_plot')
#plot(mr)
#dev.off()

scallop_GPM = read.csv("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/NGOMSCAL2016_GPM.csv")

names(scallop_GPM)[2] = 'station'
names(scallop_GPM)[7] = 'lat_start'
names(scallop_GPM)[8] = 'long_start'

scallop_GPM = merge(scallop_GPM, scallop, by = c('lat_start', 'long_start'))
scallop = scallop_GPM

#match biomod outputs to scallop tows
coordinates(scallop)=~long_start+lat_start

scal_matched <- data.frame(extract(mr, scallop))

scallop <-cbind(data.frame(scallop),scal_matched)
row.names(scallop)<-NULL

names(scallop)[34] = 'bmod'
scallop = scallop[!is.na(scallop$bmod),]

plot(log(scallop$BM_M2+1)~scallop$bmod)
abline(lm(log(BM_M2+1)~bmod, data=scallop))


scal = scallop[,c(18,27:30,34,1:2,6)]

names(scal)[1] = 'abundance'
names(scal)[7] = 'y'
names(scal)[8] = 'x'



#----------Random Forest Regression------

strat = read.csv("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Stratum.csv")
names(strat)[3] = "STRATUM"

strat$STRATUM = as.numeric(strat$STRATUM)
r <- raster(e, ncol=nc, nrow=nr)
strat <- rasterize(strat[, c(1,2)], r, strat[,3])

scal$abundance[ scal$abundance>3 ] <- 3

  
  
#SADen = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/NsbNcaSj.shp")

scalNSB = subset(scal, STRATUM=="NSB")
scalSJ = subset(scal, STRATUM=="SJ")
scalIB = subset(scal, STRATUM=="NCA")

srf=randomForest(x = scal[,c(6,5)], y= scal$abundance, strata=scal$STRATUM,
                    mtry=1.1, ntree = 1000, data = scal)  # all areas modeled together
srf

# srfNSB=randomForest(abundance ~  bmod + surveydepth + y + x  ,
#                  mtry=1.1,data = scalNSB)  # NSB
# srfNSB
# 
# srfSJ=randomForest(abundance ~  bmod + surveydepth  + y + x,
#                     mtry=1.1,data = scalSJ)  # SJ
# srfSJ
# 
# srfIB=randomForest(abundance ~  bmod + surveydepth  ,
#                     mtry=1.1,data = scalIB)  # IB
# #srf
#round(importance(srf), 2)

btd <- as.data.frame(rasterToPoints(bt)); names(btd)[3]="temp"
bsd <- as.data.frame(rasterToPoints(bs)); names(bsd)[3]="sal"
bcd <- as.data.frame(rasterToPoints(bc)); names(bcd)[3]="cur"
depd <- as.data.frame(rasterToPoints(depth)); names(depd)[3]="dep"
stratp <- as.data.frame(rasterToPoints(strat)); names(stratp)[3]="STRATUM"

rasdat = cbind(btd, min_salinity = bsd[,3], maxFlowMag2d = bcd[,3], surveydepth = depd[,3], STRATUM = stratp[,3])
names(rasdat)[3] = "max_temperature"

rasdat = merge(rasdat, as.data.frame(rasterToPoints(mr))); names(rasdat)[8] = "bmod"

rasdatAll = cbind(rasdat,predabu = predict(srf, newdata=rasdat, type='response'))
# rasdatNSB = cbind(rasdat,predabu = predict(srfNSB, newdata=rasdat, type='response'))
# rasdatIB = cbind(rasdat,predabu = predict(srfIB, newdata=rasdat, type='response'))
# rasdatSJ = cbind(rasdat,predabu = predict(srfSJ, newdata=rasdat, type='response'))

# rasdatNSB = subset(rasdatNSB, STRATUM==2)
# rasdatIB = subset(rasdatIB, STRATUM==3)
# rasdatSJ = subset(rasdatSJ, STRATUM==1)
# rasdatCom = rbind(rasdatNSB, rasdatIB, rasdatSJ)

#rasdat$STRATUM = as.numeric(rasdat$STRATUM)
rasabuAll <- rasterize(rasdatAll[, c(1,2)], r, rasdatAll[,9], fun=mean)
# rasabuCom <- rasterize(rasdatCom[, c(1,2)], r, rasdatCom[,9], fun=mean)

#par(mfrow=c(1,2))
mypal <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 1.6)
word.tif('Abu_plot96')
plot(rasabuAll, col = mypal(100))
dev.off()

#mypal <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 3)
#plot(rasabuCom, col = mypal(100))
#par(mfrow=c(1,1))

#writeRaster(rasabu, "scallop pred3", format = "ascii")



#----------Random Forest Regression- Cross validation plot-----
scal1 = scal
#scal1 = subset(scal, abundance<1)  

plot(NULL, xlim = c(20,100), ylim = c(20,100), main = '',
     xlab = '',
     ylab = '')
abline(0,1,col="dark blue", lwd = 5)

CVdata = data.frame()
for (i in 1:10){

split=sample(1:(nrow(scal1)), size = nrow(scal1)*0.1)
test = scal1[split,]
train = scal1[-split,]

srf=randomForest(abundance ~  bmod + surveydepth  ,
                 mtry=1.1,data = train)  # all areas modeled together

# srfNSB=randomForest(abundance ~  bmod + surveydepth  ,
#                     mtry=1.1,data = scalNSB)  # NSB
# 
 # srfSJ=randomForest(abundance ~  bmod + surveydepth  ,
 #                    mtry=1.1,data = scalSJ)  # SJ
# 
# srfIB=randomForest(abundance ~  bmod + surveydepth  ,
#                    mtry=1.1,data = scalIB)  # IB

CVdatan = data.frame(Observed = test[,1], Predicted = predict(srf, newdata=test, type='response'))

CVdata = rbind(CVdata, CVdatan)
#abline(   lm(Observed~Predicted, data=CVdatan), col=rgb(.8,.9,.95,alpha=0.5)           )

}

#word.tif('RFCV_plot_noline2')
 plot(CVdata$Observed ~ CVdata$Predicted, xlim = c(0,2.3), ylim = c(0,2.3), 
      xlab = "Observed", ylab="Predicted", pch=19)
 #abline(lm(Observed~Predicted, data=CVdata), col='red')
 abline(a = c(0,0), b = 1)
 #dev.off()
 
 
# plot(CVdata); abline(a = c(0,0), b = 1)

rrrr =  lm(Observed~Predicted, data=CVdata)
summary(rrrr)$coefficients


#----------Calculate Biomass------

SA = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/NGOM maps/NGOM sampling areas/Southern_areas.shp")

rascrop <- mask(rasabuAll, SA)
#plot(rascrop)

y = as.matrix(rascrop)
y[is.na(y)] <- 0
y = y*1300*1300/0.4/1000000

biomass_real = sum(y) 
biomass_real

#writeRaster(rasabu, "scallop pred2", format = "ascii")









#--------------Add in Substrata ------------

samp_list = list()

for (z in 1:500){


rasabu = rasabuAll

NSBH = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/NSB_H.shp")
NSBM = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/NSB_M.shp")
NSBL = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/NSB_L.shp")

SJH = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/SJ_H.shp")
SJM = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/SJ_M.shp")
SJL = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/SJ_L.shp")

IBH = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/NCA_H.shp")
IBM = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/NCA_M.shp")
IBL = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/Survey Simulation/Shapefiles/NCA_L.shp")

NSBHr <- mask(rasabu, NSBH)
NSBMr <- mask(rasabu, NSBM)
NSBLr <- mask(rasabu, NSBL)

SJHr <- mask(rasabu, SJH)
SJMr <- mask(rasabu, SJM)
SJLr <- mask(rasabu, SJL)

IBHr <- mask(rasabu, IBH)
IBMr <- mask(rasabu, IBM)
IBLr <- mask(rasabu, IBL)


# plot(NSBHr,NSBMr)
# plot(NSBMr, add=TRUE)
# plot(NSBLr, add=TRUE)
# 
# plot(SJHr, add=TRUE)
# plot(SJMr, add=TRUE)
# plot(SJLr, add=TRUE)
# 
# plot(IBHr, add=TRUE)
# plot(IBMr, add=TRUE)
# plot(IBLr, add=TRUE)




#--------------Sampling Simulation ONE STAGE------------

samp_sims = data.frame(SecondNey = seq(0,1, by=0.1))


samp_sims = cbind(data.frame(N = rep(120,nrow(samp_sims))),samp_sims,
                data.frame(meanRealBiomass = rep(NA,nrow(samp_sims))    ,
                           meanSimulatedBiomass = rep(NA,nrow(samp_sims)) ,
                           sdRvS = rep(NA,nrow(samp_sims)),
                           OF = rep(NA,nrow(samp_sims))))


for (t in 1:nrow(samp_sims)){

samp_stats = data.frame(real = rep(NA,100), sim = rep(NA,100),rvsDiff = rep(NA,10))

N = samp_sims[t,1] #number of tows
NeyAllocatefirst = 40
NeyAllocatesecond = samp_sims[t,2]

Randomfirst = 0
RandomSecond = 1-NeyAllocatesecond

for (j in 1:100){
  
  #How many tows to allocate to each area?
  NSBd = subset(scallop_GPM, STRATUM == 'NSB')
  SJd = subset(scallop_GPM, STRATUM == 'SJ')
  IBd = subset(scallop_GPM, STRATUM == 'NCA')
  
  ney_NSB = 40
  
  ney_SJ = 40
  
  ney_IB = 40
  
  bigareaSampNum = data.frame(area = c('ney_NSB','ney_SJ','ney_IB'),
                              sampleNum = c(ney_NSB,ney_SJ,ney_IB))
  
  
  are <- 1:3
  randAreaAllocate = N * Randomfirst
  allocations <- sample(are, randAreaAllocate, replace=TRUE)
  
  #length(which(allocations == 3))
  
  #allocate between neyman and random
  for (k in 1:nrow(bigareaSampNum)){
    y = bigareaSampNum[k,2]
    bigareaSampNum[k,2] = y+length(which(allocations == k))
  }
  
  for (k in 1:nrow(bigareaSampNum)){
    y = bigareaSampNum[k,2]
    if (y < 1){y =1}
    bigareaSampNum[k,2] = y
  }
  
  #----Neyman Allocation on NSB subareas
  NSBHd = subset(scallop_GPM, STRATUM == 'NSB' & DENSITY == 'High')
  NSBMd = subset(scallop_GPM, STRATUM == 'NSB' & DENSITY == 'Medium')
  NSBLd = subset(scallop_GPM, STRATUM == 'NSB' & DENSITY == 'Low')
  (N * NeyAllocatefirst)
  ney_NSBH = (bigareaSampNum[1,2] * NeyAllocatesecond) * (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))/
    (  
      (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))+
        (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))+
        (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))
    )
  
  ney_NSBM = (bigareaSampNum[1,2] * NeyAllocatesecond) * (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))/
    (  
      (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))+
        (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))+
        (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))
    )
  
  ney_NSBL = (bigareaSampNum[1,2] * NeyAllocatesecond) * (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))/
    (  
      (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))+
        (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))+
        (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))
    )
  
  NSBSampNum = data.frame(area = c('ney_NSBH','ney_NSBM','ney_NSBL'),
                          sampleNum = c(ney_NSBH,ney_NSBM,ney_NSBL))
  
  are <- 1:3
  NSBAllocate = bigareaSampNum[1,2] * RandomSecond
  allocations <- sample(are, NSBAllocate, replace=TRUE)
  
  #allocate between neyman and random
  for (k in 1:nrow(NSBSampNum)){
    y = NSBSampNum[k,2]
    NSBSampNum[k,2] = y+length(which(allocations == k))
  }
  
  
  #----Neyman Allocation on SJ subareas
  SJHd = subset(scallop_GPM, STRATUM == 'SJ' & DENSITY == 'High')
  SJMd = subset(scallop_GPM, STRATUM == 'SJ' & DENSITY == 'Medium')
  SJLd = subset(scallop_GPM, STRATUM == 'SJ' & DENSITY == 'Low')
  
  ney_SJH =(bigareaSampNum[2,2] * NeyAllocatesecond) * (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))/
    (  
      (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))+
        (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))+
        (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))
    )
  
  ney_SJM = (bigareaSampNum[2,2] * NeyAllocatesecond) * (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))/
    (  
      (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))+
        (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))+
        (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))
    )
  
  ney_SJL = (bigareaSampNum[2,2] * NeyAllocatesecond) * (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))/
    (  
      (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))+
        (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))+
        (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))
    )
  
  SJSampNum = data.frame(area = c('ney_SJH','ney_SJM','ney_SJL'),
                         sampleNum = c(ney_SJH,ney_SJM,ney_SJL))
  
  are <- 1:3
  SJAllocate = bigareaSampNum[2,2] * RandomSecond
  allocations <- sample(are, SJAllocate, replace=TRUE)
  
  #allocate between neyman and random
  for (k in 1:nrow(SJSampNum)){
    y = SJSampNum[k,2]
    SJSampNum[k,2] = y+length(which(allocations == k))
  }
  
  
  
  #----Neyman Allocation on IB subareas
  IBHd = subset(scallop_GPM, STRATUM == 'NCA' & DENSITY == 'High')
  IBMd = subset(scallop_GPM, STRATUM == 'NCA' & DENSITY == 'Medium')
  IBLd = subset(scallop_GPM, STRATUM == 'NCA' & DENSITY == 'Low')
  
  ney_IBH = (bigareaSampNum[3,2] * NeyAllocatesecond) * (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))/
    (  
      (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))+
        (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))+
        (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))
    )
  
  ney_IBM = (bigareaSampNum[3,2] * NeyAllocatesecond) * (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))/
    (  
      (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))+
        (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))+
        (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))
    )
  
  ney_IBL = (bigareaSampNum[3,2] * NeyAllocatesecond) * (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))/
    (  
      (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))+
        (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))+
        (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))
    )
  
  
  IBSampNum = data.frame(area = c('ney_IBH','ney_IBM','ney_IBL'),
                         sampleNum = c(ney_IBH,ney_IBM,ney_IBL))
  
  are <- 1:3
  IBAllocate = bigareaSampNum[3,2] * RandomSecond
  allocations <- sample(are, IBAllocate, replace=TRUE)
  
  #allocate between neyman and random
  for (k in 1:nrow(IBSampNum)){
    y = IBSampNum[k,2]
    IBSampNum[k,2] = y+length(which(allocations == k))
  }
  
  
  subareaSampNum = data.frame(subarea = c('ney_NSBH','ney_NSBM','ney_NSBL',
                                          'ney_SJH','ney_SJM','ney_SJL',
                                          'ney_IBH','ney_IBM','ney_IBL'),
                              sampleNum = c(NSBSampNum[1,2],NSBSampNum[2,2],NSBSampNum[3,2],
                                            SJSampNum[1,2],SJSampNum[2,2],SJSampNum[3,2],
                                            IBSampNum[1,2],IBSampNum[2,2],IBSampNum[3,2]))
  
  
  
  #change 0s to 1s in all sub areas
  for (k in 1:nrow(subareaSampNum)){
    y = subareaSampNum[k,2]
    if (y < 1){y =1}
    subareaSampNum[k,2] = y
  }
  
  #----Sample from real population following survey design
  
  NSBH_samp = sampleRandom(NSBHr, subareaSampNum[1,2], na.rm=TRUE, xy=TRUE)
  NSBM_samp = sampleRandom(NSBMr, subareaSampNum[2,2], na.rm=TRUE, xy=TRUE)
  NSBL_samp = sampleRandom(NSBLr, subareaSampNum[3,2], na.rm=TRUE, xy=TRUE)
  
  SJH_samp = sampleRandom(SJHr, subareaSampNum[4,2], na.rm=TRUE, xy=TRUE)
  SJM_samp = sampleRandom(SJMr, subareaSampNum[5,2], na.rm=TRUE, xy=TRUE)
  SJL_samp = sampleRandom(SJLr, subareaSampNum[6,2], na.rm=TRUE, xy=TRUE)
  
  IBH_samp = sampleRandom(IBHr, subareaSampNum[7,2], na.rm=TRUE, xy=TRUE)
  IBM_samp = sampleRandom(IBMr, subareaSampNum[8,2], na.rm=TRUE, xy=TRUE)
  IBL_samp = sampleRandom(IBLr, subareaSampNum[9,2], na.rm=TRUE, xy=TRUE)
  
  
  # #plot where sample tows are conducted
  all_samp = rbind(NSBH_samp, NSBM_samp, NSBL_samp, SJH_samp,SJM_samp,SJL_samp,
                   IBH_samp, IBM_samp, IBL_samp)
  mypal <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 1.6)

  word.tif('1_plot 96')
  plot(rasabu, col = mypal(100))
  points(all_samp, pch=20)
  dev.off()
  # 
  #------------------Estimate simulated biomass using Bootstrap--------------
  
  
  # make the number B
  B=10
  # make blank lists 10,000 long
  IB_H.v=numeric(B)
  IB_M.v=numeric(B)
  IB_L.v=numeric(B)
  
  SJ_H.v=numeric(B)
  SJ_M.v=numeric(B)
  SJ_L.v=numeric(B)
  
  NSB_H.v=numeric(B)
  NSB_M.v=numeric(B)
  NSB_L.v=numeric(B)
  
  for (i in 1:B){
    
    IB_H.v[i]=mean(sample(IBH_samp[,3],size=nrow(IBH_samp),replace=TRUE))
    IB_M.v[i]=mean(sample(IBM_samp[,3],size=nrow(IBM_samp),replace=TRUE))
    IB_L.v[i]=mean(sample(IBL_samp[,3],size=nrow(IBL_samp),replace=TRUE))
    
    SJ_H.v[i]=mean(sample(SJH_samp[,3],size=nrow(SJH_samp),replace=TRUE))
    SJ_M.v[i]=mean(sample(SJM_samp[,3],size=nrow(SJM_samp),replace=TRUE))
    SJ_L.v[i]=mean(sample(SJL_samp[,3],size=nrow(SJL_samp),replace=TRUE))
    
    NSB_H.v[i]=mean(sample(NSBH_samp[,3],size=nrow(NSBH_samp),replace=TRUE))
    NSB_M.v[i]=mean(sample(NSBM_samp[,3],size=nrow(NSBM_samp),replace=TRUE))
    NSB_L.v[i]=mean(sample(NSBL_samp[,3],size=nrow(NSBL_samp),replace=TRUE))
    
  }
  
  #area table
  areas = matrix(c('IBH', 'IBM', 'IBL', 'SJH', 'SJM','SJL','NSBH','NSBM','NSBL',
                   length(which(!is.na(values(IBHr)))),length(which(!is.na(values(IBMr)))),
                   length(which(!is.na(values(IBLr)))),length(which(!is.na(values(SJHr)))),
                   length(which(!is.na(values(SJMr)))), length(which(!is.na(values(SJLr)))),
                   length(which(!is.na(values(NSBHr)))), length(which(!is.na(values(NSBMr)))),
                   length(which(!is.na(values(NSBLr))))), ncol = 2)
  colnames(areas) = c('STRATUM', 'AREA') 
  
  
  #summary list
  x = list(IB_H.v, IB_M.v, IB_L.v, SJ_H.v, SJ_M.v, SJ_L.v, NSB_H.v, NSB_M.v, NSB_L.v)
  
  summary_stats = matrix(NA, nrow = length(x), ncol = 7) 
  colnames(summary_stats) = c('q0.025', 'q0.10', 'q0.15', 'q0.20', 'q0.25', 'Mean', 'q0.975')
  rownames(summary_stats) = c('IBH', 'IBM', 'IBL', 'SJH', 'SJM','SJL','NSBH','NSBM','NSBL')
  
  
  dredge = 0.4
  
  for (o in 1:length(x)){
    y=x[[o]]
    a = as.numeric(areas[[o,2]])
    y = y*1300*1300*a/dredge/1000000
    summary_stats[o,1] = quantile(y, 0.025)
    summary_stats[o,2] = quantile(y, 0.10)
    summary_stats[o,3] = quantile(y, 0.15)
    summary_stats[o,4] = quantile(y, 0.20)
    summary_stats[o,5] = quantile(y, 0.25)
    summary_stats[o,6] = mean(y)
    summary_stats[o,7] = quantile(y, 0.975)
    
  }
  
  biomass_simulated = sum(summary_stats[,6])
  
  samp_stats[j,1] = biomass_real
  samp_stats[j,2] = biomass_simulated
  samp_stats[j,3] = biomass_simulated - biomass_real
  
}

meanRealBiomass = mean(samp_stats[,1])
meanSimulatedBiomass = mean(samp_stats[,2])
sdRvS = sd(samp_stats[,3])
OF = mean(samp_stats[,3])

samp_sims[t,3] = meanRealBiomass
samp_sims[t,4] = meanSimulatedBiomass
samp_sims[t,5] = sdRvS
samp_sims[t,6] = OF
}



#######rank top 5
samp_sims$OFF = 0 + abs(samp_sims$OF)
samp_simsOrd <- samp_sims[order(samp_sims$OFF),] 
samp_simsOrd$er = samp_simsOrd$sdRvS/sqrt(100)
samp_simsOrd$zOFF = scale(samp_simsOrd$OFF,center = TRUE, scale = TRUE)
samp_simsOrd$zer = scale(samp_simsOrd$er,center = TRUE, scale = TRUE)
samp_simsOrd$zcomb = samp_simsOrd$zOFF+samp_simsOrd$zer 
#samp_sims$zcomb1 = 0 + abs(samp_simsOrd$zcomb)
samp_simsOrd$CV = samp_simsOrd$sdRvS/samp_simsOrd$meanSimulatedBiomass
samp_simsOrd$MSE = (samp_simsOrd$sdRvS)^2 + (samp_simsOrd$OFF)^2
samp_simsOrd$RMSE = (sqrt((samp_simsOrd$sdRvS)^2 + (samp_simsOrd$OF)^2)/samp_simsOrd$meanRealBiomass)*100
samp_simsOrd <- samp_simsOrd[order(samp_simsOrd$RMSE),] 

#samp_simsOrd = samp_simsOrd[c(1:4),]
#head(samp_simsOrd)

if (samp_simsOrd[1,2] == 0.5 & samp_simsOrd[11,2] == 1){
  samp_list[[length(samp_list)+1]] = samp_simsOrd
  print("got one!!")
}

print(z)

}








#--------------Sampling Simulation TWO STAGE------------

# samp_sims = expand.grid(0:10, 0:10)
# names(samp_sims)[1:2] = c('FirstNey', 'SecondNey')
# samp_sims$FirstNey = samp_sims$FirstNey/10; samp_sims$SecondNey = samp_sims$SecondNey/10
# 
# samp_sims = cbind(data.frame(N = rep(120,nrow(samp_sims))),samp_sims,
#                   data.frame(meanRealBiomass = rep(NA,nrow(samp_sims))    ,
#                              meanSimulatedBiomass = rep(NA,nrow(samp_sims)) ,
#                              sdRvS = rep(NA,nrow(samp_sims)),
#                              OF = rep(NA,nrow(samp_sims))))
#                              
#   
# for (t in 1:nrow(samp_sims)){
# 
#     samp_stats = data.frame(real = rep(NA,100), sim = rep(NA,100),rvsDiff = rep(NA,10))
#     
#     N = samp_sims[t,1] #number of tows
#     NeyAllocatefirst = samp_sims[t,2]
#     NeyAllocatesecond = samp_sims[t,3]
#     
#     Randomfirst = 1-NeyAllocatefirst
#     RandomSecond = 1-NeyAllocatesecond
#     
#     for (j in 1:100){
#     
#     #How many tows to allocate to each area?
#     NSB_area = length(which(!is.na(values(NSBHr))))+length(which(!is.na(values(NSBMr))))+
#       length(which(!is.na(values(NSBLr))))
#     
#     SJ_area = length(which(!is.na(values(SJHr))))+length(which(!is.na(values(SJMr))))+
#       length(which(!is.na(values(SJLr))))
#     
#     IB_area = length(which(!is.na(values(IBHr))))+length(which(!is.na(values(IBMr))))+
#       length(which(!is.na(values(IBLr))))
#     
#     NSBd = subset(scallop_GPM, STRATUM == 'NSB')
#     SJd = subset(scallop_GPM, STRATUM == 'SJ')
#     IBd = subset(scallop_GPM, STRATUM == 'NCA')
#     
#     ney_NSB = (N * NeyAllocatefirst) * (sd(NSBd$BM_M2)*NSB_area)/
#       ( (sd(NSBd$BM_M2)*NSB_area)+(sd(SJd$BM_M2)*SJ_area)+(sd(IBd$BM_M2)*IB_area))
#       
#     ney_SJ = (N * NeyAllocatefirst) * (sd(SJd$BM_M2)*SJ_area)/
#       ( (sd(NSBd$BM_M2)*NSB_area)+(sd(SJd$BM_M2)*SJ_area)+(sd(IBd$BM_M2)*IB_area))
#     
#     ney_IB = (N * NeyAllocatefirst) * (sd(IBd$BM_M2)*IB_area)/
#       ( (sd(NSBd$BM_M2)*NSB_area)+(sd(SJd$BM_M2)*SJ_area)+(sd(IBd$BM_M2)*IB_area))
#     
#     bigareaSampNum = data.frame(area = c('ney_NSB','ney_SJ','ney_IB'),
#                                 sampleNum = c(ney_NSB,ney_SJ,ney_IB))
#     
#     
#     are <- 1:3
#     randAreaAllocate = N * Randomfirst
#     allocations <- sample(are, randAreaAllocate, replace=TRUE)
#     
#     #length(which(allocations == 3))
#     
#     #allocate between neyman and random
#     for (k in 1:nrow(bigareaSampNum)){
#       y = bigareaSampNum[k,2]
#       bigareaSampNum[k,2] = y+length(which(allocations == k))
#     }
#     
#     for (k in 1:nrow(bigareaSampNum)){
#       y = bigareaSampNum[k,2]
#       if (y < 1){y =1}
#       bigareaSampNum[k,2] = y
#     }
#     
#     #----Neyman Allocation on NSB subareas
#     NSBHd = subset(scallop_GPM, STRATUM == 'NSB' & DENSITY == 'High')
#     NSBMd = subset(scallop_GPM, STRATUM == 'NSB' & DENSITY == 'Medium')
#     NSBLd = subset(scallop_GPM, STRATUM == 'NSB' & DENSITY == 'Low')
#     (N * NeyAllocatefirst)
#     ney_NSBH = (bigareaSampNum[1,2] * NeyAllocatesecond) * (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))/
#     (  
#      (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))+
#      (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))+
#      (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))
#     )
#     
#     ney_NSBM = (bigareaSampNum[1,2] * NeyAllocatesecond) * (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))/
#       (  
#         (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))+
#           (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))+
#           (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))
#       )
#     
#     ney_NSBL = (bigareaSampNum[1,2] * NeyAllocatesecond) * (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))/
#       (  
#         (sd(NSBHd$BM_M2)*length(which(!is.na(values(NSBHr)))))+
#           (sd(NSBMd$BM_M2)*length(which(!is.na(values(NSBMr)))))+
#           (sd(NSBLd$BM_M2)*length(which(!is.na(values(NSBLr)))))
#       )
#       
#     NSBSampNum = data.frame(area = c('ney_NSBH','ney_NSBM','ney_NSBL'),
#                                 sampleNum = c(ney_NSBH,ney_NSBM,ney_NSBL))
#     
#     are <- 1:3
#     NSBAllocate = bigareaSampNum[1,2] * RandomSecond
#     allocations <- sample(are, NSBAllocate, replace=TRUE)
#     
#     #allocate between neyman and random
#     for (k in 1:nrow(NSBSampNum)){
#       y = NSBSampNum[k,2]
#       NSBSampNum[k,2] = y+length(which(allocations == k))
#     }
#     
#       
#     #----Neyman Allocation on SJ subareas
#     SJHd = subset(scallop_GPM, STRATUM == 'SJ' & DENSITY == 'High')
#     SJMd = subset(scallop_GPM, STRATUM == 'SJ' & DENSITY == 'Medium')
#     SJLd = subset(scallop_GPM, STRATUM == 'SJ' & DENSITY == 'Low')
#     
#     ney_SJH =(bigareaSampNum[2,2] * NeyAllocatesecond) * (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))/
#       (  
#         (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))+
#           (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))+
#           (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))
#       )
#     
#     ney_SJM = (bigareaSampNum[2,2] * NeyAllocatesecond) * (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))/
#       (  
#         (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))+
#           (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))+
#           (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))
#       )
#     
#     ney_SJL = (bigareaSampNum[2,2] * NeyAllocatesecond) * (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))/
#       (  
#         (sd(SJHd$BM_M2)*length(which(!is.na(values(SJHr)))))+
#           (sd(SJMd$BM_M2)*length(which(!is.na(values(SJMr)))))+
#           (sd(SJLd$BM_M2)*length(which(!is.na(values(SJLr)))))
#       )
#     
#     SJSampNum = data.frame(area = c('ney_SJH','ney_SJM','ney_SJL'),
#                             sampleNum = c(ney_SJH,ney_SJM,ney_SJL))
#     
#     are <- 1:3
#     SJAllocate = bigareaSampNum[2,2] * RandomSecond
#     allocations <- sample(are, SJAllocate, replace=TRUE)
#     
#     #allocate between neyman and random
#     for (k in 1:nrow(SJSampNum)){
#       y = SJSampNum[k,2]
#       SJSampNum[k,2] = y+length(which(allocations == k))
#     }
#     
#     
#     
#     #----Neyman Allocation on IB subareas
#     IBHd = subset(scallop_GPM, STRATUM == 'NCA' & DENSITY == 'High')
#     IBMd = subset(scallop_GPM, STRATUM == 'NCA' & DENSITY == 'Medium')
#     IBLd = subset(scallop_GPM, STRATUM == 'NCA' & DENSITY == 'Low')
#     
#     ney_IBH = (bigareaSampNum[3,2] * NeyAllocatesecond) * (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))/
#       (  
#         (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))+
#           (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))+
#           (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))
#       )
#     
#     ney_IBM = (bigareaSampNum[3,2] * NeyAllocatesecond) * (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))/
#       (  
#         (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))+
#           (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))+
#           (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))
#       )
#     
#     ney_IBL = (bigareaSampNum[3,2] * NeyAllocatesecond) * (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))/
#       (  
#         (sd(IBHd$BM_M2)*length(which(!is.na(values(IBHr)))))+
#           (sd(IBMd$BM_M2)*length(which(!is.na(values(IBMr)))))+
#           (sd(IBLd$BM_M2)*length(which(!is.na(values(IBLr)))))
#       )
#       
#     
#     IBSampNum = data.frame(area = c('ney_IBH','ney_IBM','ney_IBL'),
#                            sampleNum = c(ney_IBH,ney_IBM,ney_IBL))
#     
#     are <- 1:3
#     IBAllocate = bigareaSampNum[3,2] * RandomSecond
#     allocations <- sample(are, IBAllocate, replace=TRUE)
#     
#     #allocate between neyman and random
#     for (k in 1:nrow(IBSampNum)){
#       y = IBSampNum[k,2]
#       IBSampNum[k,2] = y+length(which(allocations == k))
#     }
#     
#     
#     subareaSampNum = data.frame(subarea = c('ney_NSBH','ney_NSBM','ney_NSBL',
#                                             'ney_SJH','ney_SJM','ney_SJL',
#                                             'ney_IBH','ney_IBM','ney_IBL'),
#                                 sampleNum = c(NSBSampNum[1,2],NSBSampNum[2,2],NSBSampNum[3,2],
#                                               SJSampNum[1,2],SJSampNum[2,2],SJSampNum[3,2],
#                                               IBSampNum[1,2],IBSampNum[2,2],IBSampNum[3,2]))
# 
#     
#     
#     #change 0s to 1s in all sub areas
#     for (k in 1:nrow(subareaSampNum)){
#             y = subareaSampNum[k,2]
#       if (y < 1){y =1}
#             subareaSampNum[k,2] = y
#     }
#     
#     #----Sample from real population following survey design
#     
#     NSBH_samp = sampleRandom(NSBHr, subareaSampNum[1,2], na.rm=TRUE, xy=TRUE)
#     NSBM_samp = sampleRandom(NSBMr, subareaSampNum[2,2], na.rm=TRUE, xy=TRUE)
#     NSBL_samp = sampleRandom(NSBLr, subareaSampNum[3,2], na.rm=TRUE, xy=TRUE)
#     
#     SJH_samp = sampleRandom(SJHr, subareaSampNum[4,2], na.rm=TRUE, xy=TRUE)
#     SJM_samp = sampleRandom(SJMr, subareaSampNum[5,2], na.rm=TRUE, xy=TRUE)
#     SJL_samp = sampleRandom(SJLr, subareaSampNum[6,2], na.rm=TRUE, xy=TRUE)
#     
#     IBH_samp = sampleRandom(IBHr, subareaSampNum[7,2], na.rm=TRUE, xy=TRUE)
#     IBM_samp = sampleRandom(IBMr, subareaSampNum[8,2], na.rm=TRUE, xy=TRUE)
#     IBL_samp = sampleRandom(IBLr, subareaSampNum[9,2], na.rm=TRUE, xy=TRUE)
#     
#     
#     #plot where sample tows are conducted
#     # all_samp = rbind(NSBH_samp, NSBM_samp, NSBL_samp, SJH_samp,SJM_samp,SJL_samp,
#     #                  IBH_samp, IBM_samp, IBL_samp)
#     # mypal <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 3)
#     # 
#     # word.tif('22_plot')
#     # plot(rasabu, col = mypal(100))
#     # points(all_samp, pch=20)
#     # dev.off()
#     # 
#     #------------------Estimate simulated biomass using Bootstrap--------------
#     
#     
#     # make the number B
#     B=10
#     # make blank lists 10,000 long
#     IB_H.v=numeric(B)
#     IB_M.v=numeric(B)
#     IB_L.v=numeric(B)
#     
#     SJ_H.v=numeric(B)
#     SJ_M.v=numeric(B)
#     SJ_L.v=numeric(B)
#     
#     NSB_H.v=numeric(B)
#     NSB_M.v=numeric(B)
#     NSB_L.v=numeric(B)
#     
#     for (i in 1:B){
#       
#       IB_H.v[i]=mean(sample(IBH_samp[,3],size=nrow(IBH_samp),replace=TRUE))
#       IB_M.v[i]=mean(sample(IBM_samp[,3],size=nrow(IBM_samp),replace=TRUE))
#       IB_L.v[i]=mean(sample(IBL_samp[,3],size=nrow(IBL_samp),replace=TRUE))
#       
#       SJ_H.v[i]=mean(sample(SJH_samp[,3],size=nrow(SJH_samp),replace=TRUE))
#       SJ_M.v[i]=mean(sample(SJM_samp[,3],size=nrow(SJM_samp),replace=TRUE))
#       SJ_L.v[i]=mean(sample(SJL_samp[,3],size=nrow(SJL_samp),replace=TRUE))
#       
#       NSB_H.v[i]=mean(sample(NSBH_samp[,3],size=nrow(NSBH_samp),replace=TRUE))
#       NSB_M.v[i]=mean(sample(NSBM_samp[,3],size=nrow(NSBM_samp),replace=TRUE))
#       NSB_L.v[i]=mean(sample(NSBL_samp[,3],size=nrow(NSBL_samp),replace=TRUE))
#       
#     }
#     
#     #area table
#      areas = matrix(c('IBH', 'IBM', 'IBL', 'SJH', 'SJM','SJL','NSBH','NSBM','NSBL',
#                       length(which(!is.na(values(IBHr)))),length(which(!is.na(values(IBMr)))),
#      length(which(!is.na(values(IBLr)))),length(which(!is.na(values(SJHr)))),
#      length(which(!is.na(values(SJMr)))), length(which(!is.na(values(SJLr)))),
#      length(which(!is.na(values(NSBHr)))), length(which(!is.na(values(NSBMr)))),
#      length(which(!is.na(values(NSBLr))))), ncol = 2)
#       colnames(areas) = c('STRATUM', 'AREA') 
#      
#     
#     #summary list
#     x = list(IB_H.v, IB_M.v, IB_L.v, SJ_H.v, SJ_M.v, SJ_L.v, NSB_H.v, NSB_M.v, NSB_L.v)
#     
#     summary_stats = matrix(NA, nrow = length(x), ncol = 7) 
#     colnames(summary_stats) = c('q0.025', 'q0.10', 'q0.15', 'q0.20', 'q0.25', 'Mean', 'q0.975')
#     rownames(summary_stats) = c('IBH', 'IBM', 'IBL', 'SJH', 'SJM','SJL','NSBH','NSBM','NSBL')
#     
#     
#     dredge = 0.4
#     
#     for (o in 1:length(x)){
#       y=x[[o]]
#       a = as.numeric(areas[[o,2]])
#       y = y*1300*1300*a/dredge/1000000
#       summary_stats[o,1] = quantile(y, 0.025)
#       summary_stats[o,2] = quantile(y, 0.10)
#       summary_stats[o,3] = quantile(y, 0.15)
#       summary_stats[o,4] = quantile(y, 0.20)
#       summary_stats[o,5] = quantile(y, 0.25)
#       summary_stats[o,6] = mean(y)
#       summary_stats[o,7] = quantile(y, 0.975)
#       
#     }
#     
#     biomass_simulated = sum(summary_stats[,6])
#     
#     samp_stats[j,1] = biomass_real
#     samp_stats[j,2] = biomass_simulated
#     samp_stats[j,3] = biomass_simulated - biomass_real
#     
#     }
#     
#     meanRealBiomass = mean(samp_stats[,1])
#     meanSimulatedBiomass = mean(samp_stats[,2])
#     sdRvS = sd(samp_stats[,3])
#     OF = mean(samp_stats[,3])
#     
#     samp_sims[t,4] = meanRealBiomass
#     samp_sims[t,5] = meanSimulatedBiomass
#     samp_sims[t,6] = sdRvS
#     samp_sims[t,7] = OF
# }
# 
# 
# 
# #######rank top 5
# samp_sims$OFF = 0 + abs(samp_sims$OF)
# samp_simsOrd <- samp_sims[order(samp_sims$OFF),] 
# samp_simsOrd$er = samp_simsOrd$sdRvS/sqrt(100)
# samp_simsOrd$zOFF = scale(samp_simsOrd$OFF,center = TRUE, scale = TRUE)
# samp_simsOrd$zer = scale(samp_simsOrd$er,center = TRUE, scale = TRUE)
# samp_simsOrd$zcomb = samp_simsOrd$zOFF+samp_simsOrd$zer 
# #samp_sims$zcomb1 = 0 + abs(samp_simsOrd$zcomb)
# samp_simsOrd <- samp_simsOrd[order(samp_simsOrd$zcomb),] 
# 
# #samp_simsOrd = samp_simsOrd[c(1:4),]
# #head(samp_simsOrd)
# 
# samp_simsOrd

#----------------------------------end------------------------

# colnames(depth) <- c('y', 'x', 'z')
# #e = extent(depth[,c(1,2)])
# coordinates(depth) =~ x + y
# variogram = autofitVariogram(z ~ 1, depth)
# plot(variogram)
# g = gstat(formula = z ~ 1, model = variogram$var_model, data = depth, maxdist = 0.15)  
# xrange = range(depth$x)
# yrange = range(depth$y)
# grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
#                   latitude = seq(from = yrange[1], to = yrange[2], by = .004))
# gridded(grid) = ~longitude + latitude
# p = predict(g, newdata = grid)
# depth = data.frame(p);depth = depth[c(1:3)]
# r <- raster(e, ncol=nc, nrow=nr)
# depth <- rasterize(depth[, c(1,2)], r, depth[,3], fun=mean)
# plot(depth)



## select individual model types for BIOMOD
# samp_seq_CTA = seq(from = 2, to = biomod_run_num*5, by = 5)
# samp_seq_RF = seq(from = 3, to = biomod_run_num*5, by = 5)
# samp_seq_FDA = seq(from = 5, to = biomod_run_num*5, by = 5)
# for (i in 1:(biomod_run_num)){
#   CTA=samp_seq_CTA[i]
#   FDA=samp_seq_FDA[i]
#   RF = samp_seq_RF[i]
#   dd = stack(dd,d[[CTA]],d[[RF]], d[[FDA]])
# }


