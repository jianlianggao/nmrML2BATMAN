readnmrML<-function(nmrMLfile)
{
  ## written by Dr. Daniel Jacob (INRA, France) and Dr. Jianliang Gao (Imperial College London)
  ## dependencies: library(XML) & library(base64enc)
  
  warnDef<-options("warn")$warn
  warnRead<-options(warn = -1)
  datapath<-nmrMLfile
  
  ## read in nmrML file
  tree <- xmlTreeParse(datapath)
  root <- xmlRoot(tree)
  
  # Acquisition Parameters
  SFO1 <- as.double(xmlAttrs(xmlElementsByTagName(root, "irradiationFrequency", recursive = TRUE)[[1]])["value"])
  SWH <-  as.double(xmlAttrs(xmlElementsByTagName(root, "sweepWidth", recursive = TRUE)[[1]])["value"])
  SW <- SWH/SFO1
  TD  <-  as.integer(xmlAttrs(xmlElementsByTagName(root, "DirectDimensionParameterSet", recursive = TRUE)[[1]])["numberOfDataPoints"])
  TEMP <- as.double(xmlAttrs(xmlElementsByTagName(root, "sampleAcquisitionTemperature", recursive = TRUE)[[1]])["value"])
  RELAXDELAY <- as.double(xmlAttrs(xmlElementsByTagName(root, "relaxationDelay", recursive = TRUE)[[1]])["value"])
  SPINNINGRATE <- as.double(xmlAttrs(xmlElementsByTagName(root, "spinningRate", recursive = TRUE)[[1]])["value"])
  PULSEWIDTH <- as.double(xmlAttrs(xmlElementsByTagName(root, "pulseWidth", recursive = TRUE)[[1]])["value"])
  
  # Instrument
  instrument <- xmlElementsByTagName(root, "instrumentConfiguration", recursive = TRUE)[[1]]
  instrument.name <- xmlAttrs(xmlElementsByTagName(instrument,"cvParam")[[1]])["name"]
  instrument.probe <- xmlAttrs(xmlElementsByTagName(instrument,"userParam")[[1]])["value"]
  
  ### ---  FID -----
  # <fidData compressed="true" encodedLength="183298" byteFormat="Complex128">
  what <- "double"
  endian <- "little"
  sizeof <- 8
  compression <- "gzip"
  
  fidData <- xmlElementsByTagName(root, "fidData", recursive = TRUE)[["acquisition.acquisition1D.fidData"]]
  b64string <- gsub("\n", "", xmlValue(fidData))
  byteFormat <- xmlAttrs(fidData)["byteFormat"]
  raws <- memDecompress(base64decode(b64string), type=compression)
  signal <- readBin(raws, n=length(raws), what=what, size=sizeof, endian = endian)
  td <- length(signal)
  rawR <- signal[seq(from = 1, to = td, by = 2)]
  rawI <- signal[seq(from = 2, to = td, by = 2)]
  
  mediar<-mean(as.integer(rawR[c((3*length(rawR)/4):length(rawR))]),na.rm = TRUE)
  mediai<--mean(as.integer(rawI[c((3*length(rawR)/4):length(rawR))]),na.rm = TRUE)
  rawR<-rawR-mediar
  rawI<-rawI-mediai
  fid <- rawR+1i*rawI
  
  
  ### ---  1R -----
  #<spectrumDataArray compressed="true" encodedLength="416028" byteFormat="float64">
  realData <- xmlElementsByTagName(root, "spectrumDataArray", recursive = TRUE)[["spectrumList.spectrum1D.spectrumDataArray"]]
  b64string <- gsub("\n", "", xmlValue(realData))
  byteFormat <- xmlAttrs(realData)["byteFormat"]
  compression <- "gzip"
  raws <- memDecompress(base64decode(b64string), type=compression)
  spec1r <- readBin(raws, n=length(raws), what=what, size=sizeof, endian = endian)
  
  ### ---  PPM -----
  SI <- length(spec1r)
  dppm <- SW/(SI-1)
  ppm_max <-as.double(xmlAttrs(xmlElementsByTagName(root, "xAxis", recursive = TRUE)[[1]])["startValue"])
  ppm_min <-as.double(xmlAttrs(xmlElementsByTagName(root, "xAxis", recursive = TRUE)[[1]])["endValue"])
  ppm <- seq(from=ppm_min, to=ppm_max, by=dppm)
  
  ### --- get filename from datapath -----
  snam_path<-NULL
  tempIndex <- grep("/", nmrMLfile)
  if (length(tempIndex)) {
    temp = strsplit(nmrMLfile, "/")
    snam<- temp[[1]][length(temp[[1]])]
    snam_path<-substr(nmrMLfile, 1, (nchar(nmrMLfile)-nchar(snam)))
    snam<- substr(snam,1,nchar(snam)-6)
  } else {
    snam<- substr(nmrMLfile[[1]],1,nchar(nmrMLfile[[1]])-6)
  }
  
  dir2 <-paste(snam_path, "NMRData.txt", sep="")
  ### --- combine the ppm/spectrum with title line
  snam <- cbind("ppm", snam)
  sa <- cbind(rev(ppm),spec1r)
  colnames(sa)<- snam
  warnRead<-options(warn = warnDef)
  
  write.table(sa,file=dir2,row.names=FALSE,col.names=TRUE,quote=FALSE,sep = "\t")
  return (sa)
}