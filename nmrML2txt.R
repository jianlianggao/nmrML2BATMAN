## main funtion of nmrML-to-txt converter
nmrML2txt<-function(nmrMLDir, nmrMLzip, nmrMLfile, outputDir = getwd())
{
  ## written by Dr. Jianliang Gao, Imperial College London
  warnDef<-options("warn")$warn
  warnRead<-options(warn = -1)
  ## main function
  ## output data file directory is the current run dir
  dir2<-paste(outputDir[1],"/NMRdata.txt",sep="")
  
  cat("\n Start converting nmrML files...\n")

  ## get spectra data
  if (!missing(nmrMLDir))
  {
    sa<-readnmrMLDir(nmrMLDir)  
    write.table(sa,file=dir2,row.names=FALSE,col.names=TRUE,quote=FALSE,sep = "\t")
  } else if (!(missing(nmrMLzip)))  {
    sa<-readnmrMLzip(nmrMLzip)
    write.table(sa,file=dir2,row.names=FALSE,col.names=TRUE,quote=FALSE,sep = "\t")
  }  else if (!missing(nmrMLfile)) {
    sa<-readnmrML(nmrMLfile)
    write.table(sa,file=dir2,row.names=FALSE,col.names=TRUE,quote=FALSE,sep = "\t")
  } 
  
  
  cat("\n Completed \n")
  warnRead<-options(warn = warnDef)
}
