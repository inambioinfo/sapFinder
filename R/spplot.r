# .spplot
# 
# Plots all of the spectrum with the output directory from \code{tanparser}.
#
# @param tandir the output directory from \code{tanparser}.
.spplot<-function(tandir,outdir)
{
    spectral_dir=paste(outdir,.SPECTRAL.DIR,sep="/");
    if(!file.exists(spectral_dir))  
    {
        dir.create(spectral_dir);
    }
    for(fm in list.files(tandir))
    {
        m=regexpr("_ms2match\\.txt",fm,perl=TRUE);
        if(m[1]!=-1)
        {
            
            bn=regmatches(fm,m,invert=TRUE)[[1]][1];
            fr=paste(bn,"_rawPeakList.txt",sep="")
            fg=paste(spectral_dir,"/",bn,".png",sep="")
            
            if(file.exists(paste(tandir,"/",fr,sep="")))
            {
                png(fg,width=520, height=420);
                par(mgp=c(1.6,0.6,0),mar=c(3,3,0.5,0.5));
                dr<-read.table(paste(tandir,"/",fr,sep=""),
                            header=FALSE,stringsAsFactors=FALSE);
                dm<-read.table(paste(tandir,"/",fm,sep=""),
                            header=FALSE,stringsAsFactors=FALSE);
                
                dm$V1=as.numeric(dm$V1)
                dm$V2=as.numeric(dm$V2)
                
                mz=round(dr$V1,digits=3)
                int=dr$V2
                int.max<-max(int,na.rm=TRUE)
                
                m.mz=round(dm$V1,digits=3)
                m.int=dm$V2
                m.label<-gsub(pattern="-H20",replacement="O",x=dm$V3)  
                m.label<-gsub(pattern="(\\d+)",replacement="(\\1)",x=m.label) 
                plot(mz,int,type="h",ylim=c(0,130),yaxs="i",cex.lab=1.05,
                    font.lab=2,cex.main=0.65,xlab="MZ",ylab="Intensity(%)")
                text(m.mz,m.int,
                    labels=paste(m.label,sprintf("%.4f",m.mz),sep=" "),
                    cex=0.8,adj=c(-0.1,0.5),srt=90,
                    col=ifelse(grepl(pattern="y",x=m.label),"red","#32CD32"))
                lines(m.mz,m.int,type="h",lwd=1.1,
                    col=ifelse(grepl(pattern="y",x=m.label),"red","#32CD32"))
                dev.off()     
            }
            else
            {
                print(paste("I/O error:",paste(tandir,"/",fr,sep=""),
                            "doesn't exists!",sep=""))
            }
        }
    }
}
