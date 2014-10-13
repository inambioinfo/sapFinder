# .spplot
# .spplot(dobj,"test/parser_outdir/",".")
# Plots all of the spectrum with the output directory from \code{tanparser}.
#
# @param tandir the output directory from \code{tanparser}.
.spplot<-function(data,tandir,outdir)
{
    index_relcoord_map=list()  
    for(p in 2:length(data))  
    {
        for(i in 2:length(data[[p]][["IMUT"]]))
        {
            lines=unlist(strsplit(data[[p]][["IMUT"]][[i]],"\n"));
            for(l in lines)
            {
                arr=unlist(strsplit(l,"\t"));
                index_relcoord_map[[ arr[1] ]][[ "abc" ]]=as.numeric(arr[14])
                index_relcoord_map[[ arr[1] ]][[ "xyz" ]]=
                    nchar(arr[10])-as.numeric(arr[14])+1;
            }
        }
    }
    
    
    spectral_dir=paste(outdir,.SPECTRAL.DIR,sep="/");
    if(!file.exists(spectral_dir))  
    {
        dir.create(spectral_dir);
    }
    
    #maxnum_cut<-30;  
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
                par(mgp=c(1.6,0.6,0),mar=c(5,4,5,0.5),cex=0.9);
                dr<-read.table(paste(tandir,"/",fr,sep=""),
                    header=FALSE,stringsAsFactors=FALSE);

                mz=round(dr$V1,digits=3)
                int=dr$V2
                int=int/max(int)*100    
                int.max<-max(int,na.rm=TRUE)
                plot(mz,int,type="h",ylim=c(0,130),
                     yaxs="i",cex.lab=1.05,
                     font.lab=2,cex.main=0.65,
                     xlab="",ylab="Intensity(%)",axes="FALSE")
                mtext("MZ",side=1,line=4,font=2)     
                
                
                if(file.info(paste(tandir,"/",fm,sep=""))$size!=0)  
                {
                    dm<-read.table(paste(tandir,"/",fm,sep=""),
                            header=FALSE,stringsAsFactors=FALSE);
                    dm$V1=as.numeric(dm$V1)
                    dm$V2=as.numeric(dm$V2)
                    
                    #if(length(dm$V2)>maxnum_cut)    
                    #{
                    #    thed<-sort(dm$V2,decreasing=TRUE)[maxnum_cut]
                    #    dm<-subset(dm,V2>=thed,)
                    #}
                    
                    m.mz=round(dm$V1,digits=3)
                    m.int=dm$V2
                    m.label<-gsub(pattern="-H2(0|O)",replacement="O",x=dm$V3)  
                    m.label<-gsub(pattern="-NH3",replacement="*",x=m.label)  
                    m.label<-gsub(pattern="(\\d+)",
                                  replacement="(\\1)",x=m.label) 

                    m.int=m.int/max(m.int)*100    
                    
                    #colors<-rep(NA,length(m.label))	
                    colors_b<-c()
                    colors_y<-c()
                    label_b <-c()
                    label_y <-c()
                    int_b<-c()
                    int_y<-c()
                    mz_b<-c()
                    mz_y<-c()
                    for(i in 1:length(m.label))
                    {
                        fragment_coord<-as.numeric(gsub("(\\d+)","\\1",
                            regmatches(m.label[i],
                            gregexpr("(\\d+)",m.label[i]))));	
                        if(grepl(pattern="[xyz]",x=m.label[i]) == TRUE)
                        {
                            mz_y[i]=m.mz[i]
                            label_y[i]=m.label[i]
                            int_y[i]=m.int[i]
                            if(fragment_coord>=index_relcoord_map[[bn]][["xyz"]])
                            {
                                colors_y[i]="brown3";
                            }
                            else
                            {
                                colors_y[i]="cornflowerblue";
                            }
                        }
                        else
                        {
                            mz_b[i]=m.mz[i]
                            label_b[i]=m.label[i]
                            int_b[i]=m.int[i]
                            if(fragment_coord>=index_relcoord_map[[bn]][["abc"]])
                            {
                                colors_b[i]="brown3";
                            }
                            else
                            {
                                colors_b[i]="darkgreen";
                            }
                        }
                    }	

                    if(length(mz_b)>0)
                    {
                        colors_b<-na.omit(colors_b)
                        label_b<-na.omit(label_b)
                        mz_b<-na.omit(mz_b)
                        int_b<-na.omit(int_b)

                        axis(1,mz_b,label_b,las=2,labels=FALSE)
                        text(mz_b,-15,labels=label_b,
                            col=colors_b,xpd=TRUE,srt=90)
                        abline(v=mz_b,col=colors_b,lty=2,lwd=0.5)
                        lines(mz_b,int_b,type="h",lwd=1.1,col=colors_b)
                        points(mz_b,int_b,col=colors_b,cex=0.8)
                        text(mz_b,int_b,
                            labels=paste(sprintf("%.2f",mz_b),sep=" "),
                            cex=0.9,adj=c(-0.1,0.5),srt=90,col=colors_b)

                    }
                    if(length(mz_y)>0)
                    {
                        colors_y<-na.omit(colors_y)
                        label_y<-na.omit(label_y)
                        mz_y<-na.omit(mz_y)
                        int_y<-na.omit(int_y)

                        axis(3,mz_y,label_y,las=2,labels=FALSE)
                        text(mz_y,145,labels=label_y,
                            col=colors_y,xpd=TRUE,srt=90)
                        abline(v=mz_y,col=colors_y,lty=2,lwd=0.5)
                        lines(mz_y,int_y,type="h",lwd=1.1,col=colors_y)
                        points(mz_y,int_y,col=colors_y,cex=0.8)
                        text(mz_y,int_y,
                            labels=paste(sprintf("%.2f",mz_y),sep=" "),
                            cex=0.9,adj=c(-0.1,0.5),srt=90,
                            col=colors_y)
                    }
                }

                #plot(mz,int,type="h",ylim=c(0,130),yaxs="i",cex.lab=1.05,
                #	 font.lab=2,cex.main=0.65,xlab="MZ",ylab="Intensity(%)")
                #text(m.mz,m.int,
                #	 labels=paste(m.label,sprintf("%.4f",m.mz),sep=" "),
                #	 cex=0.8,adj=c(-0.1,0.5),srt=90,
                #	 col=colors)
                #lines(m.mz,m.int,type="h",lwd=1.1,
                #	  col=colors)
                axis(2)
                box()
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
