#-------------------global Private variable-------------------
.AA.NUM.EVERY.LINE<-60;
.REPORT.DIR<-"report"
.PAGE.DIR<-"pages"
.DATA.DIR<-"data"
.JAVASCRIPT.DIR<-"js";
.CONTENT.JS<-"content.js";
.JQUERY.MIN.JS <- "jquery-1.10.2.min.js";
.QTIP.MIN.JS <- "jquery.qtip.min.js";
.INDEX.JS<-"index.js"
.CSS.DIR <- "css";
.INDEX.CSS <- "index.css";
.LEFT.CSS <- "left.css";
.CONTENT.CSS <- "content.css";
.QTIP.MIN.CSS <- "jquery.qtip.min.css";

.SPECTRAL.DIR="pages/spectral";

.AA.TABLE=list(
    A=71.03711,
    R=156.10111,
    N=114.04293,
    D=115.02694,
    C=103.00919,
    E=129.04259,
    Q=128.05858,
    G=57.02146,
    H=137.05891,
    I=113.08406,
    L=113.08406,
    K=128.09496,
    M=131.04049,
    F=147.06841,
    P=97.05276,
    S=87.03203,
    T=101.04768,
    W=186.07931,
    Y=163.06333,
    V=99.06841) #amino acid mass table

# .concat
# 
# cat a string;
#
# @param ... one or more character vectors.
# @keywords character
# @keywords internal 
.concat <- function( ... )
{
    if ( length( list( ... ) ) == 0 )
    {
        return ( "" );
    }
    
    return ( paste( ..., sep="" ) );
}
# .blank
#
# To print consecutive white space characters;
#
# @param num the numbers of white space character.
# @keywords internal 
.blank<-function(num=0)
{
    return (paste(rep("&nbsp",num),collapse=""));
}

# .tag
#
# Creates a html element tag.
#
# @param id element id.
# @param class element class.
# @param other other attributes.
.tag <- function( tag, id=NA, class=NA, other=NA )
{
    idString <- "";
    classString <- "";
    otherString <- "";
    
    if ( !missing( id ) ) 
    {
        idString <- .concat( " id=\"", id, "\"" );
    }
    
    if ( !missing( class ) )
    {
        classString <- .concat( " class=\"", 
            paste( class, sep=" ", collapse=" " ),"\"" );
    }
    
    if ( !missing( other ) )
    {
        otherString <- .concat( " ", other );
    }
    
    return ( .concat( "<", tag, classString, idString, otherString, ">" ) );
}

# .ctag
#
# Creates a html tag with ending.
#
# @param id html id.
# @param class html class.
# @param other other attributes.
.ctag <- function( tag, id=NA, class=NA, other=NA )
{
    idString <- "";
    classString <- "";
    otherString <- "";
    if ( !missing( id ) )
    {
        idString <- .concat( " id=\"", id, "\"" );
    }
    
    if ( !missing( class ) )
    {
        classString <- .concat( " class=\"", 
            paste( class, sep=" ",collapse=" " ), "\"" );
    }
    
    if ( !missing( other ) )
    {
        otherString <- .concat( " ", other );
    }
    
    return ( .concat( "<", tag, classString, idString, otherString, "/>" ) );
}

# .writeTable
#
# Prints a html table line.
#
# @param tg1 element1.
# @param tg2 element2.
# @param out the output file handle.
# @param bg background colors.
.writeTable<-function(...,tg1="tr",tg2="td",out,bg=NULL)
{
    
    elem <- c(...); 
    if(is.null(bg))
    {
        .write(.tag(tg1),out);
    }
    else
    {
        .write(.tag(tg1,other=.concat('style="background:',bg,'"')),out);
    }
    for(i in elem)
    {
        .write(.tag(tg2),i,.tag(paste("/",tg2,sep="")),out);
    }
    .write(.tag(paste("/",tg1,sep="")),out);
    
}

# .write
#
# Prints a strings to file.
.write <- function( ..., nobreak=FALSE, file ) 
{
    if ( missing( file ) ) 
    {
        args <- list( ... );
        file <- args[[length( args )]]; 
        args[[length( args )]] <- ""; 
    }  
    
    # string to be written to file
    string <- paste( args, sep="" ); 
    if ( nobreak )
    {  
        cat( string, file=file, sep="" );
    }
    else
    {
        cat( string, "\n", file=file, sep="" );
    }
}


# .mut_freq_heatmap
#
#  A function to draw heatmaps of SAP matrix.
#
# @param data the SAP map table file,such as COSMIC.v67.MS1.map.tsv.
.mut_freq_heatmap<-function(data)
{
    d<-read.delim(data,header=TRUE,stringsAsFactors=FALSE);
    
    tb<-table(paste(d$AA_wild,d$AA_mut,sep="-")) 
    aa_vector=c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F",
                "P","S","T","W","Y","V","*")
    
    m=matrix(nrow=21,ncol=21,dimnames=list(aa_vector,aa_vector))
    for(i in aa_vector) 
    {
        for(j in aa_vector) 
        {
            con<-tb[paste(i,j,sep="-")]
            if(is.na(con))
            {
                m[i,j]=0
            }
            else
            {
                m[i,j]=con
            }
        }
    }
    pheatmap(m,cluster_rows = FALSE, cluster_cols = FALSE,
        color=colorRampPalette(c("grey","orange","red"))(256),
        display_numbers = TRUE,number_format="%.0f",fontsize = 16)
    
    #basic stat
    stat_list=list()
    stat_list[["pro_num"]]=length(unique(d$Protein_ID));
    stat_list[["mut_num"]]=length(unique(d$ID));
    stat_list[["gene_num"]]=length(unique(d$Gene));
    return(stat_list);
}
# .precursor_error_hist
#
#  A function to draw histogram of precursor error.
#
# @param data The peptide summary file from \code{tanparser},
# such as sapr-peptideSummary.txt.

.precursor_error_hist<-function(data,error_limit=20,error_unit="ppm")
{
    #This is just a mock variable,and it will remove the note(not the warning)
    #caused by subset;
    delta_ppm<-delta_da<-NULL;
    if(error_unit== 'ppm')
    {
        x<-as.numeric(subset(data,abs(delta_ppm)<=error_limit,
            select=delta_ppm)$delta_ppm);
    }
    else #Daltons
    {
        x<-as.numeric(subset(data,abs(delta_da)<=error_limit,
            select=delta_ppm)$delta_ppm); 
    }
    h <- hist(x, plot = FALSE, breaks = 15);
    d <- density( x ) 
    
    plot( h , border = NA, freq = FALSE, xlab = "Precursor Error (ppm)", 
        ylab = "Density",font.lab=2,main="",cex.lab=1.2) 
    
    usr <- par( "usr" )
    ncolors <- 100
    dy <- ( usr[4] - usr[3] ) / ncolors; 
    colors <- colorRampPalette( c("yellow","orange","red") )(ncolors) 
    
    abline( h = axTicks(2) , col = "gray", lwd = .5 )
    
    for( i in 1:ncolors){
        clip( usr[1], usr[2], usr[3] + (i-1) * dy, usr[3] + i*dy ) 
        plot( h, add = TRUE, axes = FALSE, ylab = "", xlab = "", 
            col = colors[i], border = NA, freq = FALSE) 
    }
    # reset the clipping area.
    do.call( clip, as.list( usr) )
    
    
    plot( h, add = TRUE, lwd = .5 , freq = FALSE, xlab = "",
        ylab = "", axes = FALSE )
    lines( d, lwd = 4, col = "#22222288" )
    rug( x, col = "gray" ) 
    box()
}

# .wm_mass_hist
#  
#  A function to draw histogram of precursor mass.
#
# @param data the SAP map table file,such as sapr-peptideSummary.txt.
.wm_mass_hist<-function(data)
{
    #This is just a mock variable,and it will remove the note(not the warning)
    # caused by subset;
    isSAP<-mass<-NULL;
    mass_mut=as.numeric(subset(data,isSAP=="true",select=c(mass))$mass)
    mass_wild=as.numeric(subset(data,isSAP=="false",select=c(mass))$mass)
    
    ylimit=max(density(mass_mut)$y,density(mass_wild)$y);
    xlimit=max(density(mass_mut)$x,density(mass_wild)$x);
    plot(density(mass_wild),col="red",xlim=c(0,xlimit),ylim=c(0,ylimit),
        main="",xlab="Mass",ylab="Density",cex.lab=1.2,font.lab=2)
    lines(density(mass_mut),col="blue")
    legend("topright",c('wild peptide','mutatant peptide'), pch=c(15,15),
        col=c('red','blue'))
}
# .wm_evalue_hist
#  
#  A function to draw histogram of precursor mass.
#
# @param data the SAP map table file,such as sapr-peptideSummary.txt.
.wm_evalue_hist<-function(data)
{
    #This is just a mock variable,and it will remove the note(not the warning)
    # caused by subset;
    isSAP<-evalue<-NULL;
    evalue_mut=-log2(as.numeric(subset(data,isSAP=="true",
        select=c(evalue))$evalue))
    evalue_wild=-log2(as.numeric(subset(data,isSAP=="false",
        select=c(evalue))$evalue))
    ylimit=max(density(evalue_mut)$y,density(evalue_wild)$y);
    xlimit=max(density(evalue_mut)$x,density(evalue_wild)$x);
    plot(density(evalue_wild),col="red",xlim=c(0,xlimit),ylim=c(0,ylimit),
        main="",xlab="-log2(Evalue)",ylab="Density",cex.lab=1.2,font.lab=2)
    lines(density(evalue_mut),col="blue")
    legend("topright",c('wild peptide','mutatant peptide'), pch=c(15,15),
        col=c('red','blue'))
}


# .wm_charge_bar
#
#  A function to draw histogram of precursor charge.
#
# @param data the SAP map table file,such as sapr-peptideSummary.txt.
.wm_charge_bar<-function(data)
{
    
    #This is just a mock variable,and it will remove the note(not the warning)
    # caused by subset;
    isSAP<-charge<-NULL;    
    table_mut<-table(subset(data,isSAP=="true",select=c(charge)))
    table_wild<-table(subset(data,isSAP=="false",select=c(charge)))
    charge_tb<-data.frame(wild=rep(NA,7),
                        mut=rep(NA,7),
                        stringsAsFactors = FALSE,
                        row.names=c("2","3","4","5","6","7","8"))
    for( i in rownames(charge_tb))
    {
        if(is.na(table_wild[i])&& is.na(table_mut[i]))
        {
        }
        else if(is.na(table_wild[i]))
        {
            charge_tb[i,1]=0
            charge_tb[i,2]=as.numeric(table_mut[i])
        }
        else if(is.na(table_mut[i]))
        {
            charge_tb[i,1]=as.numeric(table_wild[i])
            charge_tb[i,2]=0
            
        }
        else
        {
            charge_tb[i,1]=as.numeric(table_wild[i])
            charge_tb[i,2]=as.numeric(table_mut[i])
        }
    }
    charge_tb<-as.matrix(na.omit(charge_tb))
    charge_tb<-t(charge_tb)
    charge_tb_percentage<-charge_tb
    charge_tb_percentage[1,]=round(charge_tb[1,]/sum(charge_tb[1,])*100,
                            digits=2) 
    charge_tb_percentage[2,]=round(charge_tb[2,]/sum(charge_tb[2,])*100,
                            digits=2) 
    bar<-barplot(charge_tb,beside=TRUE,
                col=c("lightblue","mistyrose"),
                legend=c("wild","mut"),
                ylim=c(0,max(charge_tb)*1.2),
                main="",
                ylab="Numbers",
                xlab="Precursor Charge",
                las=2,
                font.axis=2,
                cex.lab=1.2,
                font.lab=2,
                las=3)
    text(bar,charge_tb+max(charge_tb)*0.1,
        labels=paste(charge_tb,"(",charge_tb_percentage,"%)",sep=""),srt=30)
    box()
}
# .mut_pro_dist
#
#  A function to draw barplot of protein numbers.
#
# @param data the data object from \code{.dataHandle_R}.
.mut_pro_dist<-function(data)
{
    set<-rep(NA,length(data)-1)
    for(p in 2:length(data)) 
    {
        set[p-1]=length(data[[p]][["IMUT"]])-1;
        #print(data[[p]][["IMUT"]])
    }
    
    t<-table(set)
    n<-as.vector(t)
    s<-unlist(dimnames(t))
    space=max(n)/24
    
    bar=barplot(n,
                ylim=c(0,1.2*max(n)),
                cex.main=1.5,
                space=0.7,
                font.axis=1,
                las=1,
                tcl=-0.1,
                col="deepskyblue",
                xlab="SAP numbers of every protein",
                ylab="Protein numbers",
                font.lab=2,
                cex.lab=1.2)
    #mtext("peptide num",side=1,line=3,font=2) 
    #mtext("Number",side=2,line=2.5,font=2)
    text(bar,-space,labels=s,xpd=TRUE,cex=1.4,font=0.5,
        adj=1,srt=90,col="magenta2")   
    text(bar,(n+space),labels=t,pos=3,font=0.5,cex=1.2,srt=0,offset=0.7)
    abline(h=axTicks(2),lty=1,col="darkkhaki")  
    box()
}
