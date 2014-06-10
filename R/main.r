#' @title dbCreator
#' @description An integrated function to generate variation-associated
#' database based on sample-specific NGS data or public SNV data.
#' @param vcf Input VCF file name. This file contains the information of gene
#' sequence variations.
#' @param annotation  Input annotation file name. 
#' It contains the gene annotation information and can be downloaded from UCSC
#' Genome Browser.Currently it supports RefSeq genes and ENSEMBL genes
#' annotation file.
#' @param refseq Input mRNA sequences file with FASTA format. It can be
#' downloaded from UCSC Genome Browser.
#' @param outdir Output directory. 
#' @param prefix The prefix of output file.
#' @param xmx The maximum Java heap size. The unit is "G".
#' @param xref Optional external cross-reference file,generally it's downloaded 
#' through BioMart.If this file is provided,the final html report will present
#' some relevant protein id or description.
#' @return A vector containing two file names. One is a FASTA format file
#' contains the mutated peptides, the normal protein sequences and their
#' reverse versions, and the other is a tab-delimited file contains detailed
#' variation information.
#' @export
#' @examples
#' vcf        <- system.file("extdata/sapFinder_test.vcf",
#'                         package="sapFinder")
#' annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
#'                         package="sapFinder")
#' refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
#'                         package="sapFinder")
#' xref       <- system.file("extdata/sapFinder_test_BioMart.Xref.txt",
#'                         package="sapFinder")
#' outdir     <- "db_dir"
#' prefix     <- "sapFinder_test"
#' db.files <- dbCreator(vcf=vcf, annotation=annotation,
#'                 refseq=refseq, outdir=outdir,
#'                 prefix=prefix,xref=xref)
dbCreator=function(vcf=NULL, annotation=NULL, refseq=NULL,
                outdir="./", prefix="test",xmx=NULL, xref="noxref")
{
    dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
    outdir <- normalizePath(outdir)
    outFileWithDir <- paste(outdir,"/",prefix,sep="")

    if(is.null(xmx))
    {
        ph<-paste("java","-jar",collapse=" ",sep=" ")
    }
    else
    {
        ph<-paste("java",paste("-Xmx",xmx,"G",sep=""),
                "-jar",collapse=" ",sep=" ")
    }
    run_savdbcreate<-paste(ph,
                            paste("\"",
                                paste(system.file("savdbcreate.v1.2.jar",
                                    package="sapFinder"),sep="",collapse=""),
                                "\"",sep=""), 
                            paste("\"",vcf,"\"",sep=""), 
                            paste("\"",annotation,"\"",sep=""), 
                            paste("\"",refseq,"\"",sep=""), 
                            "1",  #tryspin 
                            paste("\"",outFileWithDir,"\"",sep=""),
                            paste("\"",xref,"\"",sep=""),
                            collapse=" ",sep=" ");
    runinfo  <- system(command=run_savdbcreate,intern=TRUE);
    outFiles <- c(paste(outFileWithDir,".ms2.fasta",sep=""),
                paste(outFileWithDir,".ms2.map.tsv",sep=""))
    return(outFiles)
}

#' @title parserGear
#' @description This function is mainly for q-value calculation, 
#' protein inference and variant peptides spectra annotation.
#' @param file MS/MS search file. Currently, only XML format file 
#' of X!Tandem and DAT result of Mascot are supported. 
#' @param db A FASTA format database file used for MS/MS searching. 
#' Usually, it is from the output of the function \code{dbCreator}.
#' @param outdir Output directory. 
#' @param prefix The prefix of output file.
#' @param mutPrefix The prefix of variant peptides ID. Default is "VAR". 
#' "VAR" is the prefix which used by function \code{dbCreator}.
#' @param decoyPrefix The prefix of decoy sequences ID. Default is "###REV###".
#' "###REV###" is the prefix which used by function \code{dbCreator}.
#' @param alignment 0 or 1 to determine if peptide should be alignment or not.
#' Default is 1.
#' @param thread This parameter is used to specify the number of threads.
#' "0" represents that all of the available threads are used;
#' "1" represents one thread is used;
#' "2" represents two threads are used,and so on. Default is 1.
#' @param xmx The maximum Java heap size. The unit is "G".
#' @export
#' @examples
#' ## Step 1. Variation-associated database construction
#' vcf        <- system.file("extdata/sapFinder_test.vcf",
#'                         package="sapFinder")
#' annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
#'                         package="sapFinder")
#' refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
#'                         package="sapFinder")
#' xref       <- system.file("extdata/sapFinder_test_BioMart.Xref.txt",
#'                         package="sapFinder")
#' outdir     <- "db_dir"
#' prefix     <- "sapFinder_test"
#' db.files <- dbCreator(vcf=vcf, annotation=annotation,
#'                 refseq=refseq, outdir=outdir,
#'                 prefix=prefix,xref=xref)
#' 
#' ## Step 2. MS/MS searching
#' mgf.path   <- system.file("extdata/sapFinder_test.mgf",
#'                             package="sapFinder")
#' fasta.path <- db.files[1]
#' xml.path   <- runTandem(spectra=mgf.path, fasta=fasta.path, outdir=".",
#'                         tol=10, tolu="ppm", itol=0.1, itolu="Daltons")
#' 
#' ## Step 3. Post-processing
#' parserGear(file=xml.path, db=fasta.path, prefix=prefix, 
#'             outdir="parser_outdir", alignment=1)
parserGear=function(file=NULL,db=NULL,outdir="parser_outdir",
                    prefix="sapFinder_test",
                    mutPrefix="VAR",decoyPrefix="###REV###",
                    alignment=1,xmx=NULL,thread=1)
{
    
    regx=regexpr("xml$",file,perl=TRUE);
    regd=regexpr("dat$",file,perl=TRUE);
    if(is.null(xmx))
    {
        if(regx[1]!=-1)
        {
            ph<-paste("java","-jar",
                    paste("\"",
                        paste(system.file("parser4sapFinder.jar",
                            package="sapFinder"),sep="",collapse=""),
                    "\"",sep=""),  
                collapse=" ",sep=" ")
            alignment=1;
        }
        else if(regd!=-1)
        {
            ph<-paste("java","-cp",
                    paste("\"",
                        paste(system.file("parser4sapFinder.jar",
                            package="sapFinder"),sep="",collapse=""),
                    "\"",sep=""),
                    "cn.bgi.MascotParser",
                collapse=" ",sep=" ")
            alignment=0;
        }
    }
    else
    {
        if(regx[1]!=-1)
        {
            ph<-paste("java",paste("-Xmx",xmx,"G",sep=""),"-jar",
                    paste("\"",
                        paste(system.file("parser4sapFinder.jar",
                            package="sapFinder"),sep="",collapse=""),
                    "\"",sep=""), 
                collapse=" ",sep=" ");
            alignment=1;
        }
        if(regd[1]!=-1)
        {
            ph<-paste("java",paste("-Xmx",xmx,"G",sep=""),"-cp",
                    paste("\"",
                        paste(system.file("parser4sapFinder.jar",
                            package="sapFinder"),sep="",collapse=""),
                    "\"",sep=""),
                    "cn.bgi.MascotParser",
                collapse=" ",sep=" ");
            alignment=0;
        }
    }
    
    tandemparser=paste(ph,
                        paste("\"",file,"\"",sep=""), 
                        paste("\"",db,"\"",sep=""), 
                        paste("\"",prefix,"\"",sep=""), 
                        paste("\"",outdir,"\"",sep=""), 
                        paste('"',decoyPrefix,'"',sep=""),
                        paste('"',mutPrefix,'"',sep=""),
                        alignment,
                        thread,
                        collapse=" ",sep=" ")
    outfile=system(command=tandemparser,intern=TRUE)
}


#' @title reportCreator
#' @description This function is used for HTML-based report writing
#' @param indir The directory of output files of function \code{parserGear}.
#' @param outdir Output directory for this report 
#' @param db A FASTA format database file used for MS/MS searching. 
#' Usually, it is from the output of the function \code{dbCreator}.
#' @param prefix It must be set the same with the parameter of "prefix" in 
#' function \code{parserGear}.
#' @param varInfor It is a tab-delimited file contains detailed variation 
#' information and is from the output of the function \code{dbCreator}.
#' @export 
#' @examples
#' ## Step 1. Variation-associated database construction
#' vcf        <- system.file("extdata/sapFinder_test.vcf",
#'                         package="sapFinder")
#' annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
#'                         package="sapFinder")
#' refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
#'                         package="sapFinder")
#' xref       <- system.file("extdata/sapFinder_test_BioMart.Xref.txt",
#'                         package="sapFinder")
#' outdir     <- "db_dir"
#' prefix     <- "sapFinder_test"
#' db.files <- dbCreator(vcf=vcf, annotation=annotation,
#'                 refseq=refseq, outdir=outdir,
#'                 prefix=prefix,xref=xref)
#' 
#' ## Step 2. MS/MS searching
#' mgf.path   <- system.file("extdata/sapFinder_test.mgf",
#'                             package="sapFinder")
#' fasta.path <- db.files[1]
#' xml.path   <- runTandem(spectra=mgf.path, fasta=fasta.path, outdir=".",
#'                         tol=10, tolu="ppm", itol=0.1, itolu="Daltons")
#' 
#' ## Step 3. Post-processing
#' parserGear(file=xml.path, db=fasta.path, prefix=prefix, 
#'             outdir="parser_outdir")
#' 
#' ## Step 4. HTML-based report generation
#' reportCreator(indir="parser_outdir", outdir="report", db=fasta.path, 
#'             prefix=prefix, varInfor=db.files[2])
reportCreator=function(indir=".", outdir=.REPORT.DIR, db=NULL,
                    prefix=NULL, varInfor=NULL){
    varInfor <- normalizePath(varInfor)
    db <- normalizePath(db)
    indir <- normalizePath(indir)
    pep.summary.path <- paste(indir, "/", prefix,"-peptideSummary.txt", sep="")
    pro.summary.path <- paste(indir, "/", prefix,"-proteinSummary.txt", sep="")
    
    ## check whether the file is empty
    con <- file(pep.summary.path,"r")
    twoline <- readLines(con,n=2)
    close(con)
    
    if(is.na(twoline[2])){
        stop("the peptideSummary is null!\n")
    }
    
    if(!file.exists(outdir)){
        dir.create(outdir)
    }
    
    #setwd(outdir)
    
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    if(!file.exists(page_dir)){
        dir.create(page_dir);
        cssdir <- paste(system.file(package="sapFinder"), "css", sep="/")
        imdir  <- paste(system.file(package="sapFinder"), "images", sep="/")
        jsdir  <- paste(system.file(package="sapFinder"), "js", sep="/")
        file.copy(c(cssdir,imdir,jsdir),page_dir,recursive = TRUE);
    }
    
    data_dir=paste(outdir,.DATA.DIR,sep="/");
    if(!file.exists(data_dir)){
        dir.create(data_dir)
        file.copy(c(pep.summary.path, pro.summary.path), data_dir)
    }
    
    cat("    Step 1: Reading the Info.\n");
    dobj<-.dataHandle_R(pep.summary.path, varInfor, db);
    gc()  # call garbage collection
    #save(dobj,file=paste("dataHandle.out.RData", sep = "")) #debug
    if(length(dobj)==1){
        stop("None variant peptide is identified!\n");
    }
    cat("    Step 2: Spectrum plotting.\n");
    .spplot(dobj,indir,outdir); #draw spectral
    cat("    Step 3: Creating the html pages.\n");
    wmlist=list();
    .rihtml(0,outdir);
    .rihtml(1,outdir);
    .rihtml(2,outdir);
    .rihtml(3,outdir);
    .rdhtml(varInfor,outdir);
    .rghtml(data=dobj,outdir=outdir);
    .rhhtml(outdir);
    wmlist<-.rchtml(data=dobj,wmlist=wmlist,outdir=outdir);
    .rshtml(data=dobj, pep.summary.path=pep.summary.path, 
            pro.summary.path=pro.summary.path,
            wmlist=wmlist,outdir=outdir);
}

#' @title easyRun
#' @description This function is used to automate the variation-associated
#' database construction,
#' MS/MS searching, post-processing and HTML-based report generation.
#' @param vcf Input VCF file name. This file contains the information of
#' gene sequence variations.
#' @param annotation  Input annotation file name. 
#' It contains the gene annotation information and can be downloaded from
#' UCSC Genome Browser.
#' Currently it supports RefSeq genes and ENSEMBL genes annotation file.
#' @param refseq Input mRNA sequences file with FASTA format. It can be
#' downloaded from UCSC Genome Browser.
#' @param outdir Output directory. 
#' @param prefix The prefix of output file.
#' @param spectra MS/MS peak list file
#' @param cpu The number of CPU used for X!Tandem search. Default is 1.
#' @param enzyme Specification of specific protein cleavage sites. 
#' Default is "[KR]|[X]".
#' @param varmod Specificiation of potential modifications of residues.
#' @param fixmod Specification of modifications of residues.
#' @param tol Parent ion mass tolerance (monoisotopic mass).
#' @param tolu Parent ion M+H mass tolerance window units.
#' @param itol Fragment ion mass tolerance (monoisotopic mass).
#' @param itolu Unit for fragment ion mass tolerance (monoisotopic mass).
#' @param miss The number of missed cleavage sites. Default is 2.
#' @param maxCharge The Maximum parent charge, default is 8
#' @param ti anticipate carbon isotope parent ion assignment errors.
#' Default is false.
#' @param alignment 0 or 1 to determine if peptide should be alignment or not.
#' Default is 0.
#' @param xmx The maximum Java heap size. The unit is "G".
#' @param xref Optional external cross-reference file,generally it's downloaded 
#' through BioMart.If this file is provided,the final html report will present
#' some relevant protein id or description.
#' @param ... Additional arguments
#' @export
#' @examples
#' vcf        <- system.file("extdata/sapFinder_test.vcf",
#'                             package="sapFinder")
#' annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
#'                             package="sapFinder")
#' refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
#'                             package="sapFinder")
#' mgf.path   <- system.file("extdata/sapFinder_test.mgf",
#'                             package="sapFinder")
#' xref       <- system.file("extdata/sapFinder_test_BioMart.Xref.txt",
#'                         package="sapFinder")
#' easyRun(vcf=vcf,annotation=annotation,refseq=refseq,outdir="test",
#' prefix="sapFinder_test",spectra=mgf.path,cpu=0,tol=10, tolu="ppm", itol=0.1,
#' itolu="Daltons",alignment=1,xref=xref)
easyRun=function(vcf=NULL, annotation=NULL, refseq=NULL, outdir="./", 
                prefix="sapFinder_test",spectra="",cpu=1, enzyme="[KR]|[X]",
                tol=10,tolu="ppm",itol=0.6,itolu="Daltons",
                varmod=NULL,fixmod=NULL,miss=2,maxCharge=8,ti=FALSE,
                alignment=1, xref="noxref", xmx=NULL, ...){
    ## Stage 1. Variation-associated database construction
    dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
    outdir=normalizePath(outdir)
    cat("Stage 1. Variation-associated database construction.\n")
    db.files   <- dbCreator(vcf, annotation, refseq, outdir=outdir,
                            prefix=prefix, xmx=xmx, xref=xref)
    
    ## Stage 2. MS/MS searching
    cat("Stage 2. MS/MS searching.\n")
    xml.path   <- runTandem(spectra=spectra, fasta=db.files[1], outdir=outdir,
                            varmod=varmod, fixmod=fixmod, maxCharge=maxCharge,
                            enzyme=enzyme, cpu=cpu, ti=ti,tol=tol,
                            tolu=tolu, itol=itol, itolu=itolu,miss=miss)
    
    xml.path<-basename(xml.path)
    fasta.path<-basename(db.files[1])
    mapfile.path<-basename(db.files[2])
    ## Step 3. Post-processing
    cat("Stage 3. Post-processing.\n")
    parser_outdir=paste(outdir,"parser_outdir",sep="/")
    parserGear(file=paste(outdir,xml.path,sep="/"), 
            db=paste(outdir,fasta.path,sep="/"), prefix=prefix, 
            outdir=parser_outdir, alignment=alignment, xmx=xmx)
    
    ## Step 4. HTML-based report generation
    cat("Stage 4. HTML-based report generation.\n")
    report_dir=paste(outdir,"report",sep="/");
    reportCreator(indir=parser_outdir,outdir=report_dir, 
            db=paste(outdir,fasta.path,sep="/"), 
            prefix=prefix, varInfor=paste(outdir,mapfile.path,sep="/"))
}



# @title .dataHandle_R
# @description Prepare information for HTML-based report generation. 
# It is called inside of function \code{reportCreator}.
# @param file The peptide summary file from \code{parserGear}, 
# such as sapr-peptideSummary.txt.
# @param db A FASTA format database file used for MS/MS searching. 
# Usually, it is from the output of the function \code{dbCreator}.
# @param varInfor It is a tab-delimited file contains detailed variation
# information and is from the output of the function \code{dbCreator}.
# @return List object
.dataHandle_R=function(file=NULL, varInfor=NULL, db=NULL){
    result<-.Call('dataHandle_Cpp', PACKAGE = 'sapFinder', file, varInfor, db)
    return(result);
}
