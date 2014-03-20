#' run xtandem
#'
#' @description run xtandem
#'
#' @param spectra MS/MS peak list file
#' @param fasta Protein database file for searching.
#' @param outdir The output directory.
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
#' @return The search result file path
#' @export
#' @examples
#' # Variation-associated database construction
#' vcf        <- system.file("extdata/sapFinder_test.vcf",
#'                             package="sapFinder")
#' annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
#'                             package="sapFinder")
#' refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
#'                             package="sapFinder")
#' outdir     <- "db_dir"
#' prefix     <- "sapFinder_test"
#' db.files <- dbCreator(vcf=vcf, annotation=annotation,
#'                     refseq=refseq, outdir=outdir,
#'                     prefix=prefix)
#' 
#' # MS/MS searching
#' mgf.path   <- system.file("extdata/sapFinder_test.mgf",
#'                             package="sapFinder")
#' runTandem(spectra=mgf.path,fasta=db.files[1],
#'         tol=10,tolu="ppm",itol=0.1,itolu="Daltons")
runTandem<-function(spectra="",fasta="",outdir=".",cpu=1,
                    enzyme="[KR]|[X]",tol=10,tolu="ppm",itol=0.6,
                    itolu="Daltons",varmod=NULL,fixmod=NULL,
                    miss=2,maxCharge=8,ti=FALSE)
{
    cat(format(Sys.time()),"\n")
    cleavageSite = enzyme # none enzyme
    if(is.null(varmod))
    {
        varmod="15.994915@M"
    }
    if(is.null(fixmod))
    {
        fixmod="57.021464@C"
    }
    
    if(tolu!="ppm"){
        tolu="Daltons"
    }
    if(itolu!="ppm"){
        itolu="Daltons"
    }
    
    if(ti){
        ti="yes"
    }else{
        ti="no"
    }
    
    taxonomy=rTTaxo(taxon="sapfasta",format="peptide",URL=fasta)
    outxmlname=paste(outdir,"/",basename(spectra),"_xtandem.xml",
                    collapse="",sep="")
    param <- rTParam()
    
    param <- setParamValue(param, 'list path','taxonomy information',taxonomy)
    param <- setParamValue(param, 'protein', 'taxon', value="sapfasta")
    param <- setParamValue(param, 'list path', 'default parameters',
        value=system.file("extdata/default_input.xml",package="rTANDEM"))
    param <- setParamValue(param, 'spectrum', 'path',value=spectra)
    param <- setParamValue(param, 'output', 'xsl path',
        value=system.file("extdata/tandem-input-style.xsl",package="rTANDEM"))
    param <- setParamValue(param, 'output', 'path',value=outxmlname)
    param <- setParamValue(param, 'output', 'maximum valid expectation value',
        value=0.2)
    param <- setParamValue(param, 'output', 'parameters',value="yes")
    param <- setParamValue(param, 'output', 'results',value="valid")
    param <- setParamValue(param, 'output', 'path hashing',value="no")
    param <- setParamValue(param,"spectrum","fragment monoisotopic mass error",
        value=itol)
    ##The value for this parameter may be 'Daltons' or 'ppm'
    param <- setParamValue(param,
        "spectrum","fragment monoisotopic mass error units",value=itolu)
    param <- setParamValue(param,
        "spectrum","parent monoisotopic mass error plus",value=tol)
    param <- setParamValue(param,
        "spectrum","parent monoisotopic mass error minus",value=tol)
    param <- setParamValue(param,
        "spectrum","parent monoisotopic mass error units",value=tolu)
    #param <-setParamValue(param,"spectrum","use noise suppression",value="no")
    #param <-setParamValue(param,"spectrum","minimum parent m+h",  value=1)
    #param <-setParamValue(param,"spectrum","minimum peaks",  value=1)
    param <- setParamValue(param,"spectrum","maximum parent charge",
        value=maxCharge)
    param <- setParamValue(param,
        "spectrum","parent monoisotopic mass isotope error",value=ti)
    param <- setParamValue(param,"refine",value="no")
    param <- setParamValue(param,"refine","cleavage semi",
        value="no")
    param <- setParamValue(param,"refine","unanticipated cleavage",
        value="no")
    param <- setParamValue(param,"scoring","include reverse",value="no")
    param <- setParamValue(param,"scoring","maximum missed cleavage sites",
        value=miss)
    #param <- setParamValue(param,"scoring","minimum ion count",value=1)
    param <- setParamValue(param,"spectrum","threads",value=cpu)
    #param <- setParamValue(param,"spectrum","use conditioning",value=100)
    param <- setParamValue(param,"protein","cleavage site",value=cleavageSite)
    param <- setParamValue(param,"residue","potential modification mass",
        value=varmod)
    param <- setParamValue(param,"residue","modification mass",value=fixmod)
    result.path <- tandem(param)
    
    return(result.path)  
}
