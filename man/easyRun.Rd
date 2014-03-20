\name{easyRun}
\alias{easyRun}
\title{easyRun}
\usage{
easyRun(vcf = NULL, annotation = NULL, refseq = NULL, outdir = "./",
    prefix = "sapFinder_test", spectra = "", cpu = 1, enzyme = "[KR]|[X]",
    tol = 10, tolu = "ppm", itol = 0.6, itolu = "Daltons",
    varmod = NULL, fixmod = NULL, miss = 2, maxCharge = 8, ti = FALSE,
    alignment = 1, xmx = NULL, ...)
}
\arguments{
    \item{vcf}{Input VCF file name. This file contains the
    information of gene sequence variations.}

    \item{annotation}{Input annotation file name. It contains
    the gene annotation information and can be downloaded
    from UCSC Genome Browser. Currently it supports RefSeq
    genes and ENSEMBL genes annotation file.}

    \item{refseq}{Input mRNA sequences file with FASTA
    format. It can be downloaded from UCSC Genome Browser.}

    \item{outdir}{Output directory.}

    \item{prefix}{The prefix of output file.}

    \item{spectra}{MS/MS peak list file}

    \item{cpu}{The number of CPU used for X!Tandem search.
    Default is 1.}

    \item{enzyme}{Specification of specific protein cleavage
    sites. Default is "[KR]|[X]".}

    \item{varmod}{Specificiation of potential modifications
    of residues.}

    \item{fixmod}{Specification of modifications of
    residues.}

    \item{tol}{Parent ion mass tolerance (monoisotopic
    mass).}

    \item{tolu}{Parent ion M+H mass tolerance window units.}

    \item{itol}{Fragment ion mass tolerance (monoisotopic
    mass).}

    \item{itolu}{Unit for fragment ion mass tolerance
    (monoisotopic mass).}

    \item{miss}{The number of missed cleavage sites. Default
    is 2.}

    \item{maxCharge}{The Maximum parent charge, default is 8}

    \item{ti}{anticipate carbon isotope parent ion assignment
    errors. Default is false.}

    \item{alignment}{0 or 1 to determine if peptide should be
    alignment or not. Default is 0.}

    \item{xmx}{The maximum Java heap size. The unit is "G".}

    \item{...}{Additional arguments}
}
\description{
This function is used to automate the variation-associated
database construction, MS/MS searching, post-processing and
HTML-based report generation.
}
\examples{
vcf        <- system.file("extdata/sapFinder_test.vcf",
                            package="sapFinder")
annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
                            package="sapFinder")
refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
                            package="sapFinder")
mgf.path   <- system.file("extdata/sapFinder_test.mgf",
                            package="sapFinder")
easyRun(vcf=vcf,annotation=annotation,refseq=refseq,outdir="test",
prefix="sapFinder_test",spectra=mgf.path,cpu=0,tol=10, tolu="ppm", itol=0.1,
itolu="Daltons",alignment=1)
}

