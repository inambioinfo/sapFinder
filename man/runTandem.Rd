\name{runTandem}
\alias{runTandem}
\title{run xtandem}
\usage{
runTandem(spectra = "", fasta = "", outdir = ".", cpu = 1,
    enzyme = "[KR]|[X]", tol = 10, tolu = "ppm", itol = 0.6,
    itolu = "Daltons", varmod = NULL, fixmod = NULL, miss = 2,
    maxCharge = 8, ti = FALSE)
}
\arguments{
    \item{spectra}{MS/MS peak list file}

    \item{fasta}{Protein database file for searching.}

    \item{outdir}{The output directory.}

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
}
\value{
The search result file path
}
\description{
run xtandem
}
\examples{
# Variation-associated database construction
vcf        <- system.file("extdata/sapFinder_test.vcf",
                            package="sapFinder")
annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
                            package="sapFinder")
refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
                            package="sapFinder")
outdir     <- "db_dir"
prefix     <- "sapFinder_test"
db.files <- dbCreator(vcf=vcf, annotation=annotation,
                    refseq=refseq, outdir=outdir,
                    prefix=prefix)

# MS/MS searching
mgf.path   <- system.file("extdata/sapFinder_test.mgf",
                            package="sapFinder")
runTandem(spectra=mgf.path,fasta=db.files[1],
        tol=10,tolu="ppm",itol=0.1,itolu="Daltons")
}

