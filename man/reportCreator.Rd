\name{reportCreator}
\alias{reportCreator}
\title{reportCreator}
\usage{
reportCreator(indir = ".", outdir = .REPORT.DIR, db = NULL,
  prefix = NULL, varInfor = NULL)
}
\arguments{
  \item{indir}{The directory of output files of function
  \code{parserGear}.}

  \item{outdir}{Output directory for this report}

  \item{db}{A FASTA format database file used for MS/MS
  searching. Usually, it is from the output of the function
  \code{dbCreator}.}

  \item{prefix}{It must be set the same with the parameter
  of "prefix" in function \code{parserGear}.}

  \item{varInfor}{It is a tab-delimited file contains
  detailed variation information and is from the output of
  the function \code{dbCreator}.}
}
\description{
This function is used for HTML-based report writing
}
\examples{
## Step 1. Variation-associated database construction
vcf        <- system.file("extdata/sapFinder_test.vcf",
                        package="sapFinder")
annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
                        package="sapFinder")
refseq     <- system.file("extdata/sapFinder_test_ensGeneMrna.fa",
                        package="sapFinder")
xref       <- system.file("extdata/sapFinder_test_BioMart.Xref.txt",
                        package="sapFinder")
outdir     <- "db_dir"
prefix     <- "sapFinder_test"
db.files <- dbCreator(vcf=vcf, annotation=annotation,
                refseq=refseq, outdir=outdir,
                prefix=prefix,xref=xref)

## Step 2. MS/MS searching
mgf.path   <- system.file("extdata/sapFinder_test.mgf",
                            package="sapFinder")
fasta.path <- db.files[1]
xml.path   <- runTandem(spectra=mgf.path, fasta=fasta.path, outdir=".",
                        tol=10, tolu="ppm", itol=0.1, itolu="Daltons")

## Step 3. Post-processing
parserGear(file=xml.path, db=fasta.path, prefix=prefix,
            outdir="parser_outdir")

## Step 4. HTML-based report generation
reportCreator(indir="parser_outdir", outdir="report", db=fasta.path,
            prefix=prefix, varInfor=db.files[2])
}

