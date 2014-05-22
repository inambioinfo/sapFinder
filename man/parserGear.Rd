\name{parserGear}
\alias{parserGear}
\title{parserGear}
\usage{
parserGear(file = NULL, db = NULL, outdir = "parser_outdir",
  prefix = "sapFinder_test", mutPrefix = "VAR", decoyPrefix = "###REV###",
  alignment = 1, xmx = NULL, thread = 1)
}
\arguments{
  \item{file}{MS/MS searching file. Currently, only XML
  format result file of X!Tandem is supported.}

  \item{db}{A FASTA format database file used for MS/MS
  searching. Usually, it is from the output of the function
  \code{dbCreator}.}

  \item{outdir}{Output directory.}

  \item{prefix}{The prefix of output file.}

  \item{mutPrefix}{The prefix of variant peptides ID.
  Default is "VAR". "VAR" is the prefix which used by
  function \code{dbCreator}.}

  \item{decoyPrefix}{The prefix of decoy sequences ID.
  Default is "###REV###". "###REV###" is the prefix which
  used by function \code{dbCreator}.}

  \item{alignment}{0 or 1 to determine if peptide should be
  alignment or not. Default is 1.}

  \item{thread}{This parameter is used to specify the
  number of threads. "0" represents that all of the
  available threads are used; "1" represents one thread is
  used; "2" represents two threads are used,and so on.
  Default is 1.}

  \item{xmx}{The maximum Java heap size. The unit is "G".}
}
\description{
This function is mainly for q-value calculation, protein
inference and variant peptides spectra annotation.
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
            outdir="parser_outdir", alignment=1)
}

