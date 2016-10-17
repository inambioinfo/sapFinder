\name{dbCreator}
\alias{dbCreator}
\title{dbCreator}
\usage{
dbCreator(vcf = NULL, annotation = NULL, refseq = NULL, outdir = "./",
  prefix = "test", xmx = NULL, xref = "noxref")
}
\arguments{
  \item{vcf}{Input VCF file name. This file contains the
  information of gene sequence variations.}

  \item{annotation}{Input annotation file name. It contains
  the gene annotation information and can be downloaded
  from UCSC Genome Browser.Currently it supports RefSeq
  genes and ENSEMBL genes annotation file.}

  \item{refseq}{Input mRNA sequences file with FASTA
  format. It can be downloaded from UCSC Genome Browser.}

  \item{outdir}{Output directory.}

  \item{prefix}{The prefix of output file.}

  \item{xmx}{The maximum Java heap size. The unit is "G".}

  \item{xref}{Optional external cross-reference
  file,generally it's downloaded through BioMart.If this
  file is provided,the final html report will present some
  relevant protein id or description.}
}
\value{
A vector containing two file names. One is a FASTA format
file contains the mutated peptides, the normal protein
sequences and their reverse versions, and the other is a
tab-delimited file contains detailed variation information.
}
\description{
An integrated function to generate variation-associated
database based on sample-specific NGS data or public SNV
data.
}
\examples{
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
}

