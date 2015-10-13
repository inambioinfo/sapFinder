annotation <- system.file("extdata/sapFinder_test_ensGene.txt",
                        package="sapFinder")
test_AnnotationFileColumnNumber <-function()
{
    con<-file(annotation,"r")
    line<-readLines(con,n=1)
    elems<-unlist(strsplit(line,"\t"))
    checkEquals(length(elems),16)
    close(con)
}
