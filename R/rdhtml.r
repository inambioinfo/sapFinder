# .writeHtml_d
#
# write the html;
#
# @author xsh

.writeHtml_d<-function(stat_list,outdir,filename="myTest")
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    filename=.concat(page_dir,"/", filename, ".html" );
    file=file(filename,"w");
    
    cat('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"',
        ' "http://www.w3.org/TR/html4/loose.dtd">\n', file=file, sep="");
    .write(.tag("html"),file);
    .writeHead_d(file);
    .writeBody_d(stat_list,file);
    .write(.tag("/html"),file);
    
    close(file);
}

# .writeHead_d
#
# write the header partion;
#
# @author xsh
.writeHead_d<-function(file)
{
    .write(.tag("head"),file);
    meta<-'http-equiv="Content-Type" content="text/html; charset=utf-8"';
    .write(.ctag("meta",other=meta),file);
    .write(.tag("title"),"stat",.tag("/title"),file);
    .write(.tag("script",other=paste('type=\"text/javascript\" src=','"',
        .JAVASCRIPT.DIR,"/",.JQUERY.MIN.JS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/script"), file );
    .write(.tag("script",other=paste('type=\"text/javascript\" src=','"',
        .JAVASCRIPT.DIR,"/",.INDEX.JS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/script"), file );
    .write(.tag("link",other=paste('rel="stylesheet" type="text/css" href=',
        '"',.CSS.DIR,"/",.INDEX.CSS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/link"), file );
    
    .write(.tag("style",other='type="text/css"'),
        "html{overflow:auto;};body{position:relative;}",.tag("/style"),file);
    
    .write(.tag("/head"),file);
}
# .writeBody_d
#
# write the body partion;
#
# @author xsh

.writeBody_d<-function(stat_list,file)
{
    .write(.tag("body"),file);
    .write(.tag("div",class="seq_cn1"),file);
    
    .write(.tag("h1"),"Summary List",.tag("/h1"),.ctag("br"),file);
    .write(.tag("div"),.tag("table",class="stat"),file);
    .write(.tag("tbody"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Gene number :",.tag("/th"),file);
    .write(.tag("td"),stat_list[["gene_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Protein number :",.tag("/th"),file);
    .write(.tag("td"),stat_list[["pro_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"SAP number :",.tag("/th"),file);
    .write(.tag("td"),stat_list[["mut_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("/tbody"),file);
    .write(.tag("/table"),.tag("/div"),file);
    
    .write(.ctag("br"),.ctag("hr",class="clear-contentunit"),
        .ctag("br"),file); 
    
    .write(.tag("h1"),"Summary Chart",.tag("/h1"),.ctag("br"),file);
    .write(.tag("div",other='style="text-align:center"'),file);
    .write(.tag("h2"),"Mutation frequency matrix",.tag("/h2"),file);
    .write(.ctag("img",other=paste('style="width:900px" src="../',.DATA.DIR,
        '/mutation_frequency_heatmap.png"',sep="")),.ctag("br"),file);
    .write(.tag("/div"),file);
    .write(.ctag("br"),file); 
    
    .write(.ctag("br"),.ctag("hr",class="clear-contentunit"),.ctag("br"),file);
    
    .write(.tag("div",class="top_btn"),.tag("/div"),file); 
    #--------------.top_btn------------>
    .write(.tag("/div"),file);
    .write(.tag("/body"),file);
}

#############################main###############################
# .rdhtml
# 
# Creates the html page of database statistical information; 
#
# @param map.table The SAP map table file,such as COSMIC.v67.MS1.map.tsv.
.rdhtml<-function(map.table,outdir)
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    data_dir=paste(outdir,.DATA.DIR,sep="/");
    if(!file.exists(page_dir))
    {
        dir.create(page_dir);
        cssdir=paste(system.file(package="sapFinder"),"css",sep="/");
        imdir=paste(system.file(package="sapFinder"),"images",sep="/");
        jsdir=paste(system.file(package="sapFinder"),"js",sep="/");
        file.copy(c(cssdir,imdir,jsdir),page_dir,recursive = TRUE);
    }
    if(!file.exists(data_dir))
    {
        dir.create(data_dir);
    }
    
    ###################1.heatmap##########
    png(paste(data_dir,"mutation_frequency_heatmap.png",sep="/"),
        width = 900, height = 900);
    stat_list<-.mut_freq_heatmap(map.table)
    dev.off()
    ###################1.basic stat##########
    .writeHtml_d(stat_list,outdir,filename="db_stat");
}
