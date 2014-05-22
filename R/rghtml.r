# .writeHtml_g
#
# write the html;
#
# @author xsh
.writeHtml_g<-function(data,outdir,filename="myLeft")
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    filename=.concat(page_dir,"/", filename, ".html" );
    file=file(filename,"w");
    
    cat( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"',
        ' "http://www.w3.org/TR/html4/loose.dtd">\n', file=file, sep="");
    .write(.tag("html",other='xmlns="http://www.w3.org/1999/xhtml"'),file);
    .writeHead_g(file);
    .writeBody_g(data,file);
    .write(.tag("/html"),file);
    
    close(file);
}
# .writeHead_g
#
# write the header partion;
#
# @author xsh
.writeHead_g<-function(file)
{
    .write(.tag("head"),file);
    meta<-'http-equiv="Content-Type" content="text/html; charset=utf-8"';
    .write(.ctag("meta",other=meta),file);
    .write(.tag("title"),"Left",.tag("/title"),file);
    .write(.tag("script",other=paste('type=\"text/javascript\" src=','"',
        .JAVASCRIPT.DIR,"/",.JQUERY.MIN.JS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/script"), file );
    .write(.tag("script",other=paste('type=\"text/javascript\" src=','"',
        .JAVASCRIPT.DIR,"/",.INDEX.JS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/script"), file );
    .write(.tag("link",other=paste('rel="stylesheet" type="text/css" href=',
        '"',.CSS.DIR,"/",.LEFT.CSS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/link"), file );
    .write(.tag("style",other='type="text/css"'),"html{overflow:auto;}",
        .tag("/style"),file); 
    .write(.tag("/head"),file);
}
# .writeBody_g
#
# write the body partion;
#
# @author xsh

.writeBody_g<-function(data,file)
{
    .write(.tag("body"),file);
    .write(.tag("div",class="seq_ctl_top"),"Index List",.tag("/div"),file);
    .write(.tag("ul",id="seq_ul"),file);
    #.write(.tag("li"),.tag("span"),.tag("/span"),file); 
    for(p in 2:length(data))
    {
        .write(.tag("li"),
            .tag("a",other=paste('href="',names(data[p]),
            '.html" target="main"',sep="")),
            data[[p]][["TRAN"]],.tag("/a"),.tag("/li"),file);
    }
    .write(.tag("/ul"),file);
    
    .write(.tag("div",class="top_btn"),file);  #----------.top_btn---------->
    .write(.tag("/div"),file);
    
    .write(.tag("/body"),file);
}

#############################main###############################
# .rghtml
# 
# Creates the html page of SAP identification infomation;
#
# @param data List object from .dataHandle_R

.rghtml<-function(data,outdir)
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    if(!file.exists(page_dir))
    {
        dir.create(page_dir); 
        cssdir=paste(system.file(package="sapFinder"),"css",sep="/");
        imdir=paste(system.file(package="sapFinder"),"images",sep="/");
        jsdir=paste(system.file(package="sapFinder"),"js",sep="/");
        file.copy(c(cssdir,imdir,jsdir),page_dir,recursive = TRUE);
    }
    .writeHtml_g(data=data,outdir,filename='group_left');
}
