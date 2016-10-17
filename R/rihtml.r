# .writeHtml_i
#
# write the html;
#
# @author xsh
.writeHtml_i<-function(mode,outdir,filename="myTest")
{
    filename=.concat(paste(outdir,filename,sep="/"), ".html" );
    file=file(filename,"w");
    
    cat('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"',
        ' "http://www.w3.org/TR/html4/loose.dtd">\n', file=file, sep="");
    .write(.tag("html"),file);
    .writeHead_i(mode,file);
    .writeBody_i(mode,file);
    .write(.tag("/html"),file);
    
    close(file);
}
# .writeHead_i
#
# write the header partion;
#
# @author xsh
.writeHead_i<-function(mode,file)
{
    .write(.tag("head"),file);
    meta<-'http-equiv="Content-Type" content="text/html; charset=utf-8"';
    .write(.ctag("meta",other=meta),file);
    .write(.tag("title"),"index",.tag("/title"),file);
    if(mode==0)
    {
        target_dir=.PAGE.DIR;
    }
    else if (mode>0)
    {
        target_dir=".";
    }
    .write(.tag("script",
        other=paste('type=\"text/javascript\" src=','"',target_dir,"/",
        .JAVASCRIPT.DIR,"/",.JQUERY.MIN.JS,'"',sep="")),nobreak=TRUE, file);
    .write( "</script>", file );
    if(mode==0)
    {
        .write(.tag("script",
            other=paste('type=\"text/javascript\" src=','"',target_dir,"/",
            .JAVASCRIPT.DIR,"/",.INDEX.JS,'"',sep="")),nobreak=TRUE, file);
        .write( "</script>", file );
	}
    else if (mode>0)
    {
        .write(.tag("script",
            other=paste('type=\"text/javascript\" src=','"',target_dir,"/",
            .JAVASCRIPT.DIR,"/","index2.js",'"',sep="")),nobreak=TRUE, file);
        .write( "</script>", file );
    }
    .write(.tag("link",
        other=paste('rel="stylesheet" type="text/css" href=','"',target_dir,
        "/",.CSS.DIR,"/",.INDEX.CSS,'"',sep="")),nobreak=TRUE, file);
    .write( "</link>", file );
    .write(.tag("/head"),file);
}
# .writeBody_i
#
# write the body partion;
#
# @author xsh
.writeBody_i<-function(mode,file)
{
    .write(.tag("body"),file);
    #header
    .write(.tag("div",class="header"),file);
    .write(.tag("div",class="flleft logo_title"),file);
    .write(.tag("div",class="title"),
        "Single Amino Acid Variant Analysis Report",.tag("/div"),file);
    .write(.tag("/div"),file);
    .write(.tag("div",class="head_right"),.tag("/div"),file)
    .write(.tag("/div"),file);
    #navigation
    .write(.tag("div",class="bg_bar nav_bg"),file);
    .write(.tag("ul",class="nav_ul"),file);
    if(mode==0)
    {
        .write(.tag("li"),.tag("a",other='href="index.html"'),
            "SAPs Info",.tag("/a"),.tag("/li"),file);
        .write(.tag("li",class="bg_bar line"),.tag("/li"),file); 
        .write(.tag("li"),
            .tag("a",other=.concat('href="',.PAGE.DIR,'/index_search.html"')),
            "Search Stat",.tag("/a"),.tag("/li"),file);
        .write(.tag("li",class="bg_bar line"),.tag("/li"),file);
        .write(.tag("li"),
            .tag("a",other=.concat('href="',.PAGE.DIR,'/index_db.html"')),
            "Database Stat",.tag("/a"),.tag("/li"),file);
		.write(.tag("li",class="bg_bar line"),.tag("/li"),file);
        .write(.tag("li"),
            .tag("a",other=.concat('href="',.PAGE.DIR,'/index_help.html"')),
            "Help",.tag("/a"),.tag("/li"),file);
        .write(.tag("li",class="bg_bar line"),.tag("/li"),file);
    }
    else if (mode>0)
    {
        .write(.tag("li"),.tag("a",other='href="../index.html"'),"SAPs Info",
            .tag("/a"),.tag("/li"),file);
        .write(.tag("li",class="bg_bar line"),.tag("/li"),file);
        .write(.tag("li"),.tag("a",other='href="index_search.html"'),
            "Search Stat",.tag("/a"),.tag("/li"),file);
        .write(.tag("li",class="bg_bar line"),.tag("/li"),file);
        .write(.tag("li"),.tag("a",other='href="index_db.html"'),
            "Database Stat",.tag("/a"),.tag("/li"),file);
		 .write(.tag("li",class="bg_bar line"),.tag("/li"),file);
        .write(.tag("li"),.tag("a",other='href="index_help.html"'),
            "Help",.tag("/a"),.tag("/li"),file);
        .write(.tag("li",class="bg_bar line"),.tag("/li"),file);
        
    }
    
    #     .write(.tag("li"),.tag("a",other='href="javascript:void(0);"'),
    #            "undefined",.tag("/a"),file);
    #     .write(.tag("ul",class="nav_drop"),file);
    #     .write(.tag("a",other='href="javascript:void(0);"'),.tag("li"),
    #            "undefined",.tag("/li"),.tag("/a"),.tag("li",class="line1"),
    #            .tag("/li"),file);
    #     .write(.tag("a",other='href="javascript:void(0);"'),.tag("li"),
    #            "undefined",.tag("/li"),.tag("/a"),.tag("li",class="line1"),
    #            .tag("/li"),file);
    #     .write(.tag("/ul"),file);
    #     .write(.tag("/li"),file);
    #     .write(.tag("li",class="bg_bar line"),.tag("/li"),file);
    
    .write(.tag("/ul"),file);
    .write(.tag("div",class="seq_top"),.tag("/div"),file);
    .write(.tag("/div"),file);
    #left sidebar
    if(mode==0)
    {
        .write(.tag("iframe",
            id="left",class="seq_contentl",other=paste('src="',.PAGE.DIR,
            '/group_left.html" name="left" frameborder="0"',sep="")),
            .tag("/iframe"),file);
        .write(.tag("div",class="seq_contentc"),.tag("/div"),file); 
        .write(.tag("iframe",id="main",class="seq_cn",
            other=paste('src="',.PAGE.DIR
            ,'/P1.html" name="main" frameborder="0"',
            sep="")),.tag("/iframe"),file);
    }
    else if(mode==1)
    {
        .write(.tag("iframe",id="main",class="seq_cn",other=paste('src="',
            'search_stat.html" name="main" frameborder="0"'
            ,sep="")),.tag("/iframe"),file);
    }
    else if(mode==2)
    {
        .write(.tag("iframe",id="main",class="seq_cn",
            other=paste('src="','db_stat.html" name="main" frameborder="0"'
            ,sep="")),.tag("/iframe"),file);
    }
	else if(mode==3)
	{
		.write(.tag("iframe",id="main",class="seq_cn",
            other=paste('src="','help.html" name="main" frameborder="0"'
            ,sep="")),.tag("/iframe"),file);

	}
    #--foot-->
    #.write(.tag("div",class="seq_foot"),file);
    #.write(.tag("div",class="seq_footl"),file);
    #.write(.tag("span",class="foot_logo"),.tag("/span"),file);
    #.write(.tag("span",class="foot_text"),.tag("/span"),file);
    #.write(.tag("span"),.tag("/span"),file);
    #.write(.tag("/div"),file);
    #.write(.tag("div",class="seq_footr"),file);
    #.write(.tag("span",class="foot_text"),.tag("/span"),file);
    #.write(.tag("span"),.tag("/span"),file);
    #.write(.tag("/div"),file);
    #.write(.tag("/div"),file);
    
    .write(.tag("/body"),file);
}

#############################main###############################
# .rihtml
# 
# create the html index page;
#
# @param mode Optional parameters:{0,1,2}.
# "0" creates main index(sap).
# "1" creates search stat index,
# "2" creates for database stat index.

.rihtml<-function(mode=NA,outdir)
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    if(!file.exists(page_dir))
    {
        dir.create(page_dir);
        
        cssdir=paste(system.file(package="sapFinder"),"css",sep="/");
        imdir=paste(system.file(package="sapFinder"),"images",sep="/");
        jsdir=paste(system.file(package="sapFinder"),"js/",sep="/");
        file.copy(c(cssdir,imdir,jsdir),page_dir,recursive = TRUE);
        
    }
    if(mode==0)
    {
        fn="index";
    }
    else if(mode==1)
    {
        fn=.concat(.PAGE.DIR,"/index_search");
    }
    else if(mode==2)
    {
        fn=.concat(.PAGE.DIR,"/index_db");
    }    
	else if(mode==3)
    {
        fn=.concat(.PAGE.DIR,"/index_help");
    }
    else
    {
        stop("Optional mode value i: 0,1,2;");
    }
    .writeHtml_i(mode,outdir=outdir,filename=fn);
}
