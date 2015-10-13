# .writeHtml_c
#
# write the html;
#
# @author xsh
.writeHtml_c<-function(data,wmlist,outdir,filename="myTest")
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    
    filename=.concat(page_dir,"/", filename, ".html" );
    file=file(filename,"w");
    
    cat('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"',
        ' "http://www.w3.org/TR/html4/loose.dtd">\n', file=file, sep="");
    .write(.tag("html",other='xmlns="http://www.w3.org/1999/xhtml"'),file);
    .writeHead_c(file);
    wmlist<-.writeBody_c(data=data,wmlist=wmlist,file);
    .write(.tag("/html"),file);
    
    close(file);
    return(wmlist)
}
# .writeHead_c
#
# write the header partion;
#
# @author xsh

.writeHead_c<-function(file)
{
    .write(.tag("head"),file);
    meta<-'http-equiv="Content-Type" content="text/html; charset=utf-8"';
    .write(.ctag("meta",other=meta),file);
    .write(.tag("title"),"MutShow",.tag("/title"),file);
    .write(.tag("script",other=paste('type=\"text/javascript\" src=','"',
            .JAVASCRIPT.DIR,"/",.JQUERY.MIN.JS,'"',sep="")),nobreak=TRUE,file);
    .write( "</script>", file );
    .write(.tag("script",other=paste('type=\"text/javascript\" src=','"',
            .JAVASCRIPT.DIR,"/",.QTIP.MIN.JS,'"',sep="")),nobreak=TRUE,file);
    .write( "</script>", file );
    .write(.tag("script",other=paste('type=\"text/javascript\" src=','"',
            .JAVASCRIPT.DIR,"/",.CONTENT.JS,'"',sep="")),nobreak=TRUE,file);
    .write( "</script>", file );
    .write(.tag("link",other=paste('rel="stylesheet" type="text/css" href=',
            '"',.CSS.DIR,"/",.QTIP.MIN.CSS,'"',sep="")),nobreak=TRUE, file);
    .write( "</link>", file );
    .write(.tag("link",other=paste('rel="stylesheet" type="text/css" href=',
            '"',.CSS.DIR,"/",.CONTENT.CSS,'"',sep="")),nobreak=TRUE, file);
    .write( "</link>", file );
    .write(.tag("/head"),file);
}
# .writeBody_c
#
# write the body partion;
#
# @author xsh

.writeBody_c<-function(data,wmlist,file)
{
    
    .write(.tag("body"),file);
    .write(.tag("div",class="container"),file);
    .write(.tag("div",class="content"),file);
    #===========Description=====
    .write(.tag("div",class="sectionheader",
            other='href="javascript:;" target="_self"'),file);
    .write(.tag("span",class="button contenttoggle",
            other='title="Click to collapse"'),"-",
            .tag("/span"),"Description",file);
    .write(.tag("/div"),file);
    .write(.tag("div",class="sectionbody",
            other='style="display: block;"'),file);
    
    .write(.tag("div"),.tag("table",class="stat"),file);
    .write(.tag("tbody"),file);
    
    .write(.tag("tr"),file);
    .write(.tag("th"),"Transcript ID :",.tag("/th"),file);
    .write(.tag("td"),data[["TRAN"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Gene Name :",.tag("/th"),file);
    .write(.tag("td"),data[["GENE"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Chromosome :",.tag("/th"),file);
    .write(.tag("td"),data[["CHR"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    
    if(!is.null(data[["PROT"]]))
    {
        .write(.tag("tr"),file);
        .write(.tag("th"),"Protein ID :",.tag("/th"),file);
        .write(.tag("td"),data[["PROT"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
    }
    if(!is.null(data[["SP"]]))
    {
        .write(.tag("tr"),file);
        .write(.tag("th"),"SwissProt :",.tag("/th"),file);
        .write(.tag("td"),data[["SP"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
    }
    if(!is.null(data[["DESC"]]))
    {
        .write(.tag("tr"),file);
        .write(.tag("th"),"Description :",.tag("/th"),file);
        .write(.tag("td"),data[["DESC"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
    }
    
    .write(.tag("/tbody"),file);
    .write(.tag("/table"),.tag("/div"),file);
    
    .write(.tag("/div"),file);
    #=============Sequence======
    .write(.tag("div",class="sectionheader",
            other='href="javascript:;" target="_self"'),file);
    .write(.tag("span",class="button contenttoggle",
            other='title="Click to collapse"'),"-",
            .tag("/span"),"Sequence",file);
    .write(.tag("/div"),file);
    .write(.tag("div",class="sectionbody",
            other='style="display: block;"'),file);
    .write(.tag("p",class="scale"),
            paste(1,20,40,60,sep=paste(rep(".",18),collapse="")),
            .tag("/p"),file);
    .write(.tag("p",class="scale"),
            paste("|","|","-|","-|",sep=paste(rep("-",18),collapse="")),	
            .tag("/p"),file);
    
    
    imut_dict=list(); 
    for(m in 2:length(data[["IMUT"]]))
    {
        elem=unlist(strsplit(names(data[["IMUT"]][m]),"\t"));
        imut_dict[[elem[1]]][["wild"]]=elem[2];
        imut_dict[[elem[1]]][["mut"]]=elem[3];
        imut_dict[[elem[1]]][["type"]]=elem[4];
        imut_dict[[elem[1]]][["id"]]=elem[5];
        imut_dict[[elem[1]]][["pos"]]=elem[6]; 
        imut_dict[[elem[1]]][["na_change"]]=elem[7];  
        imut_dict[[elem[1]]][["table"]]=data[["IMUT"]][[m]]; 
    }
    
    umut_dict=list();
    for(m in 2:length(data[["UMUT"]]))
    {
        elem=unlist(strsplit(data[["UMUT"]][[m]],"\t")); 
        if(!is.null(umut_dict[[elem[1]]]))
        {
            umut_dict[[elem[1]]][["mut"]]=
                paste(umut_dict[[elem[1]]][["mut"]],elem[3],sep=" | ");
            umut_dict[[elem[1]]][["type"]]=
                paste(umut_dict[[elem[1]]][["type"]],elem[4],sep=" | ");
            umut_dict[[elem[1]]][["id"]]=
                paste(umut_dict[[elem[1]]][["id"]],elem[5],sep="|"); 
            umut_dict[[elem[1]]][["pos"]]=
                paste(umut_dict[[elem[1]]][["pos"]],elem[6],sep=" | ");
            umut_dict[[elem[1]]][["na_change"]]=
                paste(umut_dict[[elem[1]]][["na_change"]],elem[7],sep=" | ");
        }
        else
        {
            umut_dict[[elem[1]]][["mut"]]=elem[3];
            umut_dict[[elem[1]]][["type"]]=elem[4];
            umut_dict[[elem[1]]][["id"]]=elem[5];
            umut_dict[[elem[1]]][["pos"]]=elem[6];
            umut_dict[[elem[1]]][["na_change"]]=elem[7];
        }
        umut_dict[[elem[1]]][["wild"]]=elem[2]; 
    }
    
    
    range_wm_set=c(); 
    range_w_set=c(); 
    elem_w=unique(unlist(strsplit(data[["RANGE_W"]],";")));
    elem_wm<-c(elem_w,unique(unlist(strsplit(data[["RANGE_M"]],";")))) 
    for(m in elem_wm)
    {
        se=strsplit(m,":")[[1]]; 
        range_wm_set=c(range_wm_set,se[1]:se[2]) 
    }
    for(m in elem_w)
    {
        se=strsplit(m,":")[[1]]; 
        range_w_set=c(range_w_set,se[1]:se[2]) 
    }
    
    AAs=unlist(strsplit(data[["SEQ"]],""));
    len_seq=length(AAs);
    inter=len_seq%/%.AA.NUM.EVERY.LINE 
    remain=len_seq%%.AA.NUM.EVERY.LINE 
    for(i in 0:inter)
    {
        if(i==inter)
        {
            if(remain!=0)
            {
                start=i*.AA.NUM.EVERY.LINE+1;
                end=len_seq;
                wmlist<-.writeSeq_c(pro_id=data[["TRAN"]],
                                    start=start,
                                    end=end,
                                    imut_dict=imut_dict,
                                    umut_dict=umut_dict,
                                    range_wm_set=range_wm_set,
                                    range_w_set=range_w_set,
                                    AAs=AAs,
                                    wmlist=wmlist,file=file);
            }
        }
        else
        {
            start=i*.AA.NUM.EVERY.LINE+1;
            end=(i+1)*.AA.NUM.EVERY.LINE;
            wmlist<-.writeSeq_c(pro_id=data[["TRAN"]],
                                start=start,
                                end=end,
                                imut_dict=imut_dict,
                                umut_dict=umut_dict,
                                range_wm_set=range_wm_set,
                                range_w_set=range_w_set,
                                AAs=AAs,
                                wmlist=wmlist,
                                file=file);
        }
    }
    .write(.tag("/div"),file); #------------.sectionbody end--------------->
    #===========Summary================
    .write(.tag("div",class="sectionheader",
            other='href="javascript:;" target="_self"'),file);
    .write(.tag("span",class="button contenttoggle",
            other='title="Click to collapse"'),"+",.tag("/span"),
            "Peptide Summary Report",file);
    .write(.tag("/div"),file);
    .write(.tag("div",class="sectionbody",
            other='style="display: none;"'),file);
    
    .write(.tag("table",id="table-design"),file);
    .writeTable("Index","Charge","Mh","Delta_ppm","isSAP","miss","RT",
            "Evalue","Qvalue","Peptide","Modification",tg1="thead",
            tg2="th",out=file);
    .write(.tag("tbody"),file);
    for(m in 2:length(data[["ALLTABLE"]]))
    {
        elem=unlist(strsplit(data[["ALLTABLE"]][[m]],"\t"));
        .writeTable(elem,out=file);
    }
    .write(.tag("/tbody"),file);
    .write(.tag("/table"),file);
    
    .write(.tag("/div"),file); #--------------.sectionbody end--------------->
    .write(.tag("div",class="top_btn"),file);  #-----------.top_btn---------->
    .write(.tag("/div"),file);
    .write(.tag("/div"),file); #--------------.content end---------------->
    .write(.tag("/div"),file); #--------------.container end---------------->
    
    .write(.tag("/body"),file);
    return(wmlist)
}

# .writeSeq_c
#     
# write the Sequence partion
#
# @author xsh

.writeSeq_c<-function(pro_id,start,end,imut_dict,umut_dict,
                    range_wm_set,range_w_set,AAs,wmlist,file)
{
    im_set=c()    
    um_set=c()
    
    .write(.tag("p",class="sequence"),nobreak=TRUE,file);
    
    for(j in start:end)
    {
        j_c=as.character(j);    
        if(!is.null(imut_dict[[j_c]]))    
        {
            .write(.tag("span",class="imut",
                id=paste("M",j_c,sep=""),other=paste('title="',
                imut_dict[[j_c]][["mut"]],'"',sep="")),
                AAs[j],.tag("/span"),nobreak=TRUE,file);
            im_set=c(im_set,j);
            if(is.element(j,range_w_set)) 
            {
                wmlist[[pro_id]][[j_c]]=1; 
            }
        }
        else if(!is.null(umut_dict[[j_c]])) 
        {
            .write(.tag("span",class="umut",id=paste("U",j_c,sep=""),
                other=paste('title="',umut_dict[[j_c]][["mut"]],
                '"',sep="")),AAs[j],.tag("/span"),nobreak=TRUE,file);
            um_set=c(um_set,j);
        }
        else if(is.element(j,range_wm_set)) 
        {
            .write(.tag("span",class="wild"),AAs[j],.tag("/span"),
                nobreak=TRUE,file);
        }
        else
        {
            .write(AAs[j],nobreak=TRUE,file);
        }
    }
    .write(.tag("/p"),file);
    #========writing imut's panel===========
    for (j in im_set)
    {
        j_c=as.character(j);
        #-------writing info---------
        .write(.tag("div",class="panel",id=paste("M",j_c,sep="")),file);
        
        .write(.tag("div"),.tag("table",class="stat"),file);
        .write(.tag("div",class="panelheader"),
            "Mutation Info",.tag("/div"),file);
        .write(.tag("tbody"),file);
        
        .write(.tag("tr"),file);
        .write(.tag("th"),"AA change:",.tag("/th"),file);
        .write(.tag("td"),paste(imut_dict[[j_c]][["wild"]]," ",j_c," => ",
                    imut_dict[[j_c]][["mut"]],sep=""),.tag("/td"),file);
        .write(.tag("/tr"),file);
        .write(.tag("tr"),file);
        .write(.tag("th"),"NA change:",.tag("/th"),file);
        .write(.tag("td"),imut_dict[[j_c]][["na_change"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
        .write(.tag("tr"),file);
        .write(.tag("th"),"Chr position:",.tag("/th"),file);
        .write(.tag("td"),imut_dict[[j_c]][["pos"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
        .write(.tag("tr"),file);
        .write(.tag("th"),"Type:",.tag("/th"),file);
        .write(.tag("td"),imut_dict[[j_c]][["type"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
        
        
        db_link<-.db_link_switch(imut_dict[[j_c]][["id"]])
        
        #ids<-unlist(strsplit(imut_dict[[j_c]][["id"]],"[|]"))
        #m1<-gregexpr("\\d+$",ids)  
        #ids_num<-unlist(regmatches(ids, m1))
        #m2<-gregexpr("^[A-Za-z]+",ids)  
        #ids_prefix<-unlist(regmatches(ids, m2))
        #if( ids=="NULL" || (typeof(ids_num)=="character" && 
        #length(ids_num)==0) || (typeof(ids_prefix)=="character" &&
        #length(ids_prefix)==0))
        #{
        #    db_link<-paste( paste(.tag("a",
        #other=paste('href="javascript:void(0);"',sep="")),
        #ids,.tag("/a"),sep=""),collapse="|")
        #}
        #else if(ids_prefix=="COSM")
        #{
        #  db_link<-paste( paste(.tag("a",other=
        #paste('href="htftp://cancer.sanger.ac.uk/cosmic/mutation/overview?id='
        #,ids_num,'"',sep="")),ids,.tag("/a"),sep=""),collapse="|")
        #}
        #else if(ids_prefix=="rs")
        #{
        #  db_link<-paste( paste(.tag("a",other=paste(
        #'href="http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=',
        #ids_num,'"',sep="")),ids,.tag("/a"),sep=""),collapse="|") 
        #}
        
        .write(.tag("tr"),file);
        .write(.tag("th"),"DB Info Link:",.tag("/th"),file);
        .write(.tag("td"),db_link,.tag("/td"),file);
        .write(.tag("/tr"),file);
        
        .write(.tag("/tbody"),file);
        .write(.tag("/table"),.tag("/div"),file);
        
        #-------writing summary---------
        .write(.tag("div",class="panelheader"),"MSMS Summary",
                .tag("/div"),file);
        .write(.tag("table",id="table-design"),file);
        .writeTable("Spectrum","Index","Charge","Mh","Delta_ppm","isSAP",
                    "miss","RT","Evalue","Qvalue","Peptide","Modification",
                    "AA_wild","AA_mut",tg1="thead",tg2="th",out=file);
        .write(.tag("tbody"),file);
        table_line=unlist(strsplit(imut_dict[[j_c]][["table"]],"\n"));
        for(l in table_line)
        {
            elem=unlist(strsplit(l,"\t"));
			elem[10]<-.mut_highlight(elem[10],elem[14])	
			elem <- elem[ 1:length(elem)-1 ]	
            link_tag=.ctag("img",
                other=paste('tipsrc="spectral/',elem[1],'.png" alt="',
                elem[1],'" src="images/spectral_thumb.gif"',sep="")); 
            .writeTable(link_tag,elem,out=file);
        }
        
        .write(.tag("/tbody"),file);
        .write(.tag("/table"),file);
        
        
        .write(.tag("/div"),file);#-------------.panel end---------------->
    }
    #========writing umut's panel===========
    for (j in um_set)
    {
        j_c=as.character(j);
        #-------writing info---------
        .write(.tag("div",class="panel",id=paste("U",j_c,sep="")),file);
        
        
        .write(.tag("div"),.tag("table",class="stat"),file);
        .write(.tag("div",class="panelheader"),"Mutation Info",
            .tag("/div"),file);
        .write(.tag("tbody"),file);
        
        .write(.tag("tr"),file);
        .write(.tag("th"),"AA change:",.tag("/th"),file);
        .write(.tag("td"),
            paste(umut_dict[[j_c]][["wild"]]," ",j_c," => ",
                umut_dict[[j_c]][["mut"]],sep=""),.tag("/td"),file);
        .write(.tag("/tr"),file);
        .write(.tag("tr"),file);
        .write(.tag("th"),"NA change:",.tag("/th"),file);
        .write(.tag("td"),umut_dict[[j_c]][["na_change"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
        .write(.tag("tr"),file);
        .write(.tag("th"),"Chr position:",.tag("/th"),file);
        .write(.tag("td"),umut_dict[[j_c]][["pos"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
        .write(.tag("tr"),file);
        .write(.tag("th"),"Type:",.tag("/th"),file);
        .write(.tag("td"),umut_dict[[j_c]][["type"]],.tag("/td"),file);
        .write(.tag("/tr"),file);
        
        db_link<-.db_link_switch(umut_dict[[j_c]][["id"]])
        
        .write(.tag("tr"),file);
        .write(.tag("th"),"DB Info Link:",.tag("/th"),file);
        .write(.tag("td"),db_link,.tag("/td"),file);
        .write(.tag("/tr"),file);
        
        .write(.tag("/tbody"),file);
        .write(.tag("/table"),.tag("/div"),file);
        
        .write(.tag("/div"),file);#--------------.panel end---------------->
    }
    return(wmlist)
}
# .db_link_switch
#     
# dbSNP or COSMIC id website link swither.
#
# @author xsh

.db_link_switch<-function(ids_raw)
{
    
    ids<-unlist(strsplit(ids_raw,"[|]"))
    m1<-gregexpr("\\d+$",ids)  
    ids_num<-unlist(regmatches(ids, m1))
    m2<-gregexpr("^[A-Za-z]+",ids)  
    ids_prefix<-unlist(regmatches(ids, m2))
    
    links<-c();
    for (i in 1:length(ids))
    {
        if(ids[i]=="NULL" ||
            (typeof(ids_num[i])=="character" && length(ids_num[i])==0) ||
            (typeof(ids_prefix[i])=="character"&&length(ids_prefix[i])==0))
        {
            links[i]<-paste(.tag("a",
                other=paste('href="javascript:void(0);"',sep="")),
                ids[i],.tag("/a"),sep="");
        }
        else if(ids_prefix[i]=="COSM")
        {
            h='href="htftp://cancer.sanger.ac.uk/cosmic/mutation/overview?id=';
            links[i]<-paste(.tag("a",other=paste(h,ids_num[i],'"',sep="")),
                            ids[i],.tag("/a"),sep="")
        }
        else if(ids_prefix[i]=="rs")
        {
            h='href="http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=';
            links[i]<-paste(.tag("a",other=paste(h,ids_num[i],'"',sep="")),
                            ids[i],.tag("/a"),sep="")
        }
    }
    db_link<-paste(links,collapse="|")
    return(db_link)
}

#############################main###############################
# .rchtml
# 
# Creates the html page of SAP identification infomation;
#
# @param data List object from .dataHandle_R
# @param wmlist \code{NULL} list object.
# @return List object.
.rchtml<-function(data,wmlist,outdir)
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
    
    for(p in 2:length(data)) 
    {
        wmlist<-.writeHtml_c(data=data[[p]],
                            wmlist=wmlist,
                            outdir=outdir,
                            filename=names(data[p])); 
        #print(data[[p]],)
    }
    return(wmlist)
}
