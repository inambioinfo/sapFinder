# .writeHtml_s
#
# write the html;
#
# @author xsh

.writeHtml_s<-function(data,stat_list,pep.summary.path,
        pro.summary.path,filename="myTest",wmlist,outdir)
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");

    filename=.concat(page_dir,"/", filename, ".html" );
    file=file(filename,"w");

    cat('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"',
        ' "http://www.w3.org/TR/html4/loose.dtd">\n', file=file, sep="");
    .write(.tag("html"),file);
    .writeHead_s(file);
    .writeBody_s(data,stat_list,pep.summary.path,pro.summary.path,
        file,wmlist=wmlist);
    .write(.tag("/html"),file);

    close(file);
}

# .writeHead_i
#
# write the header partion;
#
# @author xsh
.writeHead_s<-function(file)
{
    .write(.tag("head"),file);
    meta<-'http-equiv="Content-Type" content="text/html; charset=utf-8"';
    .write(.ctag("meta",other=meta),file);
    .write(.tag("title"),"stat",.tag("/title"),file);
    .write(.tag("script",
        other=paste('type=\"text/javascript\" src=','"',
        .JAVASCRIPT.DIR,"/",.JQUERY.MIN.JS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/script"), file );
    .write(.tag("script",
        other=paste('type=\"text/javascript\" src=','"',.JAVASCRIPT.DIR,
        "/",.INDEX.JS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/script"), file );
    .write(.tag("link",other=paste('rel="stylesheet" type="text/css" href=',
        '"',.CSS.DIR,"/",.INDEX.CSS,'"',sep="")),nobreak=TRUE, file);
    .write( .tag("/link"), file );
    
    .write(.tag("style",other='type="text/css"'),
        "html{overflow:auto;};body{position:relative;}",.tag("/style"),file);
    
    .write(.tag("/head"),file);
}
# .writeBody_g
#
# write the body partion;
#
# @author xsh

.writeBody_s<-function(data,stat_list,
                    pep.summary.path,
                    pro.summary.path,
                    file,wmlist)
{
    .write(.tag("body"),file);
    .write(.tag("div",class="seq_cn1"),file);
    .write(.tag("a",class="button contenttoggle",
        other=.concat('title="Click to download" href="../',.DATA.DIR,'/',
        basename(pep.summary.path),'"')),"Download peptide summary",
        .tag("/a"),file);
    .write(.tag("a",class="button contenttoggle",
        other=.concat('title="Click to download" href="../',.DATA.DIR,'/',
        basename(pro.summary.path),'"')),"Download protide summary",
        .tag("/a"),file);

    .write(.ctag("br"),.ctag("hr",class="clear-contentunit"),.ctag("br"),file);

    .write(.tag("h1"),"Summary List",.tag("/h1"),.ctag("br"),file);
    .write(.tag("div"),.tag("table",class="stat"),file);
    .write(.tag("tbody"),file);

    .write(.tag("tr"),file);
    .write(.tag("th"),"Number of identified spectra :",.tag("/th"),file);
    .write(.tag("td"),stat_list[["all_sp_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Number of identified peptide :",.tag("/th"),file);
    .write(.tag("td"),stat_list[["all_pep_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Number of identified protein :",.tag("/th"),file);
    .write(.tag("td"),stat_list[["all_pro_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Number of identified spectra with SAP :",
            .tag("/th"),file);
    .write(.tag("td"),stat_list[["mut_sp_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Number of identified peptide with SAP :",
            .tag("/th"),file);
    .write(.tag("td"),stat_list[["mut_pep_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);
    .write(.tag("tr"),file);
    .write(.tag("th"),"Number of identified protein with SAP :",
            .tag("/th"),file);
    .write(.tag("td"),stat_list[["mut_pro_num"]],.tag("/td"),file);
    .write(.tag("/tr"),file);

    .write(.tag("/tbody"),file);
    .write(.tag("/table"),.tag("/div"),file);
    
#     .write(.tag("div",class="stat"),file);
#     .write(.tag("ul"),file);
#     .write(.tag("li"),.tag("b",class="stat_var"),
#            "Identified spectrum numbers :",.tag("/b"),
#            .blank(4),stat_list[["all_sp_num"]],.tag("/li"),file);
#     .write(.tag("li"),.tag("b",class="stat_var"),
#            "Identified peptide numbers :",.tag("/b"),.blank(4),
#            stat_list[["all_pep_num"]],.tag("/li"),file);
#     .write(.tag("li"),.tag("b",class="stat_var"),
#            "Identified protein numbers :",.tag("/b"),
#            .blank(4),stat_list[["all_pro_num"]],.tag("/li"),file);
#     .write(.tag("li"),.tag("b",class="stat_var"),
#            "Identified SAPs' peptide numbers :",.tag("/b"),
#            .blank(4),stat_list[["mut_pep_num"]],.tag("/li"),file);
#     .write(.tag("li"),.tag("b",class="stat_var"),
#            "Identified SAPs' protein numbers :",.tag("/b"),
#            .blank(4),stat_list[["mut_pro_num"]],.tag("/li"),file);
#     .write(.tag("/ul"),file);
#     .write(.tag("/div"),file);
    
    .write(.ctag("br"),.ctag("hr",class="clear-contentunit"),.ctag("br"),file);
    
    .write(.tag("h1"),"Summary Chart",.tag("/h1"),.ctag("br"),file);
    .write(.tag("div",other='style="text-align:center"'),file);
    .write(.tag("h2"),"Precursor error",.tag("/h2"),file);
    .write(.ctag("img",other=paste('style="width:520px" src="../',
        .DATA.DIR,'/precursor_ms1_error.png"',sep="")),.ctag("br"),file);
    .write(.tag("/div"),file);
    .write(.ctag("br"),file);

    .write(.tag("div",other='style="text-align:center"'),file);
    .write(.tag("h2"),"Evalue distribution of wild and mut peptides",
        .tag("/h2"),file);
    .write(.ctag("img",other=paste('style="width:520px" src="../',.DATA.DIR,
        '/wild_mut_evalue_hist.png"',sep="")),.ctag("br"),file);
    .write(.tag("/div"),file);
    .write(.ctag("br"),file);

    .write(.tag("div",other='style="text-align:center"'),file);
    .write(.tag("h2"),"Mass distribution of wild and mut peptides",
        .tag("/h2"),file);
    .write(.ctag("img",other=paste('style="width:520px" src="../',.DATA.DIR,
        '/wild_mut_mass_hist.png"',sep="")),.ctag("br"),file);
    .write(.tag("/div"),file);
    .write(.ctag("br"),file);

    .write(.tag("div",other='style="text-align:center"'),file);
    .write(.tag("h2"),"Charge distribution of wild and mut peptides",
        .tag("/h2"),file);
    .write(.ctag("img",other=paste('style="width:520px" src="../',.DATA.DIR,
        '/wild_mut_charge_barplot.png"',sep="")),.ctag("br"),file);
    .write(.tag("/div"),file);
    .write(.ctag("br"),file);

    .write(.tag("div",other='style="text-align:center"'),file);
    .write(.tag("h2"),"SAP number distribution of each protein",
        .tag("/h2"),file);
    .write(.ctag("img",other=paste('style="width:520px" src="../',.DATA.DIR,
        '/mut_num_each_protein_barplot.png"',sep="")),.ctag("br"),file);
    .write(.tag("/div"),file);

    .write(.ctag("br"),.ctag("hr",class="clear-contentunit"),
        .ctag("br"),file);
    
    .write(.tag("h1"),"Summary Table",.tag("/h1"),.ctag("br"),file);
    .write(.tag("div",other='style="text-align:center"'),file);
    .write(.tag("/div"),file);
    .write(.tag("div"),.tag("table"),file);
    .writeTable("Peptide","AA_wild","AA_site","AA_mut","AA_delta(Da)",
        "Evalue","SPC","isUnique","isWM","Link",tg1="thead",tg2="th",out=file);
    .write(.tag("tbody"),file);
    for(p in 1:length(data))
    {
        if(p %% 2 == 0)
        {
            color="#F0F0F0";
        }
        else
        {
            color="#FFF";

        }
        for(m in 1:length(data[[p]]))
        {
            tag_link=.concat(.tag("a",other=.concat('href="',
                names(data[p]),'.html"')),data[[p]][[m]][["pro"]],.tag("/a"));
            isWM="no";
            if(!is.null(wmlist[[  data[[p]][[m]][["pro"]]  ]]))
            {
                tmp<-wmlist[[  data[[p]][[m]][["pro"]]  ]]
                if(!is.na(tmp[  data[[p]][[m]][["AA_site"]]  ]))
                {
                    isWM="yes";
                }

            }
            if(length(data[[p]])==1)
            {
				highlight_pep<-.mut_highlight(names(data[[p]][m]),
                                data[[p]][[m]][["rel_coord"]]);
                .writeTable(highlight_pep,
                            data[[p]][[m]][["AA_wild"]],
                            data[[p]][[m]][["AA_site"]],
                            data[[p]][[m]][["AA_mut"]],
                            data[[p]][[m]][["AA_delta"]],
                            data[[p]][[m]][["evalue"]],
                            data[[p]][[m]][["spc"]],
                            data[[p]][[m]][["is_unique"]],
                            isWM,tag_link,
                            bg=color,out=file);
            }
            else
            {
                if(m==1)
                {
					highlight_pep<-.mut_highlight(names(data[[p]][m]),
                                    data[[p]][[m]][["rel_coord"]]);
                    .write(.tag("tr",other=
                        .concat('style="background:',color,'"')),file);
                    .write(.tag("td"),highlight_pep,.tag("/td"),file);
                    .write(.tag("td"),data[[p]][[m]][["AA_wild"]],
                            .tag("/td"),file);
                    .write(.tag("td"),data[[p]][[m]][["AA_site"]],
                            .tag("/td"),file);
                    .write(.tag("td"),data[[p]][[m]][["AA_mut"]],
                            .tag("/td"),file);
                    .write(.tag("td"),data[[p]][[m]][["AA_delta"]],
                            .tag("/td"),file);
                    .write(.tag("td"),data[[p]][[m]][["evalue"]],
                            .tag("/td"),file);
                    .write(.tag("td"),data[[p]][[m]][["spc"]],
                            .tag("/td"),file);
                    .write(.tag("td"),data[[p]][[m]][["is_unique"]],
                            .tag("/td"),file);
                    .write(.tag("td"),isWM,.tag("/td"),file);
                    .write(.tag("td",other=.concat("rowspan=",
                        length(data[[p]]))),tag_link,.tag("/td"),file);
                    .write(.tag("/tr"),file);
                }
                else
                {
					highlight_pep<-.mut_highlight(names(data[[p]][m]),
                                data[[p]][[m]][["rel_coord"]]);
                    .writeTable(highlight_pep,
                                data[[p]][[m]][["AA_wild"]],
                                data[[p]][[m]][["AA_site"]],
                                data[[p]][[m]][["AA_mut"]],
                                data[[p]][[m]][["AA_delta"]],
                                data[[p]][[m]][["evalue"]],
                                data[[p]][[m]][["spc"]],
                                data[[p]][[m]][["is_unique"]],
                                isWM,bg=color,out=file);
                }
            }
        }
    }
    .write(.tag("/tbody"),file);
    .write(.tag("/table"),.tag("/div"),file);
    .write(.ctag("br"),.ctag("hr",class="clear-contentunit"),
            .ctag("br"),file);

    .write(.tag("div",class="top_btn"),.tag("/div"),file);  #-----.top_btn--->
    .write(.tag("/div"),file);
    .write(.tag("/body"),file);
}


# .pepsummary
#
# extract the data for peptide table. input is the dobj
#
# @author xsh
.pepsummary<-function(data)
{
    
    
    pro_pep_pair=c()
    for(p in 2:length(data))
    {
        for(i in 2:length(data[[p]][["IMUT"]]))
        {
            lines=unlist(strsplit(data[[p]][["IMUT"]][[i]],"\n"));
            for(l in lines)
            {
                arr=unlist(strsplit(l,"\t"));
                pro_pep_pair=c(paste(data[[p]][["TRAN"]],
                    arr[10],sep="\t"),pro_pep_pair);
            }
        }
    }
    is_uniq_list=list()
    for(p in unique(pro_pep_pair))
    {
        elem=unlist(strsplit(p,"\t"));
        if(!is.null(is_uniq_list[[ elem[2] ]]))
        {
            is_uniq_list[[ elem[2] ]]="no";
        }
        else
        {
            is_uniq_list[[ elem[2] ]]="yes";
        }
    }
    
    pro_list=list()
    for(p in 2:length(data))
    {
        pro_id=data[[p]][["TRAN"]];
        pep_list=list();
        for(i in 2:length(data[[p]][["IMUT"]]))
        {
            #elem=unlist(strsplit(names(data[[p]][["IMUT"]][i]),"\t"));
            #wild_aa=elem[2];
            #mut_aa=elem[3];
            lines=unlist(strsplit(data[[p]][["IMUT"]][[i]],"\n"));
            aa_site<-unlist(strsplit(names(data[[p]][["IMUT"]][i]),"\t"))[1];
            for(l in lines)
            {
                arr=unlist(strsplit(l,"\t"));
                if(!is.null(pep_list[[ arr[10] ]]))
                {
                    if (pep_list[[ arr[10] ]][["evalue"]]>as.numeric(arr[8])) 
                        pep_list[[ arr[10] ]][["evalue"]]=as.numeric(arr[8]);
                    pep_list[[ arr[10] ]][["spc"]]=
                        as.numeric(pep_list[[ arr[10] ]][["spc"]])+1; 

                }
                else
                {
                    pep_list[[ arr[10] ]][["evalue"]]=as.numeric(arr[8]);
                    pep_list[[ arr[10] ]][["spc"]]=1;
                    pep_list[[ arr[10] ]][["pro"]]=pro_id;
                    pep_list[[ arr[10] ]][["AA_wild"]]=arr[12];
                    pep_list[[ arr[10] ]][["AA_mut"]]=arr[13];
                    pep_list[[ arr[10] ]][["AA_site"]]=aa_site;

                    pep_list[[ arr[10] ]][["rel_coord"]]=arr[14];	
#                     tryCatch(
#                     {
#                         pep_list[[ arr[10] ]][["AA_delta"]]=
#                             round(.AA.TABLE[[arr[13]]]-.AA.TABLE[[arr[12]]],
#                                   digits=2)},
#                     error = function(err) 
#                         paste(err,pro_id,sep="     ")).
                    if(arr[13] == "*")
                    {
                        pep_list[[ arr[10] ]][["AA_delta"]]=
                            round(-.AA.TABLE[[arr[12]]],digits=2);
                    }
                    else if(arr[12]=="*")
                    {
                        pep_list[[ arr[10] ]][["AA_delta"]]=
                            round(.AA.TABLE[[arr[13]]],digits=2);
                    }
                    else
                    {
                        pep_list[[ arr[10] ]][["AA_delta"]]=
                            round(.AA.TABLE[[arr[13]]]-.AA.TABLE[[arr[12]]],
                                digits=2);
                    }
                    pep_list[[arr[10]]][["is_unique"]]=is_uniq_list[[arr[10]]];
                }
            }
        }
        pro_list[[ names(data[p]) ]]=pep_list;
    }
    return(pro_list)
}
#############################main###############################
# .rshtml
# 
# Creates the html statistics page of identification;
#
# @param data List object from .dataHandle_R
# @param pep.summary.path The peptide summary file.
# @param pro.summary.path The protein summary file.
# @param wmlist The list object from \code{rchtml}
.rshtml<-function(data,pep.summary.path,pro.summary.path,wmlist,outdir)
{
    #This is just a mock variable,and it will remove the note(not warning)
    #caused by subset;
    peptide<-index<-isSAP<-NULL;  
    page_dir=paste(outdir,.PAGE.DIR,sep="/");

    if(!file.exists(page_dir))
    {
        dir.create(page_dir);
        cssdir=paste(system.file(package="sapFinder"),"css",sep="/");
        imdir=paste(system.file(package="sapFinder"),"images",sep="/");
        jsdir=paste(system.file(package="sapFinder"),"js",sep="/");
        file.copy(c(cssdir,imdir,jsdir),page_dir,recursive = TRUE);
    }
    data_dir=paste(outdir,.DATA.DIR,sep="/");

    if(!file.exists(data_dir))
    {
        dir.create(data_dir);
    }
    

    ###################1.heatmap##########
    #png(paste(.DATA.DIR,"mutation_frequency_heatmap.png",sep="/"),
    #    width = 520, height = 520)
    #.mut_freq_heatmap(map.table)
    #dev.off()
    pep.df<-read.delim(pep.summary.path,header=TRUE,stringsAsFactors=FALSE);
    pro.df<-read.delim(pro.summary.path,header=TRUE,stringsAsFactors=FALSE);

    pep.df$delta_ppm=as.numeric(pep.df$delta_ppm);
    pep.df$delta_da=as.numeric(pep.df$delta_da);
    pep.df$mass=as.numeric(pep.df$mass);
    pep.df$evalue=as.numeric(pep.df$evalue);

    stat_list<-list();
    stat_list[["all_pro_num"]]<-dim(pro.df)[1];
    stat_list[["all_sp_num"]]<-dim(pep.df)[1];
    stat_list[["all_pep_num"]]<-length(unique(pep.df$peptide));
    stat_list[["mut_pep_num"]]<-dim(unique(subset(pep.df,isSAP=="true",
                                            select=c(peptide))))[1];
    stat_list[["mut_pro_num"]]<-length(data)-1;
    stat_list[["mut_sp_num"]]<-dim(unique(subset(pep.df,isSAP=="true",
                                            select=c(index))))[1];
    
    
    #mut_num_vector=c()
    #for(p in 2:length(data))
    #{
    #    for(i in 2:length(data[[p]][["IMUT"]]))
    #    {   
    #        tmp=unlist(strsplit(names(data[[p]][["IMUT"]][i]),"\t"))[5];
    #        mut_num_vector=c(mut_num_vector,unlist(strsplit(tmp,'[|]')));
    #    }
    #}
    
    #mut_num=length(unique(mut_num_vector))
    #################2.mass distribution of MS level one ###################
    png(paste(data_dir,"precursor_ms1_error.png",sep="/"),
        width = 540, height = 420);
    par(mar=c(4,4,1,1));
    .precursor_error_hist(pep.df);
    dev.off();
    ###########3.Evalue histogram of mutant peptide and wild peptide#########
    png(paste(data_dir,"wild_mut_evalue_hist.png",sep="/"),
        width = 540, height = 420);
    par(mar=c(4, 4, 1, 1));
    .wm_evalue_hist(pep.df);
    dev.off();

    #######4.Mass histogram of mutant peptide and wild peptide###############
    png(paste(data_dir,"wild_mut_mass_hist.png",sep="/"),
        width = 540, height = 420);
    par(mar=c(4, 4, 1, 1));
    .wm_mass_hist(pep.df);
    dev.off();

    #######5.Charge barplot of mutant peptide and wild peptide###############
    png(paste(data_dir,"wild_mut_charge_barplot.png",sep="/"),
        width = 540, height = 420);
    par(mar=c(4,4,1,1));
    .wm_charge_bar(pep.df);
    dev.off();

    #########6.barplot of protein number ##########
    png(paste(data_dir,"mut_num_each_protein_barplot.png",sep="/"),
        width = 540, height = 420);
    par(mar=c(4,4,1,1));
    .mut_pro_dist(data);
    dev.off();
    ####### data process
    fdata<-.pepsummary(data);
    ####### writing html
    .writeHtml_s(fdata,stat_list,
                pep.summary.path,
                pro.summary.path,
                filename="search_stat",
                wmlist=wmlist,
                outdir=outdir);
}
