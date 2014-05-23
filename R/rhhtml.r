# .writeHtml_h
#
# write the html;
#
# @author xsh

.writeHtml_h<-function(outdir,filename="myTest")
{
    page_dir=paste(outdir,.PAGE.DIR,sep="/");
    filename=.concat(page_dir,"/", filename, ".html" );
    file=file(filename,"w");
    
    cat('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"',
        ' "http://www.w3.org/TR/html4/loose.dtd">\n', file=file, sep="");
    .write(.tag("html"),file);
    .writeHead_h(file);
    .writeBody_h(file);
    .write(.tag("/html"),file);
    
    close(file);
}

# .writeHead_h
#
# write the header partion;
#
# @author xsh
.writeHead_h<-function(file)
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
# .writeBody_h
#
# write the body partion;
#
# @author xsh

.writeBody_h<-function(file)
{
    .write(.tag("body"),file);
    .write(.tag("div",class="seq_cn1"),file);
    
    #.write(.tag("h1"),"Summary List",.tag("/h1"),.ctag("br"),file);
    .write(.tag("div",other='style="text-align:center"'),file);
    
    .write(.ctag("img",other=paste('style="width:800px" 
            src="images/help_images/help1.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),"The main page of this report."
            ,.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.ctag("img",other=paste('style="width:650px" 
            src="images/help_images/help2.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),"The description section.",.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.ctag("img",other=paste('style="width:800px" 
            src="images/help_images/help3.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),
            'The identified SAPs are highlighted in red in the sequence. 
            The un-identified SAP is highlighted in gray. When you hover 
            your mouse pointer over the SAP, the amino acid substitution 
            will display in a small box next to the mouse. If you want to 
            access the basic information about the mutation, you can single 
            click the SAP. Only the identified-SAPs have the "MSMS Summary" 
            section. At the same time, the "MSMS Summary" section in the 
            identified-SAPs bubble tips will also only contain the basic 
            identification result of relevant SAP. In addition, you can view 
            the labeled spectra with SAP by clicking the first column of 
            "MSMS Summary".'
            ,.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.ctag("img",other=paste('style="width:900px" 
            src="images/help_images/help4.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),
            'If you want to check the whole identified peptides of the 
            corresponding proteins, you can get them from the "Peptide 
            Summary Report" section. You can distinguish between variant 
            and wild-type peptides by the column "isSAP". In addition, the 
            column "miss" represents the number of missed cleavage.'
            ,.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.ctag("img",other=paste('style="width:600px" 
            src="images/help_images/help5.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),
            'This is the spectra of "RLNEGSSAMANG<b><font 
            color=\'red\'>V</font></b>EEKEPEAPEM".Green fragment labels 
            represent all possible matches of b ions. Blue fragment labels 
            represent all possible matches of  y ions. And red labels 
            represent the fragment ions contain the amino acid substitution, 
            no matter they are b ions or y ions.'
            ,.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.ctag("img",other=paste('style="width:900px" 
            src="images/help_images/help6.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),
            "The Search Stat page shows the identification-related 
            statistical values and diagrams."
            ,.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.ctag("img",other=paste('style="width:800px" 
            src="images/help_images/help7.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),
            'In Search Stat page, there is a "Summary Table" section that 
            include all of the Variant Peptide detected in this workflow. 
            And through the Link column, being able to jump to the 
            corresponding Protein. The meanings of different columns 
            are as follows:'
            ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
           '1."AA_wild" column represents the wild-type amino acid.'
           ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
            '2."AA_site" column represents the site of amino acid 
            change in the coding sequence.'
            ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
           '3."AA_mut" column represents the variant amino acid.'
           ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
            '4."AA_delta(Da)" column represents the difference between 
            variant and wild-type amino acids (AA_mut - AA_wild).'
            ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
            '5."Evalue" column represents  the maximum E-value of 
            corresponding peptide.'
            ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
            '6."SPC" column represents the spectral counting of 
            identified peptide.'
            ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
            '7."isUnique" column represents the variant peptide is 
            unique to one particular protein or not.'
            ,.tag("/p"),file);
    .write(.tag("p",class='ident'),
            '8."isWM" column represents whether the variant peptide and 
            corresponding wild-type peptide are identified at the same time.'
            ,.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.ctag("img",other=paste('style="width:900px" 
            src="images/help_images/help8.png"',sep="")),.ctag("br"),file);
    .write(.tag("div",id='plaincontent'),file);
    .write(.tag("p",class='ident'),
            "The Database Stat page shows the database-related statistical 
            values about SAPs. And the mutation frequency heatmap illustrates 
            the count of different amino-acid substitutions in the database."
            ,.tag("/p"),file);
    .write(.tag("/div"),file);
    
    .write(.tag("/div"),file);
    .write(.tag("div",class="top_btn"),.tag("/div"),file); 
    .write(.tag("/div"),file);
    #--------------.top_btn------------>
    .write(.tag("/body"),file);
}

#############################main###############################
# .rhhtml
# 
# Creates the html help page; 
.rhhtml<-function(outdir)
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
    
    .writeHtml_h(outdir,filename="help");
}
