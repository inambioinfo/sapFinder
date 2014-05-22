#include <Rcpp.h>
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <sstream>
#include <iomanip> //设置精度时使用
//#include <tr1/unordered_map>

//R Stuff
extern "C" 
{
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
}


using namespace Rcpp;

//split函数实现
void split(const std::string& s, const std::string& delim,std::vector<std::string> *ret) 
{ 
	size_t last = 0;
	size_t index=s.find_first_of(delim,last); 
	while (index!=std::string::npos) 
	{
		ret->push_back(s.substr(last,index-last));
		last=index+1;
		index=s.find_first_of(delim,last); 
	} 
	if (index-last>0)
	{ 
		ret->push_back(s.substr(last,index-last)); 
	} 
} 
//integer to std::string转换
std::string int2str(int &i) {
	std::string s;
	std::stringstream ss(s);
	ss << i;

	return ss.str();
}
//std::string to integer转换
int str2int(std::string &s) {
	int i;
	std::stringstream ss(s);
	ss >> i;
	return i;
}

//字符串类型数字保留特定小数位点。返回的还是字符串
std::string round4str(std::string &s) {//保留四位小数
	float f;
	std::string ns;
	std::stringstream ss(s);
	std::stringstream newss(ns);
	ss >> f;
	newss<< std::fixed<<std::setprecision(4) << f;
	return newss.str();
}
std::string round2str(std::string &s) {//保留两位小数
	float f;
	std::string ns;
	std::stringstream ss(s);
	std::stringstream newss(ns);
	ss >> f;
	newss<< std::fixed<<std::setprecision(2) << f;
	return newss.str();
}

RcppExport SEXP dataHandle_Cpp(SEXP rawdata_, SEXP map_table_, SEXP fasta_)
{
	std::string rawdata = as<std::string> (rawdata_);
	std::string map_table = as<std::string> (map_table_);
	std::string fasta = as<std::string> (fasta_);

	typedef std::map<std::string, std::string> hash_fasta;
	typedef std::map<std::string, std::string> hash_map;
	typedef std::map<std::string, std::string> hash_mutPro; //只保留突变蛋白的id。
	typedef std::map<std::string, std::string> hash_section;
	typedef std::map<std::string, std::string> hash_unique;
	typedef std::map<std::string, std::string> hash_unMut;
	typedef std::map<std::string, std::string> dict_map;
	typedef std::map<std::string, std::string> dict_ident;

	hash_fasta hashF; //Fasta字典
	hash_map hashM; //Map表字典
	hash_section hashS;  //按蛋白ID构建的一个字典
	hash_mutPro hashMP; //含突变蛋白记录字典
	hash_unMut hashUM; //记录蛋白上的所有突变位点（包含鉴定和未鉴定到的。）
	hash_unique hashU; //一条query中可能比到多个ID，如果是正常肽段，其中的突变ID是会被map回到正常蛋白ID的，这样就造成一条记录在同一蛋白ID中出现了多次，因此需要去冗余。又由于允许一条query同时出现在不同的正常蛋白ID中，因此，此hashU的键应该为"pro_id+query号"。重复出现的不保存。
	dict_map dictG;   //转录本id和基因名的map
	dict_map dictC;   //转录本id和染色体的map
	dict_map dictProt;   //转录本id和相应蛋白id的map,新增xref，不一定存在
	dict_map dictSp;   //转录本id和swissprot id的map,新增xref，不一定存在
	dict_map dictDesc;   //转录本id和description的map,新增xref，不一定存在
	dict_ident dictIW; //用于记录蛋白中鉴定到的正常肽段位点区间
	dict_ident dictIM; //用于记录蛋白中鉴定到的突变肽段位点区间
	dict_ident dictIRS; //用于鉴定到的突变肽段中突变位点相对该肽段的相对坐标，主要是画谱图时使用。

	/*读取map表*/
	std::ifstream min(map_table.c_str());//相当于perl中的句柄,c_str()函数返回一个指向正规C字符串的指针, 内容与本std::string串相同,直接给fin传入std::string是ok的，但传std::string变量是会报错的，需要转化为指针.
	if(!min)
	{
		//std::cerr << "Couldn't open the map file!";//在R包中，最好不要用cerr或cout等C++的IO函数，check包时会报警告。
		return wrap(NA_REAL);
	}
	std::string line;  //每行字符串临时变量
	getline(min, line);//获取首行title
	int rawid_k=-1,pro_k=-1,gene_k=-1,site_k=-1,start_k=-1,wild_k=-1,mut_k=-1,type_k=-1,db_k=-1,chr_k=-1,chr_site_k=-1,wna_k=-1,mna_k=-1;
	int prot_k=-1,sp_k=-1,desc_k=-1;  //这三个为xref项，不是100%存在。
	{	//作用域限定空间, 获取不同title的列号
		std::vector<std::string> elem;
		split(line,"\t",&elem);
		for (unsigned int i=0;i<elem.size();i++)
		{
			if(elem[i]=="ID")
			{
				rawid_k=i;
				//cout<<elem[i]<<std::endl;
			}
			else if(elem[i]=="Protein_ID")
			{	
				pro_k=i;
			}
			else if(elem[i]=="Gene")
			{	
				gene_k=i;
			}
			else if(elem[i]=="Mut_coords")
			{	
				site_k=i;
			}
            else if(elem[i]=="Chr_coords")
			{	
				chr_site_k=i;
			}
            else if(elem[i]=="NA_wild")
  			{	
				wna_k=i;
			}	
    	    else if(elem[i]=="NA_mut")
  			{	
				mna_k=i;
			}	
			else if(elem[i]=="Start")
			{	
				start_k=i;
			}			
			//else if(elem[i]=="End")
			//{
			//	end_k=i;
			//}
			else if(elem[i]=="AA_wild")
			{	
				wild_k=i;
			}
			else if(elem[i]=="AA_mut")
			{	
				mut_k=i;
			}
			else if(elem[i]=="Type")
			{	
				type_k=i;
			}
			else if(elem[i]=="Mut_ID")
			{	
				db_k=i;
			}
			else if(elem[i]=="chromosome")
			{	
				chr_k=i;
			}
			else if(elem[i]=="ProtID")
			{
				prot_k=i;  //xref项，不是一定存在
			}
			else if(elem[i]=="SpID")
			{
				sp_k=i;  //xref项，不是一定存在
			}
			else if(elem[i]=="Desc")
			{
				desc_k=i;  //xref项，不是一定存在
			}

		}
	}


	getline(min, line);//获取一行
	while(min)
	{
		std::vector<std::string> map_array;
		split(line,"\t",&map_array); //split的第三项为指针
		
		hashM[map_array[rawid_k]]=line;//此句不能放在下面过滤LI和IL的后面，因为突变ID包含I->L或L->I的ID，如果在这里就过滤掉，后面会导致在hashM中找不到值。引起std::bad_alloc，内存分配失败；因此在后面需要再过滤一遍[LI]。（其实最好的办法就是要构库时直接去除[LI]）
		std::string aa_pair =map_array[wild_k]+map_array[mut_k]; //氨基酸突变前后对
		if(aa_pair.find("IL") !=std::string::npos ||aa_pair.find("LI") !=std::string::npos) //I->L或L->I的不是突变
		{
			getline(min, line);
			continue;
		}

		if(hashUM.count(map_array[pro_k]))
		{
			hashUM[map_array[pro_k]]=hashUM[map_array[pro_k]]+"\n"+map_array[site_k]+"\t"+map_array[wild_k]+"\t"+map_array[mut_k]+"\t"+map_array[type_k]+"\t"+map_array[db_k]+"\t"+map_array[chr_site_k]+"\t"+map_array[wna_k]+"->"+map_array[mna_k];
		}
		else
		{
			hashUM[map_array[pro_k]]=map_array[site_k]+"\t"+map_array[wild_k]+"\t"+map_array[mut_k]+"\t"+map_array[type_k]+"\t"+map_array[db_k]+"\t"+map_array[chr_site_k]+"\t"+map_array[wna_k]+"->"+map_array[mna_k];
		}
		getline(min, line);  
	}
	min.close();
	/*读取hash表*/
	std::ifstream fin(fasta.c_str());//相当于perl中的句柄,c_str()函数返回一个指向正规C字符串的指针, 内容与本std::string串相同,直接给fin传入std::string是ok的，但传std::string变量是会报错的，需要转化为指针.
	if(!fin)
	{
		//std::cerr << "Couldn't open the fasta file!";
		return wrap(NA_REAL);
	}
	std::string tmp_id;//id临时变量

	// Priming read. May want to check that this line is a "header" before the loop.
	getline(fin, line);//获取一行
	while(fin)
	{
		if(line[0] == '>')
		{

			size_t end = line.find_first_of(" \t\n\r");//找到首个空格
			//cout<< line.substr(1,end-1) <<std::endl;
			tmp_id=line.substr(1,end-1); //从1开始忽略>号，end-1到首个空格前
		}
		else
		{
			if(hashF.count(tmp_id))//count方法就相当于perl中的exists,判断键是否存在
			{
				hashF[tmp_id]+=line;
			}
			else
			{
				hashF[tmp_id]=line;	//插入值比较简单，和java中的基本一致。
			}
		}
		getline(fin, line);	//继续获取下一行
	}
	fin.close();

	/*rawdata处理*/
	std::ifstream rin(rawdata.c_str());  //相当于perl中的句柄,c_str()函数返回一个指向正规C字符串的指针, 内容与本std::string串相同,直接给fin传入std::string是ok的，但传std::string变量是会报错的，需要转化为指针.
	if(!rin)
	{
		//std::cerr << "Couldn't open the rawdata file!";
		return wrap(NA_REAL);
	}
	getline(rin, line); //获取首行title
	int query_i=-1,charge_i=-1,mz_i=-1,delta_i=-1,isDecoy_i=-1,isSAP_i=-1,miss_i=-1,range_i=-1,qvalue_i=-1,pep_i=-1,mod_i=-1,rawpro_i=-1,evalue_i=-1,rt_i=-1;
	{	//作用域限定空间, 获取不同title的列号
		std::vector<std::string> elem;
		split(line,"\t",&elem);
		for (unsigned int i=0;i<elem.size();i++)
		{
			if(elem[i]=="index")	//query的title已改成index,但为了好区分，这里还是叫query
			{
				query_i=i;
				//cout<<elem[i]<<std::endl;
			}
			else if(elem[i]=="charge")
			{	
				charge_i=i;
			}
			else if(elem[i]=="mz")
			{	
				mz_i=i;
			}
			else if(elem[i]=="delta_ppm")
			{	
				delta_i=i;
			}
			else if(elem[i]=="position")
			{	
				range_i=i;
			}
			else if(elem[i]=="isdecoy")
			{	
				isDecoy_i=i;
			}
				else if(elem[i]=="isSAP")
			{	
				isSAP_i=i;
			}
			else if(elem[i]=="miss")
			{	
				miss_i=i;
			}

			else if(elem[i]=="Qvalue")
			{	
				qvalue_i=i;
			}
			else if(elem[i]=="evalue")
			{	
				evalue_i=i;
			}
			else if(elem[i]=="peptide")
			{	
				pep_i=i;
			}
			else if(elem[i]=="protein")
			{	
				rawpro_i=i;
			}
			else if(elem[i]=="mods")
			{	
				mod_i=i;
			}			
			else if(elem[i]=="rt")
			{	
				rt_i=i;
			}
		}
	}
  
	getline(rin, line);
	while(rin)
	{

		std::vector<std::string> elem;
		split(line,"\t",&elem);
		
		std::vector<std::string> id_array;
		std::vector<std::string> range_array;
		bool isSAP;
		bool isDecoy;
		split(elem[rawpro_i],";",&id_array);
		split(elem[range_i],";",&range_array);	//每张突变谱可能对应多个不同的变异id,相应的有可能会有多个不同的突变肽坐标区间。它们之间用;分隔。
		if(elem[isDecoy_i]=="false")
		{
			isDecoy=false;
		}
		else
		{
			isDecoy=true;
		}
		if(elem[isSAP_i]=="false")
		{
			isSAP=false;
		}
		else
		{
			isSAP=true;
		}
		if(!isDecoy)  //仅输出非decoy的序列
		{
			if(isSAP)
			{
				for (unsigned int i=0;i<id_array.size();i++)
				{
					size_t rvfound = (id_array[i]).find("###REV###");
					if(rvfound!=std::string::npos) //去除decoy的id
					{
						continue;
					}
					std::vector<std::string> map_array;
					split(hashM[id_array[i]],"\t",&map_array);
					/*下面键为pro_id和query号合成的字符串,值为“pep,isSAP,site,mut_aa,wild_aa,Type,DB_id”;而基因名，染色体及蛋白序列信息由于与具体鉴定情况无关，后面根据map表和fasta的hash再获取即可*/
					std::string uniq_string=map_array[pro_k]+";"+elem[query_i];
					if(hashU.count(uniq_string))
					{
						continue;//同一正常蛋白ID中只充许某一query号的记录出现一次。多余的需要过滤掉。避免重复输出
					}
					hashU[uniq_string]=1;
					hashMP[map_array[pro_k]]=1;	// 标记该蛋白是否存在突变位点。
					dictG[map_array[pro_k]]=map_array[gene_k];
					dictC[map_array[pro_k]]=map_array[chr_k];

					if(prot_k>-1)  //这三项不是100%存在，因此需要判定索引是否误置-1。是-1则不保存。
					{
						dictProt[map_array[pro_k]]=map_array[prot_k];
					}
					if(sp_k>-1)
					{
						dictSp[map_array[pro_k]]=map_array[sp_k];
					}
					if(desc_k>-1)
					{
						dictDesc[map_array[pro_k]]=map_array[desc_k];
					}


					std::vector<std::string> se;//start和end位点
					split(range_array[i],":",&se);//起止位点是以":"分割的,se[0]为start;se[1]里的为end。
					int s=str2int(map_array[start_k])+str2int(se[0])-1;//突变肽段还原到蛋白序列上的起始位点坐标。因为se的起止只是突变肽段在前后各两漏切的短肽上的相对坐标。
					int e=str2int(map_array[start_k])+str2int(se[1])-1; //突变肽段还原到蛋白序列上的终止位点坐标。

					int relative_mut_coords = str2int(map_array[site_k])-s+1;   //检测到的突变肽段中突变位点的相对坐标。主要用于画谱图时确定并标出哪些是包含变异的fragment.

					if(dictIM.count(map_array[pro_k]))//将校正好的绝对坐标区间记录到dictIM字典中。由于同一肽段多张谱图的关系，会存在大量的重复，没有关系，到R中可以很方便的去除冗余。
					{
						dictIM[map_array[pro_k]]=dictIM[map_array[pro_k]]+";"+int2str(s)+":"+int2str(e);
					}
					else
					{
						dictIM[map_array[pro_k]]=int2str(s)+":"+int2str(e);
					}
					//cerr<<s<<"\t"<<e<<std::endl;

					if(hashS.count(map_array[pro_k]))
					{
						hashS[map_array[pro_k]]=hashS[map_array[pro_k]]+"\n"+elem[pep_i]+"\t"+"yes"+"\t"+map_array[site_k]+"\t"+map_array[wild_k]+"\t"+map_array[mut_k]+"\t"+map_array[type_k]+"\t"+map_array[db_k]+"\t"+map_array[chr_site_k]+"\t"+map_array[wna_k]+"->"+map_array[mna_k]+"\t"+elem[query_i]+"\t"+elem[charge_i]+"\t"+round4str(elem[mz_i])+"\t"+round4str(elem[delta_i])+"\t"+elem[isSAP_i]+"\t"+elem[miss_i]+"\t"+round2str(elem[rt_i])+"\t"+round4str(elem[evalue_i])+"\t"+round4str(elem[qvalue_i])+"\t"+elem[pep_i]+"\t"+elem[mod_i]+"\t"+int2str(relative_mut_coords);
					}
					else
					{
						hashS[map_array[pro_k]]=elem[pep_i]+"\t"+"yes"+"\t"+map_array[site_k]+"\t"+map_array[wild_k]+"\t"+map_array[mut_k]+"\t"+map_array[type_k]+"\t"+map_array[db_k]+"\t"+map_array[chr_site_k]+"\t"+map_array[wna_k]+"->"+map_array[mna_k]+"\t"+elem[query_i]+"\t"+elem[charge_i]+"\t"+round4str(elem[mz_i])+"\t"+round4str(elem[delta_i])+"\t"+elem[isSAP_i]+"\t"+elem[miss_i]+"\t"+round2str(elem[rt_i])+"\t"+round4str(elem[evalue_i])+"\t"+round4str(elem[qvalue_i])+"\t"+elem[pep_i]+"\t"+elem[mod_i]+"\t"+int2str(relative_mut_coords);	//仅突变的谱图才有index=20（即第21项）的relative_mut_coords。下面非突变的谱图只有20项。
					}
				}

			}
			else
			{
				for (unsigned int i=0;i<id_array.size();i++)
				{
					size_t rvfound = (id_array[i]).find("###REV###");
					if(rvfound!=std::string::npos) //去除decoy的id
					{
						continue;
					}
      
					size_t msfound = (id_array[i]).rfind("|MS");
					size_t slfound = (id_array[i]).rfind("|SL");
					size_t sgfound = (id_array[i]).rfind("|SG");
					if(msfound!=std::string::npos)
					{
						msfound+=3;  //加上的是"|MS"三个字符的长度
					}
					if(slfound!=std::string::npos)
					{
						slfound+=3;  //加上的是"|SL"三个字符的长度
					}
					if(sgfound!=std::string::npos)
					{
						sgfound+=3;  //加上的是"|SG"三个字符的长度
					}

					if(msfound == (id_array[i]).size() || slfound == (id_array[i]).size() || sgfound == (id_array[i]).size()) //此部分用于解决tandem可能存在的bug。用于校正正常肽段中的突变ID到正常蛋白ID中，理论情况下，如果是正常肽段，那么突变ID对应的正常蛋白ID必存在于该ID串中（mascot即是如此），如“COS13143|21-41|V37A|MS;ACTC1”;将COS13143|21-41|V37A|MS;校正回后正好为ACTC1。但是，对于tandem可能会存在将突变ID校正回正常蛋白ID时该正常蛋白ID不存在于该记录串中（此问题在mascot的结果中不存在）。因此需要这段代码来保证信息的完整性。
					{
						std::vector<std::string> map_array;
						split(hashM[id_array[i]],"\t",&map_array);
						std::string uniq_string=map_array[pro_k]+";"+elem[query_i];
						if(hashU.count(uniq_string))
						{
							continue;
						}
						hashU[uniq_string]=1;

						std::vector<std::string> se;//start和end位点
						split(range_array[i],":",&se);//起止位点是以":"分割的,se[0]为start;se[1]里的为end。
						int s=str2int(map_array[start_k])+str2int(se[0])-1;//wild肽段还原到蛋白序列上的起始位点坐标。
						int e=str2int(map_array[start_k])+str2int(se[1])-1; //wild肽段还原到蛋白序列上的终止位点坐标。
						if(dictIW.count(map_array[pro_k]))
						{
							dictIW[map_array[pro_k]]=dictIW[map_array[pro_k]]+";"+int2str(s)+":"+int2str(e);
						}
						else
						{
							dictIW[map_array[pro_k]]=int2str(s)+":"+int2str(e);
						}

						if(hashS.count(map_array[pro_k]))
						{
							hashS[map_array[pro_k]]=hashS[map_array[pro_k]]+"\n"+elem[pep_i]+"\t"+"no"+"\tNA\tNA\tNA\tNA\tNA\tNA\tNA"+"\t"+elem[query_i]+"\t"+elem[charge_i]+"\t"+round4str(elem[mz_i])+"\t"+round4str(elem[delta_i])+"\t"+elem[isSAP_i]+"\t"+elem[miss_i]+"\t"+round2str(elem[rt_i])+"\t"+round4str(elem[evalue_i])+"\t"+round4str(elem[qvalue_i])+"\t"+elem[pep_i]+"\t"+elem[mod_i];
						}
						else
						{
							hashS[map_array[pro_k]]=elem[pep_i]+"\t"+"no"+"\tNA\tNA\tNA\tNA\tNA\tNA\tNA"+"\t"+elem[query_i]+"\t"+elem[charge_i]+"\t"+round4str(elem[mz_i])+"\t"+round4str(elem[delta_i])+"\t"+elem[isSAP_i]+"\t"+elem[miss_i]+"\t"+round2str(elem[rt_i])+"\t"+round4str(elem[evalue_i])+"\t"+round4str(elem[qvalue_i])+"\t"+elem[pep_i]+"\t"+elem[mod_i];
						}
					}
					else
					{
						if(hashF.count(id_array[i]))
						{
							std::string uniq_string=(id_array[i])+";"+elem[query_i];
							if(hashU.count(uniq_string))
							{
								continue;
							}
							hashU[uniq_string]=1;

							std::vector<std::string> se;//start和end位点
							split(range_array[i],":",&se);//起止位点是以":"分割的,se[0]为start;se[1]里的为end。
							if(dictIW.count(id_array[i]))
							{
								dictIW[id_array[i]]=dictIW[id_array[i]]+";"+se[0]+":"+se[1];
							}
							else
							{
								dictIW[id_array[i]]=se[0]+":"+se[1];
							}

							if(hashS.count(id_array[i]))
							{
								hashS[id_array[i]]=hashS[id_array[i]]+"\n"+elem[pep_i]+"\t"+"no"+"\tNA\tNA\tNA\tNA\tNA\tNA\tNA"+"\t"+elem[query_i]+"\t"+elem[charge_i]+"\t"+round4str(elem[mz_i])+"\t"+round4str(elem[delta_i])+"\t"+elem[isSAP_i]+"\t"+elem[miss_i]+"\t"+round2str(elem[rt_i])+"\t"+round4str(elem[evalue_i])+"\t"+round4str(elem[qvalue_i])+"\t"+elem[pep_i]+"\t"+elem[mod_i];
							}
							else
							{
								hashS[id_array[i]]=elem[pep_i]+"\t"+"no"+"\tNA\tNA\tNA\tNA\tNA\tNA\tNA"+"\t"+elem[query_i]+"\t"+elem[charge_i]+"\t"+round4str(elem[mz_i])+"\t"+round4str(elem[delta_i])+"\t"+elem[isSAP_i]+"\t"+elem[miss_i]+"\t"+round2str(elem[rt_i])+"\t"+round4str(elem[evalue_i])+"\t"+round4str(elem[qvalue_i])+"\t"+elem[pep_i]+"\t"+elem[mod_i];
							}
						}
						else
						{
							//std::cerr<<id_array[i]<<"doesn't exists!"<<std::endl;
						}
					}
					
				}
			}
		}
		getline(rin, line);
	}
	rin.close();

	//std::ofstream fout;//~output data object~//
	//fout.open("filter.TEMP.tsv");//~output data object~//
	
	Rcpp::List proList("null");
	int index=0;
	for(hash_section::iterator it=hashS.begin();it != hashS.end();++it)
	{
		if(hashMP.count(it->first)) //有突变位点的才输出
		{
			std::vector<std::string> line;
			split(it->second,"\n",&line);	//按换行符分隔的行,每行是一张隶属于该蛋白突变或正常肽段的谱图鉴定信息及map表信息。
			std::vector<std::string>::iterator itl; //行迭代器
			//typedef std::map<std::string, std::string> redundant; //肽段可能出现多次，只记录一次。
			typedef std::map<std::string, std::string> imut_group; //鉴定到的包含突变位点的肽段分组用。
			//redundant red;
			imut_group img;
			imut_group img_new;

			//fout<<std::endl;
			//fout<<it->second<<std::endl;//DEBUG
			//fout<<std::endl;
			Rcpp::List infoList("null");
			infoList["TRAN"]=it->first;
			infoList["GENE"]=dictG[it->first];
			infoList["CHR"]=dictC[it->first];
			if(prot_k>-1)
			{
				infoList["PROT"]=dictProt[it->first];  //举个例子，PROT为ENSP号
			}
			if(sp_k>-1)
			{
				infoList["SP"]=dictSp[it->first];
			}
			if(desc_k>-1)
			{
				infoList["DESC"]=dictDesc[it->first];
			}
			infoList["SEQ"]=hashF[it->first];
			infoList["RANGE_M"]=dictIM[it->first];
			infoList["RANGE_W"]=dictIW[it->first];

			//fout<<"=BEGIN"<<std::endl; //~output data object~//
			//fout<<"@TRAN\t"<<it->first<<std::endl; //~output data object~//
			//fout<<"@GENE\t"<<dictG[it->first]<<std::endl; //~output data object~//
			//fout<<"@CHR\t"<<dictC[it->first]<<std::endl; //~output data object~//
			//fout<<"@SEQ\t"<<hashF[it->first]<<std::endl; //~output data object~//
			//fout<<"@RAGNE\t"<<dictI[it->first]<<std::endl; //~output data object~//
			//fout<<"@ALL_TABLE_START"<<std::endl; //~output data object~//

			Rcpp::List allTableList("null");  //用于保存每个突变蛋白的Peptide Summary report.即该蛋白的突变和未突变的所有谱图总和。
			for (itl=line.begin(); itl!=line.end(); ++itl)
			{
				std::vector<std::string> elem;
				split(*itl,"\t",&elem);
				//fout<<elem[7]<<"\t"<<elem[8]<<"\t"<<elem[9]<<"\t"<<elem[10]<<"\t"<<elem[11]<<"\t"<<elem[12]<<"\t"<<elem[13]<<"\t"<<elem[14]<<"\t"<<elem[15]<<"\t"<<elem[16]<<"\t"<<elem[17]<<std::endl; //~output data object~//
				allTableList[elem[9]]=elem[9]+"\t"+elem[10]+"\t"+elem[11]+"\t"+elem[12]+"\t"+elem[13]+"\t"+elem[14]+"\t"+elem[15]+"\t"+elem[16]+"\t"+elem[17]+"\t"+elem[18]+"\t"+elem[19];
				if(elem[1] == "yes")
				{ 
					std::string key=elem[2]+"\t"+elem[3]+"\t"+elem[4]+"\t"+elem[5]+"\t"+elem[6]+"\t"+elem[7]+"\t"+elem[8];
					if(img.count(key))//以site,wildAA,mutAA,突变类型，突变ID,染色体突变座标，突变前后核苷酸对 作键。
					{
						img[key]=img[key]+"\n"+elem[9]+"\t"+elem[10]+"\t"+elem[11]+"\t"+elem[12]+"\t"+elem[13]+"\t"+elem[14]+"\t"+elem[15]+"\t"+elem[16]+"\t"+elem[17]+"\t"+elem[18]+"\t"+elem[19]+"\t"+elem[3]+"\t"+elem[4]+"\t"+elem[20];
					}
					else
					{
						img[key]=elem[9]+"\t"+elem[10]+"\t"+elem[11]+"\t"+elem[12]+"\t"+elem[13]+"\t"+elem[14]+"\t"+elem[15]+"\t"+elem[16]+"\t"+elem[17]+"\t"+elem[18]+"\t"+elem[19]+"\t"+elem[3]+"\t"+elem[4]+"\t"+elem[20];	//elem[20]即为relative_mut_coords。只有突变的谱图有。
					}
				}
				//else if(elem[1] == "no")
				//{
				//	red[elem[0]]=1;
				//}
			}
			
			//fout<<"@ALL_TABLE_END"<<std::endl; //~output data object~//
			infoList["ALLTABLE"]=allTableList;

			/*=====同一位点双不同突变且同时被鉴定引起的bug修正开始=====*/	
			typedef std::map<std::string, std::string> hash_double_mut_key; //计录蛋白同同一突变位点出现突变类型的数目。主要用于修正“同一突变位点同时”发生了两个突变，并且被“同时鉴定”到的问题。
			typedef std::map<std::string, std::string> hash_double_mut_value; //计录蛋白同同一突变位点出现突变类型的数目。主要用于修正“同一突变位点同时”发生了两个突变，并且被“同时鉴定”到的问题。
			hash_double_mut_key hashDMK; 
			hash_double_mut_value hashDMV; 
			for(imut_group::iterator iti=img.begin();iti != img.end();++iti) 
			{
				std::vector<std::string> elem;
				split(iti->first,"\t",&elem);
				if(hashDMK.count(elem[0]))
				{
					/*std::vector<std::string> arr;
					split(hashDM[elem[0]],"#SEP#",&arr);*/
					std::vector<std::string> arr;
					split(hashDMK[elem[0]],"\t",&arr);

					std::string key=elem[0]+"\t"+elem[1]+"\t"+arr[2]+" | "+elem[2]+"\t"+arr[3]+" | "+elem[3]+"\t"+arr[4]+"|"+elem[4]+"\t"+arr[5]+"|"+elem[5]+"\t"+arr[6]+" | "+elem[6];/*创建新的key,mutAA，突变类型，COSID,染色体突变座标,核苷酸改变，这五者用“|”号并列起来,没有鉴定到的突变的相应合并代码不在此cpp，而在rchtml.r文件中。另外有一点非常重要!!!!!arr[4]指的是突变id,由于突变id后面是还需要拆分并进行匹配的。因此这里的合并不能用含有空格的" | "，必需用"|"，否则会引起后面的匹配bug.切记！！！！！！！！！！！！！！！！！！！！！！！*/

					img_new[key]=hashDMV[elem[0]]+"\n"+iti->second;   /*新产生的突变信息必需保存到另一个img_new中。因为当前正在循环img，如果此时对其进行添加删除。会引起嵌套循环。产生不可控的结果。*/
					img_new.erase(hashDMK[elem[0]]);/*删除冗余旧值1*/
					img_new.erase(iti->first);/*删除冗余旧值2*/

				}
				else
				{
					img_new[iti->first]=iti->second;
					hashDMK[elem[0]]=iti->first;
					hashDMV[elem[0]]=iti->second;
				}
			}
			/*========同一位点双不同突变且同时被鉴定引起的bug修正结束=====*/

			Rcpp::List iMutList("null");
			for(imut_group::iterator iti=img_new.begin();iti != img_new.end();++iti) 
			{
				//fout<<"@IMUT_START\t"<<iti->first<<std::endl; //~output data object~//
				//fout<<iti->second<<std::endl; //~output data object~//
				iMutList[iti->first]=iti->second;
				//std::cout<<iti->first<<"\n"<<iti->second<<std::endl;
				//fout<<"@IMUT_END"<<std::endl; //~output data object~//
			}

			infoList["IMUT"]=iMutList;

			//fout<<"@UMUT_START"<<std::endl; //~output data object~//
			//fout<<hashUM[it->first]<<std::endl; //~output data object~//
			//fout<<"@UMUT_END"<<std::endl; //~output data object~//
			//fout<<"=END"<<std::endl; //~output data object~//
			//fout<<std::endl; //~output data object~//

			std::vector<std::string> uline;
			split(hashUM[it->first],"\n",&uline);
			Rcpp::List uMutList("null");
			int i=0;
			for(std::vector<std::string>::iterator itu=uline.begin(); itu!=uline.end(); itu++)
			{
				i++;
				uMutList["U"+int2str(i)]=*itu;
			}
			infoList["UMUT"]=uMutList;
			index++;

			std::string tmp="P"+int2str(index);
			proList[tmp]=infoList;
		}
	}
	//fout.close(); //~output data object~//
	return(proList);
}


/*
 * fixed the RECOMMENDED "registering-native-routines" of bioconductor'check.
*/
extern "C"
{
	void R_init_sapFinder(DllInfo *dll)
	{
		/*
		 * Register routines,allocate resources.
		 * Currently we call all of the functions whith .Call
		 */
		R_CallMethodDef callEntries[] = {
			{"dataHandle_Cpp", (DL_FUNC) &dataHandle_Cpp, 3},
			{NULL,NULL,0}
		};
		R_registerRoutines(dll,NULL,callEntries,NULL,NULL);
	}
	void R_upload_sapFinder(DllInfo *dll)
	{
		/* Release resources. */
	}

}
