var timeout,hv_obj,goTop,scroll_time,seq_cn_wid,seq_cn_hg,clazz;
$(function(){
		goTopEx();
		top_icon();	
		cnt_hg_adj();
		$(window).scroll(function(){
			cnt_hg_adj();
			});
		$(window).resize(function(){
			cnt_hg_adj();
			});
		$("ul#seq_ul > li > span").click(function(){
			$(this).parent().find("ul").slideToggle("normal").end().siblings().find("span").removeClass("click").end().find("ul").slideUp("normal");
			if($(this).hasClass("click"))
			$(this).removeClass("click");
			else
			$(this).addClass("click");
			});
		$(".nav_ul > li").not(".line").hover(function(){
				hv_obj=this;
				timeout=setTimeout(function(){
					$(hv_obj).addClass("li_hv");
					$(hv_obj).find(".nav_drop").slideDown(300);					
					},300);			
				},function(){
				clearTimeout(timeout);				  
				$(hv_obj).removeClass("li_hv");
				$(hv_obj).find(".nav_drop").slideUp(300);					
				});
		$(".nav_drop li").hover(function(){
				$(this).addClass("li_hv");
				},function(){
				$(this).removeClass("li_hv");
				});
		$(".seq_contentc").click(function(){
				if($(this).hasClass("seq_contentc1")){
				var seq_cn_wid=$(".seq_cn").width() - 247;
				$(this).removeClass("seq_contentc1");
				$(".seq_contentl").css("display","block");
				$(".seq_cn").css("width",seq_cn_wid);			
				}else{
				var seq_cn_wid=$(".seq_cn").width() + 247;
				$(this).addClass("seq_contentc1");
				$(".seq_contentl").css("display","none");
				$(".seq_cn").css("width",seq_cn_wid);			  
				}
				});	
		$(".seq_top").click(function(){
				if($(this).hasClass("seq_top1")){
				var seq_cn_hg=$(".seq_cn").height() - 76 + "px";
				$(this).removeClass("seq_top1");
				$(".header").css("display","block");
				$(".seq_cn").css("height",seq_cn_hg);
				$(".seq_contentl").css("height",seq_cn_hg);			
				}else{
				var seq_cn_hg=$(".seq_cn").height() + 76 + "px";
				$(this).addClass("seq_top1");
				$(".header").css("display","none");
				$(".seq_contentl").css("height",seq_cn_hg);	
				$(".seq_cn").css("height",seq_cn_hg);		
				}
				});
		$("div[class^='top_btn']").hover(function(){
				clazz=$(this).attr("class");		 
				$(this).addClass(clazz + "_hv");
				},function(){
				$(this).removeClass(clazz + "_hv");
				}); 
});
function cnt_hg_adj(){
	var win_wid=$(window).width();
	var win_hg=$(window).height();
	/*
	 * BUG FIX: index页面加载时变形
	 *由于index.css中.seq_contentl和.header的默认的display属性都是block而非hidden的。因此没必要添加下面的这段判定。如果添加了，在当前css设置下，依然可能引起运行else里面的语句而非if括弧里的。（原因暂不明，可能是'$(".seq_contentl").css("display")'这句在载入时本身就不正常。）。试想一个left sidebar开启display，但是减的宽度只是10而非257。整个页面加载后就有可能变形。因此，最简单的办法就是删除本来就不需要的判定，强制以display="block"进行展示。
	 */
	//if($(".seq_contentl").css("display")=="block"){ 
	seq_cn_wid=win_wid - 257 + "px";
	//}else{
	//	seq_cn_wid=win_wid - 10 + "px";	
	//}
	//if($(".header").css("display")=="block"){
	seq_cn_hg=win_hg - 143 + "px";		
	//}else{
	//	seq_cn_hg=win_hg - 67 + "px";	
	//}
	$(".seq_cn").css({"width":seq_cn_wid,"height":seq_cn_hg});
	$(".seq_contentl").css({"height":seq_cn_hg});
}
function goTopEx(){	
	$("div[class^='top_btn']").click(function(){
			goTop=setInterval(scrollMove,10);	        	
			return false;						
			});
}
function top_icon(){
	if(scroll_time)clearTimeout(scroll_time);
	getScrollTop()>0 ? $("div[class^='top_btn']").css("visibility","visible"):$("div[class^='top_btn']").css("visibility","hidden");
	scroll_time=setTimeout("top_icon()",300);
}
function getScrollTop(){	   
	return $(document).scrollTop();
}
function setScrollTop(value){
	$(document).scrollTop(value);
} 
function scrollMove(){
	setScrollTop(getScrollTop()/1.3);
	if(getScrollTop()<1)clearInterval(goTop);
}
