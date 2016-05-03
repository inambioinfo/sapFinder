$(function() {
	/*~~章节伸缩按钮~~*/
	$(".sectionheader").click(function() {
		$(this).find(".button.contenttoggle").remove();	/*~由于后面用的是prepend，因此需要将".button.contenttoggle"类所在标签先移除~*/
		$(this).next("div.sectionbody").is(":hidden") ? $(this).prepend("<span class=\"button contenttoggle\" title=\"Click to collapse\">-</span>") : $(this).prepend("<span class=\"button contenttoggle\" title=\"Click to collapse\">+</span>") ;	/*~ next('div')获取当前标签的下一div标签，后面为二目操作符，如果是隐藏的则prepend前者，不是则prepend后者。 ~*/
		$(this).next("div.sectionbody").slideToggle();
	});

	/*~~突变位点panel的toggle~~*/
	$(".imut,.umut").click(function(){ /*css选择器中逗号相当于或*/
		//alert($(".active").attr('id'));
		//alert(id=$(this).attr('id'));
		if($(".active").attr('id') == $(this).attr('id'))	/*如果.active所在标签的id与本次点击的id相同，表明此时点击的就是已经打开的突变点的panel，此时的动作应当时直接关闭并移除.active类*/
		{
	    	$(".panel#"+[id=$(this).attr('id')]).slideToggle("slow");/*关闭*/
			$('.active').removeClass('active');  /*设为不激活状态*/
		}
		else	/*其他情况都可以先关闭已打开的panel，然后再打开当前选定的panel*/
		{
		
			$(".active").slideToggle("fast");	/*对于标记为.active类的，肯定是open的panel，用slideToggle快速关闭。*/
			$('.active').removeClass('active'); /*然后将标记为.active类的标签的.active类标志移除，这样以后就不会再激发前面的slideToggle按钮*/
    		$(".panel#"+[id=$(this).attr('id')]).slideToggle("slow");	/*对于满足特定id的panel，缓慢打开*/
			$(".panel#"+[id=$(this).attr('id')]).toggleClass("active"); /*同时将给panel添加.active类标志，以便打开别的面板时能关闭*/
		}
	});

    /*~~突变小气泡提示符~~*/
	$('span[title].mut').qtip({ 
		position: { 
			my: 'bottom left', 
			at: 'top center',
			adjust: {
				x: 0,	/*控制气泡往后偏移*/
				y: 0,
			},
		},
	});
	/*~~谱图大气泡提示符~~*/
    $('img').each(function() {
        // Grab fullsize image src
       var bigSrc = $(this).attr("tipsrc");
        $(this).qtip({
            content: {
                title: '<img src="' + bigSrc + '" alt="" />',
                text: function(api) {
                    // Retrieve content from ALT attribute of the $('.selector') element
                    return $(this).attr('alt');
                }
            },			
        position: {
            my: 'top left',
            at: 'top left',
            target: $(window)
        },
        show: {
		 	/*target: $('span'),*/
			event:'click',
            effect: function() {
                $(this).slideDown();
            }
        },
        hide: {
			event:'unfocus',
            effect: function() {
                $(this).slideUp();
            }
        },
		
		/*
		hide: {
			target: $('img')  
		},
		*/
        style: {
            classes: 'ui-tooltip-light'
            }
        });

   });

});

//####################################
//下面的代码块为go Top按钮的js实现代码
//####################################
var timeout,goTop,scroll_time,clazz;
$(function(){
		goTopEx();
		top_icon();	
		$("div[class^='top_btn']").hover(function(){	//top_botton在hover时高亮
				clazz=$(this).attr("class");		 
				$(this).addClass(clazz + "_hv");
				},function(){
				$(this).removeClass(clazz + "_hv");
				}); 
});
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

