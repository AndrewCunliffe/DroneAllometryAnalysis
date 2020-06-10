(function($){
    window.LiveChatPopUp = $(window.jQuery);
})(function($){

    LiveChatPopUp = new Object();
    var is_called = 0;
    var getElement = function(options){
        var element =
        // '<div id="newchat" class="tawkchat" ga-on="click" ga-event-category="Contact" ga-event-label="Live Help" ga-event-action="PopUp Banner">'+
        //     '<div class="livechat-container left">'+
        //         '<div class="livechat-wrapper-1">'+
        //             '<img class="livechat-closebtn" src="'+options.cacheserver+'/images/live-chat-exit-button.png">'+
        //             '<img class="livechat-icon" src="'+options.cacheserver+'/images/live-chat-popup.png">'+
        //         '</div>'+
        //         '<div class="livechat-wrapper-2">'+
        //             '<div class="font-large font-bold aligned left">'+
        //                 'NEED HELP?!'+
        //             '</div>'+
        //             '<div class="font-medium font-light aligned left">'+
        //                 'Live Chat With Us'+
        //             '</div>'+
        //         '</div>'+
        //     '</div>'+
        //     '<div class="livechat-container right" >'+
        //         '<img src="'+options.cacheserver+'/images/live-chat-forward.png">'+
        //     '</div>'+
        // '</div>';


        '<div id="pop-newchat">'+
            '<div class="pop-img-div" >'+
                '<img class="pop-closebtn" src="'+cache_server+'/images/livechat-closebtn.png">'+
                '<img class="pop-image-style" src="'+cache_server+'/images/liveChatHead.jpg" >'+
            '</div>'+
            '<div class ="pop-text-div">';
        if  (is_search_page == 1){
             element = element+'<div class="would-you-like-some-1">'+LANG_LIVECHAT_LOOKINGHELP+'</div>';
        }else{
             element = element+'<div class="looking-for-the-right">'+LANG_LIVECHAT_LOOKING+'</div>'+
                '<div class="would-you-like-some">'+LANG_LIVECHAT_LOOKINGHELP+'</div>';
        }   

        if (lang == "hu" || lang == "jp" || lang == "es" || lang == "de" || lang == "tr"){
            element = element+ 
                '<div id="button-pop" class="button-pop tawkchat" ga-on="click" ga-event-category="Contact" ga-event-label="Live Help" ga-event-action="PopUp Banner" style="width:auto" ><span class="text-button">'+LANG_LIVECHAT_CHATWITHUS+'</span></div>'
            '</div>'+
        '</div>';
        }else{
            element = element+ 
            '<div id="button-pop" class="button-pop tawkchat" ga-on="click" ga-event-category="Contact" ga-event-label="Live Help" ga-event-action="PopUp Banner" ><span class="text-button">'+LANG_LIVECHAT_CHATWITHUS+'</span></div>'
            '</div>'+
        '</div>';
        }
        return element;
    }

    var Popup = function(options){
        this.options = $.extend({}, Popup.defaults, options);
        this.element = getElement(this.options);

        var $Item = this;
        $('body').on("click", "#pop-newchat .pop-closebtn ", function(e){
            e.stopPropagation();
            $Item.close();
        });

        $('body').on("click", "#pop-newchat .tawkchat", function(){
            setTimeout(function(){
                $Item.close();
            }, 2000);
        });
        $('body').on("click","#button-pop",function(){
            $("#pop-newchat").hide();
        });
    }

    Popup.defaults = {
        start_in : 0,
        cacheserver:''
    };

    Popup.prototype.display = function(){
        var $Item = this;
        var start_in = $Item.options.start_in;
        if ( !$('#pop-newchat').length && !is_called ) {
            is_called = 1;
            setTimeout(function(){
                $($Item.element).css({
                    'bottom':'0',
                    'margin-bottom':'-3px',
                    'position':'fixed',
                    'right':'10px',
                    'text-align':'center',
                    'zIndex':'50',
                    'display':'block'
                }).appendTo('body');

                return  is_called = 0;
            }, start_in);
        }
    };

    Popup.prototype.close = function(){
        $('#pop-newchat').remove();
        var futdate = new Date();
        var expdate = futdate.getTime();
        expdate += 14 * 24 * 3600 * 1000 // expires in 14 days (milliseconds)
        futdate.setTime(expdate);
        setCookie('hcs', '1', futdate, '/');
    };

    pop = function(options){
        return (new Popup(options)).display();
    }

    hide = function(options){
        return (new Popup()).close();
    }

    LiveChatPopUp.pop = pop;
    LiveChatPopUp.hide = hide;
    return LiveChatPopUp;
});

