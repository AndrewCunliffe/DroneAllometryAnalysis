
jQuery( document ).ready(function() {
  var dropArea = document.getElementById('rooted');
  var scrollTop = 0;
  var formDrag = document.getElementById('form-drag');
  //var formUrl = document.getElementById('paste_url');
  var formUpload = document.getElementById('upload_url');
  var drop = true;
  var scrollElement = null;

  if(typeof selfpage !== 'undefined' && selfpage.indexOf("details") !== -1){
    scrollElement = jQuery("#zip");
  }else{
    scrollElement = jQuery(document);
  }

  ;['dragenter', 'dragover', 'dragleave', 'drop'].forEach(function(eventName){
    dropArea.addEventListener(eventName, preventDefaults, false)
    document.body.addEventListener(eventName, preventDefaults, false)
  });

  dropArea.addEventListener('dragenter', function(e){

    var uploaderElement = jQuery('#uploader');

    appendError();
    var formReverse = document.getElementById('qbp');
    formReverse.style.display = 'block';
    formDrag.style.display = 'block';
    //formUrl.style.display = 'none';
    formUpload.style.display = 'none';
    console.log(uploaderElement);
    if(!(typeof uploaderElement !== 'undefined' && uploaderElement.length > 0)){
        if(drop === true){
        scrollTop = scrollElement.scrollTop();
        drop = false;
        }

        scrollElement.scrollTop(0);
    }

  }, false);

  dropArea.addEventListener('drop',  function(e) {
    var uploaderElement = jQuery('#uploader');
      var _flagOk = false;
      var formReverse = document.getElementById('qbp');
      var isIE = (navigator.userAgent.indexOf('MSIE')!==-1 || navigator.appVersion.indexOf('Trident/') > -1);
      var isEdge = (window.navigator.userAgent.indexOf("Edge") > -1);
      if(!formReverse.contains(e.target)){
          formReverse.style.display = 'none';
          formDrag.style.display = 'none';
          formUpload.style.display = 'block';

            if(!(typeof uploaderElement !== 'undefined' && uploaderElement.length > 0)){
                scrollElement.scrollTop(scrollTop);
            }
          
          drop = true;
      }else{

          var dt = e.dataTransfer;
          var files = dt.files;

          if(files.length > 0 && validateFileUpload(files[0])){
              _flagOk = true;
              var object = {file : files[0]};
              var img = new Image();

            img.src = window.URL.createObjectURL( object.file );

            img.onload = function() {
                var width = img.naturalWidth,
                    height = img.naturalHeight;

                window.URL.revokeObjectURL( img.src );

                if( width < 7000 && height < 7000 ) {
                    fetchUploadReverseSearch(object);
                }else{
                    appendError(1);
                }
            };
              
          }
          if(!_flagOk && !isIE){

              var dropElement = e.dataTransfer.getData('text/html');
              
              if(dropElement != null && dropElement != ''){
                  _flagOk = true;
                  var imgElement = jQuery(dropElement).find('img');
                  if(imgElement.length > 0){
                    imgElement = imgElement[0];
                  }else{
                    imgElement = jQuery(dropElement).filter('img');
                  }
                  
                  var url = jQuery(imgElement).attr('src');
                  if(!(url.indexOf('.gif') > -1 || url.indexOf('.webp') > -1 )){
                    var object = {image_path : url};
                    formReverse.style.display = 'none';
                    formDrag.style.display = 'none';
                    //formUrl.style.display = 'block';
                    fetchUploadReverseSearch(object);
                  }else{
                    _flagOk = false;
                  }
              }
          }

          if(_flagOk == false){
              drop = true;
              if(isIE){
                appendError(2);
              }else{
                appendError(1);
              }
          }
      }

  }, false);

  jQuery('#reverse_search_mobile').on('change', function(e){
    var files = this.files;
    var _flagOk = false;

    if(files.length > 0 && validateFileUpload(files[0])){
        _flagOk = true;
        var object = {file : files[0]};
        fetchUploadReverseSearch(object);
    }
  });

  jQuery('.srchbar-reverse-pointer img').on('mouseover', function(){
    var isIE = (navigator.userAgent.indexOf('MSIE')!==-1 || navigator.appVersion.indexOf('Trident/') > -1);
    var srcImage = '//static-cdn.123rf.com/images/reverse_icon_black.png';

    if(isIE){
        jQuery(this).attr('src', srcImage);
    }
  });

  jQuery('.srchbar-reverse-pointer img').on('mouseout', function(){
    var isIE = (navigator.userAgent.indexOf('MSIE')!==-1 || navigator.appVersion.indexOf('Trident/') > -1);
    var srcImage = '//static-cdn.123rf.com/images/reverse_icon.png';

    if(isIE){
        jQuery(this).attr('src', srcImage);
    }
  });

  function validateFileUpload(file){
    var ValidImageTypes = ["image/jpeg", "image/png"];
    return (ValidImageTypes.indexOf(file.type) >= 0 && file.size < 5242880)
}

function preventDefaults(e){
  e.preventDefault()
  e.stopPropagation()
}

function appendError (type) {
      var errorElement = document.getElementById('err_msg');
      var htmlApend = '';
    if(type == 1){
        htmlApend = '<ul><li>Image must be JPG / PNG</li> <li>Image size must be less than 5 MB</li></ul>';
    }
    if(type == 2){
        htmlApend = '<ul><li>This Method is not supported in IE</li></ul>';
    }

    if(type == 1 || type == 2){
        formDrag.style.display = 'none';
        formUpload.style.display = 'block';
    }
      errorElement.innerHTML = htmlApend;
      
  }

function fetchUploadReverseSearch(object){
      var data = {};
      var myFormData = new FormData();
      if(typeof object.file !== 'undefined'){

        var img = new Image();

        img.src = window.URL.createObjectURL( object.file );

        img.onload = function() {
            var width = img.naturalWidth,
                height = img.naturalHeight;

            window.URL.revokeObjectURL( img.src );

            if( width < 7000 && height < 7000 ) {
                myFormData.append('file', object.file);
                data = myFormData;
                ajaxUploadReverseSearch(data);
            }

        };
        
      }

      if(typeof object.image_path !== 'undefined'){
          myFormData.append('image_path', object.image_path);
          data = myFormData;
          ajaxUploadReverseSearch(data);
      }

    
  }

  function ajaxUploadReverseSearch(data){
    jQuery.ajax({
        type: 'POST',
        url: '/upload_drag.php',
        data: data,
        cache: false,
        success: function (response) {
            location.href = "http://www.123rf.com/reversesearch/?fid="+response;
        },
        processData: false,
        contentType: false,
    });
  }

});


