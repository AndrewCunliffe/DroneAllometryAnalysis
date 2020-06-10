try {
	var SHRSL_src = document.getElementById("_SHRSL_img_1").src;
	SHRSL_src = SHRSL_src.replace('shareasale.com','shareasale-analytics.com');
	SHRSL_newImage_idmatch = new Image();
	SHRSL_newImage_idmatch.src = SHRSL_src;
}
catch (err) {
	
	var SHRSL_imgarr1 = document.getElementsByTagName("img");
	var SHRSL_imgarr2 = new Array();
	
	for (SHRSL_i=0;SHRSL_i<SHRSL_imgarr1.length;SHRSL_i++) {
		if(SHRSL_imgarr1[SHRSL_i].src.search('shareasale.com/sale.cfm') >= 0) {
			SHRSL_imgarr2.push(SHRSL_imgarr1[SHRSL_i].src);
		}
	}
	
	SHRSL_newImage_idnomatch = new Image();
	
	
	for (SHRSL_j=0;SHRSL_j<SHRSL_imgarr2.length;SHRSL_j++) {
		var SHRSL_src_nomatch = SHRSL_imgarr2[SHRSL_j];
		SHRSL_src_nomatch = SHRSL_src_nomatch.replace('shareasale.com','shareasale-analytics.com');
		SHRSL_newImage_idnomatch.src = SHRSL_src_nomatch;
	}

}