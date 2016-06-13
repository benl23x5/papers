	google_encoding = 'latin1';
        if (window.hint) { google_hints = window.hint; }
	if (!window.google_safe) { google_safe = 'high'; }
	if (!window.google_max_num_ads) { google_max_num_ads = '4'; }
	if (!window.google_ad_output) { google_ad_output = 'js'; }
	if (!window.google_feedback) { google_feedback = 'on'; }
	if (!window.google_image_size) { google_image_size = ostg_ad_width+"x"+ostg_ad_height; }
	if (!window.google_ad_format) { google_ad_format = google_image_size + '_pas_abgnc'; } 
	if (window.sf_proj_home) { google_page_url = sf_proj_home; }
	currWin = window;
	while (currWin.parent != currWin) {
	   currWin = currWin.parent;
	}     
	full_url = ""+window.location;
	if (full_url.match(/slashdot/)) {
		google_ad_section = 'apple askslashdot backslash books developers games hardware interviews it linux politics science yro default';
	}

	function google_ad_request_done(ostg_ads) {
		// defaults
		if (!window.ostg_ad_width) { ostg_ad_width  = '728'; }
		if (!window.ostg_ad_height) { ostg_ad_height  = '90'; }
		if (!window.ostg_color_bg) { ostg_color_bg  = '000000'; }
		if (!window.ostg_table_cellspacing) { ostg_table_cellspacing  = '0'; }
		if (!window.ostg_table_cellpadding) { ostg_table_cellpadding  = '0'; }
		if (!window.ostg_td_style) { ostg_td_style  = 'color: #ffffff; overflow:hidden; font-size:11px; font-family: arial,sans-serif;'; }
		if (!window.ostg_div_style) { ostg_div_style  = 'padding:5px; padding-bottom:0; display: block; float: left;'; }
		if (!window.ostg_color_ga) { ostg_color_ga  = 'ffffff'; }
		if (!window.ostg_ga_style) { ostg_ga_style  = 'color: #ffffff; padding-left: 5px; font-size:11px;'; }
		if (!window.ostg_link_style) { ostg_link_style  = 'color: #ffffff; font-size:12px;'; }
		if (!window.ostg_copydiv_style) { ostg_copydiv_style  = 'cursor: pointer;'; }
		if (!window.ostg_url_style) { ostg_url_style  = 'color: #cccccc;'; }
		if (!window.ostg_div_style_url) { ostg_div_style_url  = 'font-size:11px;'; }

		// Proceed only if we have ads to display!
		if (ostg_ads.length < 1 )
			return;
		
		if (!window.ostg_div_style_extra) { ostg_div_style_extra = ['width: 99%; font-size: 16px;','width: 48%; font-size: 14px;','width: 31%; font-size: 12px;','width: 23%; font-size: 11px;']; }
		if (!window.ostg_link_style_extra) { ostg_link_style_extra = ['font-size:18px;', 'font-size:14px;', 'font-size:12px;', 'font-size:11px;']; }
		if (!window.ostg_url_style_extra) { ostg_url_style_extra = ['font-size: 14px;', 'font-size:12px;', 'font-size:11px;', 'font-size:10px;']; }
		
		ostg_div_style += ostg_div_style_extra[ostg_ads.length - 1];
		ostg_link_style += ostg_link_style_extra[ostg_ads.length - 1];
		ostg_url_style += ostg_url_style_extra[ostg_ads.length - 1];
				
		// Display ads in a table
		document.write("<table height=\"" + ostg_ad_height + "\" width=\"" + (ostg_ad_width+10) + "\" bgcolor=\"#" + ostg_color_bg + "\" cellpadding=\"" + ostg_table_cellpadding + "\" cellspacing=\"" + ostg_table_cellspacing + "\">");
		document.write("<tr><td valign=\"top\" style=\""+ostg_td_style+"\">");
		
		// Print "Ads by Google" -include link to Google feedback page if available
		
		document.write("<div style=\""+ostg_ga_style+"\">");
		if (google_info.feedback_url) {
			document.write("<a target=\"_top\" href=\""+google_info.feedback_url+"\" style=\"color: #"+ostg_color_ga+"\"><B>Ads by Google</B></a>");
		} else {
			document.write("Ads By Google");
		}
		document.write("</div>");
		
		// For text ads, display each ad in turn.
		// In this example, each ad goes in a new row in the table.
		if (ostg_ads[0].type == 'text') {
			for(i = 0; i < ostg_ads.length; ++i) {
				document.write("<div style=\"" + ostg_div_style + "\">" + "<a href=\"" + ostg_ads[i].url + "\" target=\"_blank\" style=\"" + ostg_link_style + "\">" + "<b>" + ostg_ads[i].line1 + "</b></a><br>" + "<div style=\"" + ostg_copydiv_style + "\" onClick=\"window.open(\'"+ostg_ads[i].url+"\')\">" + ostg_ads[i].line2 + " " + ostg_ads[i].line3 + "</div>" + "<div style=\"" + ostg_div_style_url+"\">" + "<a href=\"" + ostg_ads[i].url + "\" target=\"_blank\" style=\"" + ostg_url_style + "\">" + ostg_ads[i].visible_url + "</a></div></div>");
			}
		} else if (ostg_ads[0].type == 'image') {
			document.write("<a target=_top href=\""+ostg_ads[0].url+"\"><img src=\""+ostg_ads[0].image_url+"\" height="+ostg_ads[0].image_height+" width="+ostg_ads[0].image_width+" border=0></a>");
		}
		
		// Finish up anything that needs finishing up
		document.write ("</td></tr></table>");
	}
