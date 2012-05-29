<?php
{***************************************************************************
* Software: FPDF                                                            *
* Version : 1.41                                                            *
* Date:     2002/03/13                                                      *
* Author:   Olivier PLATHEY                                                 *
* License:  Freeware                                                        *
*                                                                           *
* You may use and modify this software as you wish.                         *
***************************************************************************}
define('FPDF_VERSION','1.41');

class FPDF
begin
//Private properties
var sspage;               //current page number
var ssn;                  //current object number
var ssoffsets;            //array of object offsets
var ssbuffer;             //buffer holding in-memory PDF
var sspages;              //array containing pages
var ssstate;              //current document state
var sscompress;           //compression flag
var ssDefOrientation;     //default orientation
var ssCurOrientation;     //current orientation
var ssOrientationChanges; //array indicating orientation changes
var ssfwPt,ssfhPt;         //dimensions of page format in points
var ssfw,ssfh;             //dimensions of page format in user unit
var sswPt,sshPt;           //current dimensions of page in points
var ssw,ssh;               //current dimensions of page in user unit
var sslMargin;            //left margin
var sstMargin;            //top margin
var ssbMargin;            //page break margin
var sscMargin;            //cell margin
var ssx,ssy;               //current position in user unit for cell positionning
var sslasth;              //height of last cell printed
var ssk;                  //scale factor (number of points in user unit)
var ssLineWidth;          //line width in user unit
var ssfontnames;          //array of Postscript (Type1) font names
var ssfonts;              //array of used fonts
var ssimages;             //array of used images
var ssFontFamily;         //current font family
var ssFontStyle;          //current font style
var ssFontSizePt;         //current font size in points
var ssFontSize;           //current font size in user unit
var ssDrawColor;          //commands for drawing color
var ssFillColor;          //commands for filling color
var ssTextColor;          //commands for text color
var ssColorFlag;          //indicates whether fill and text colors are different
var ssws;                 //word spacing
var ssAutoPageBreak;      //automatic page breaking
var ssPageBreakTrigger;   //threshold used to trigger page breaks
var ssInFooter;           //flag set when processing footer
var ssDisplayMode;        //display mode
var sstitle;              //title
var sssubject;            //subject
var ssauthor;             //author
var sskeywords;           //keywords
var sscreator;            //creator
var ssAliasNbPages;       //alias for total number of pages

{***************************************************************************
*                                                                           *
*                              Public methods                               *
*                                                                           *
***************************************************************************}
procedure TJPFpdf.FPDF(ssorientation := 'P',ssunit := 'mm',ssformat := 'A4');
begin
	//Initialization of properties
	Self.sspage := 0;
	Self.ssn := 2;
	Self.ssbuffer := '';
	Self.sspages := array();
	Self.ssOrientationChanges := array();
	Self.ssstate := 0;
	Self.ssfonts := array();
	Self.ssimages := array();
	Self.ssInFooter := false;
	Self.ssFontFamily := '';
	Self.ssFontStyle := '';
	Self.ssFontSizePt := 12;
	Self.ssDrawColor := '0 G';
	Self.ssFillColor := '0 g';
	Self.ssTextColor := '0 g';
	Self.ssColorFlag := false;
	Self.ssws := 0;
	//Font names
	Self.ssfontnames['courier'] := 'Courier';
	Self.ssfontnames['courierB'] := 'Courier-Bold';
	Self.ssfontnames['courierI'] := 'Courier-Oblique';
	Self.ssfontnames['courierBI'] := 'Courier-BoldOblique';
	Self.ssfontnames['helvetica'] := 'Helvetica';
	Self.ssfontnames['helveticaB'] := 'Helvetica-Bold';
	Self.ssfontnames['helveticaI'] := 'Helvetica-Oblique';
	Self.ssfontnames['helveticaBI'] := 'Helvetica-BoldOblique';
	Self.ssfontnames['times'] := 'Times-Roman';
	Self.ssfontnames['timesB'] := 'Times-Bold';
	Self.ssfontnames['timesI'] := 'Times-Italic';
	Self.ssfontnames['timesBI'] := 'Times-BoldItalic';
	Self.ssfontnames['symbol'] := 'Symbol';
	Self.ssfontnames['zapfdingbats'] := 'ZapfDingbats';
	//Scale factor
	if(ssunit = 'pt') then 
		Self.ssk := 1;
	else if(ssunit = 'mm') then 
		Self.ssk := 72/25.4;
	else if(ssunit = 'cm') then 
		Self.ssk := 72/2.54;
	else if(ssunit = 'in') then 
		Self.ssk := 72;
	else
		Self.ssError('Incorrect unit: ' + ssunit);
	//Page format
	if(is_string(ssformat)) then 
	begin
		ssformat := strtolower(ssformat);
		if(ssformat = 'a3') then 
			ssformat := array(841.89,1190.55);
		else if(ssformat = 'a4') then 
			ssformat := array(595.28,841.89);
		else if(ssformat = 'a5') then 
			ssformat := array(420.94,595.28);
		else if(ssformat = 'letter') then 
			ssformat := array(612,792);
		else if(ssformat = 'legal') then 
			ssformat := array(612,1008);
		else
			Self.ssError('Unknown page format: ' + ssformat);
		Self.ssfwPt := ssformat[0];
		Self.ssfhPt := ssformat[1];
	end
	else
	begin
		Self.ssfwPt := round(ssformat[0]*Self.ssk,2);
		Self.ssfhPt := round(ssformat[1]*Self.ssk,2);
	end;
	Self.ssfw := round(Self.ssfwPt/Self.ssk,2);
	Self.ssfh := round(Self.ssfhPt/Self.ssk,2);
	//Page orientation
	ssorientation := strtolower(ssorientation);
	if(ssorientation = 'p' or ssorientation = 'portrait') then 
	begin
		Self.ssDefOrientation := 'P';
		Self.sswPt := Self.ssfwPt;
		Self.sshPt := Self.ssfhPt;
	end
	else if(ssorientation = 'l' or ssorientation = 'landscape') then 
	begin
		Self.ssDefOrientation := 'L';
		Self.sswPt := Self.ssfhPt;
		Self.sshPt := Self.ssfwPt;
	end
	else
		Self.ssError('Incorrect orientation: ' + ssorientation);
	Self.ssCurOrientation := Self.ssDefOrientation;
	Self.ssw := round(Self.sswPt/Self.ssk,2);
	Self.ssh := round(Self.sshPt/Self.ssk,2);
	//Page margins (1 cm)
	ssmargin := round(28.35/Self.ssk,2);
	Self.ssSetMargins(ssmargin,ssmargin);
	//Interior cell margin (1 mm)
	Self.sscMargin := ssmargin/10;
	//Line width (0.2 mm)
	Self.ssLineWidth := round(.567/Self.ssk,3);
	//Automatic page break
	Self.ssSetAutoPageBreak(true,2*ssmargin);
	//Full width display mode
	Self.ssSetDisplayMode('fullwidth');
	//Compression
	Self.ssSetCompression(true);
end;

procedure TJPFpdf.SetMargins(ssleft,sstop);
begin
	//Set left and top margins
	Self.sslMargin := ssleft;
	Self.sstMargin := sstop;
end;

procedure TJPFpdf.SetLeftMargin(ssmargin);
begin
	//Set left margin
	Self.sslMargin := ssmargin;
end;

procedure TJPFpdf.SetAutoPageBreak(ssauto,ssmargin := 0);
begin
	//Set auto page break mode and triggering margin
	Self.ssAutoPageBreak := ssauto;
	Self.ssbMargin := ssmargin;
	Self.ssPageBreakTrigger := Self.ssh-ssmargin;
end;

procedure TJPFpdf.SetDisplayMode(ssmode,ssz := 100);
begin
	//Set display mode in viewer
	if(ssmode = 'fullpage' or ssmode = 'fullwidth' or ssmode = 'real' or ssmode = 'default') then 
		Self.ssDisplayMode := ssmode;
	else if(ssmode = 'zoom') then 
		Self.ssDisplayMode := ssz;
	else
		Self.ssError('Incorrect display mode: ' + ssmode);
end;

procedure TJPFpdf.SetCompression(sscompress);
begin
	//Set page compression
	if(function_exists('gzcompress')) then 
		Self.sscompress := sscompress;
	else
		Self.sscompress := false;
end;

procedure TJPFpdf.SetTitle(sstitle);
begin
	//Title of document
	Self.sstitle := sstitle;
end;

procedure TJPFpdf.SetSubject(sssubject);
begin
	//Subject of document
	Self.sssubject := sssubject;
end;

procedure TJPFpdf.SetAuthor(ssauthor);
begin
	//Author of document
	Self.ssauthor := ssauthor;
end;

procedure TJPFpdf.SetKeywords(sskeywords);
begin
	//Keywords of document
	Self.sskeywords := sskeywords;
end;

procedure TJPFpdf.SetCreator(sscreator);
begin
	//Creator of document
	Self.sscreator := sscreator;
end;

procedure TJPFpdf.AliasNbPages(ssalias := 'beginnbend;');
begin
	//Define an alias for total number of pages
	Self.ssAliasNbPages := ssalias;
end;

procedure TJPFpdf.Error(ssmsg);
begin
	//Fatal error
	die('<B>FPDF error: </B>' + ssmsg);
end;

procedure TJPFpdf.Open();
begin
	//Begin document
	Self.ss_begindoc();
end;

procedure TJPFpdf.Close();
begin
	//Terminate document
	if(sspage = Self.sspage = 0) then 
		Self.ssError('Document contains no page');
	//Page footer
	Self.ssInFooter := true;
	Self.ssFooter();
	Self.ssInFooter := false;
	//Close page
	Self.ss_endpage();
	//Close document
	Self.ss_enddoc();
end;

procedure TJPFpdf.AddPage(ssorientation := '');
begin
	//Start a new page
	ssfamily := Self.ssFontFamily;
	ssstyle := Self.ssFontStyle;
	sssize := Self.ssFontSizePt;
	sslw := Self.ssLineWidth;
	ssdc := Self.ssDrawColor;
	ssfc := Self.ssFillColor;
	sstc := Self.ssTextColor;
	sscf := Self.ssColorFlag;
	if(Self.sspage>0) then 
	begin
		//Page footer
		Self.ssInFooter := true;
		Self.ssFooter();
		Self.ssInFooter := false;
		//Close page
		Self.ss_endpage();
	end;
	//Start new page
	Self.ss_beginpage(ssorientation);
	//Set line cap style to square
	Self.ss_out('2 J');
	//Set line width
	Self.ss_out(sslw + ' w');
	//Set font
	if(ssfamily) then 
		Self.ssSetFont(ssfamily,ssstyle,sssize);
	//Set colors
	if(ssdc <> '0 G') then 
		Self.ss_out(ssdc);
	if(ssfc <> '0 g') then 
		Self.ss_out(ssfc);
	Self.ssTextColor := sstc;
	Self.ssColorFlag := sscf;
	//Page header
	Self.ssHeader();
	//Restore line width
	if(Self.ssLineWidth <> sslw) then 
	begin
		Self.ssLineWidth := sslw;
		Self.ss_out(sslw + ' w');
	end;
	//Restore font
	if(ssfamily) then 
		Self.ssSetFont(ssfamily,ssstyle,sssize);
	//Restore colors
	if(Self.ssDrawColor <> ssdc) then 
	begin
		Self.ssDrawColor = ssdc;
		Self.ss_out(ssdc);
	end;
	if(Self.ssFillColor <> ssfc) then 
	begin
		Self.ssFillColor = ssfc;
		Self.ss_out(ssfc);
	end;
	Self.ssTextColor := sstc;
	Self.ssColorFlag := sscf;
end;

procedure TJPFpdf.Header();
begin
	//To be implemented in your own inherited class
end;

procedure TJPFpdf.Footer();
begin
	//To be implemented in your own inherited class
end;

procedure TJPFpdf.PageNo();
begin
	//Get current page number
	return Self.sspage;
end;

procedure TJPFpdf.SetDrawColor(ssr,ssg := -1,ssb := -1);
begin
	//Set color for all stroking operations
	if((ssr = 0 and ssg = 0 and ssb = 0) or ssg = -1) then 
		Self.ssDrawColor := substr(ssr/255,0,5) + ' G';
	else
		Self.ssDrawColor := substr(ssr/255,0,5) + ' ' + substr(ssg/255,0,5) + ' ' + substr(ssb/255,0,5) + ' RG';
	if(Self.sspage>0) then 
		Self.ss_out(Self.ssDrawColor);
end;

procedure TJPFpdf.SetFillColor(ssr,ssg := -1,ssb := -1);
begin
	//Set color for all filling operations
	if((ssr = 0 and ssg = 0 and ssb = 0) or ssg = -1) then 
		Self.ssFillColor := substr(ssr/255,0,5) + ' g';
	else
		Self.ssFillColor := substr(ssr/255,0,5) + ' ' + substr(ssg/255,0,5) + ' ' + substr(ssb/255,0,5) + ' rg';
	Self.ssColorFlag := (Self.ssFillColor <> Self.ssTextColor);
	if(Self.sspage>0) then 
		Self.ss_out(Self.ssFillColor);
end;

procedure TJPFpdf.SetTextColor(ssr,ssg := -1,ssb := -1);
begin
	//Set color for text
	if((ssr = 0 and ssg = 0 and ssb = 0) or ssg = -1) then 
		Self.ssTextColor := substr(ssr/255,0,5) + ' g';
	else
		Self.ssTextColor := substr(ssr/255,0,5) + ' ' + substr(ssg/255,0,5) + ' ' + substr(ssb/255,0,5) + ' rg';
	Self.ssColorFlag := (Self.ssFillColor <> Self.ssTextColor);
end;

procedure TJPFpdf.GetStringWidth(sss);
begin
	//Get width of a string in the current font
	global ssfpdf_charwidths;

	sscw := &ssfpdf_charwidths[Self.ssFontFamily.Self.ssFontStyle];
	ssw := 0;
	ssl := strlen(sss);
	for(ssi := 0;ssi<ssl;ssi++) do 
		ssw += sscw[sss[ssi]];
	return ssw*Self.ssFontSize/1000;
end;

procedure TJPFpdf.SetLineWidth(sswidth);
begin
	//Set line width
	Self.ssLineWidth := sswidth;
	if(Self.sspage>0) then 
		Self.ss_out(sswidth + ' w');
end;

procedure TJPFpdf.Line(ssx1,ssy1,ssx2,ssy2);
begin
	//Draw a line
	Self.ss_out(ssx1 + ' -' + ssy1 + ' m ' + ssx2 + ' -' + ssy2 + ' l S');
end;

procedure TJPFpdf.Rect(ssx,ssy,ssw,ssh,ssstyle := '');
begin
	//Draw a rectangle
	if(ssstyle = 'F') then 
		ssop := 'f';
	else if(ssstyle = 'FD' or ssstyle = 'DF') then 
		ssop := 'B';
	else
		ssop := 'S';
	Self.ss_out(ssx + ' -' + ssy + ' ' + ssw + ' -' + ssh + ' re ' + ssop);
end;

procedure TJPFpdf.SetFont(ssfamily,ssstyle := '',sssize := 0);
begin
	//Select a font; size given in points
	if(not Self.ss_setfont(ssfamily,ssstyle,sssize)) then 
		Self.ssError('Incorrect font family or style: ' + ssfamily + ' ' + ssstyle);
end;

procedure TJPFpdf.SetFontSize(sssize);
begin
	//Set font size in points
	Self.ss_setfontsize(sssize);
end;

procedure TJPFpdf.Text(ssx,ssy,sstxt);
begin
	//Output a string
	sstxt := str_replace(')','\\)',str_replace('(','\\(',str_replace('\\','\\\\',sstxt)));
	sss := 'BT ' + ssx + ' -' + ssy + ' Td (' + sstxt + ') Tj ET';
	if(Self.ssColorFlag) then 
		sss := 'q ' + Self.ssTextColor + ' ' + sss + ' Q';
	Self.ss_out(sss);
end;

procedure TJPFpdf.AcceptPageBreak();
begin
	//Accept automatic page break or not
	return Self.ssAutoPageBreak;
end;

procedure TJPFpdf.Cell(ssw,ssh := 0,sstxt := '',ssborder := 0,ssln := 0,ssalign := '',ssfill := 0);
begin
	//Output a cell
	if(Self.ssy+ssh>Self.ssPageBreakTrigger and not Self.ssInFooter and Self.ssAcceptPageBreak()) then 
	begin
		ssx := Self.ssx;
		ssws := Self.ssws;
		if(ssws>0) then 
			Self.ss_out('0 Tw');
		Self.ssAddPage(Self.ssCurOrientation);
		Self.ssx := ssx;
		if(ssws>0) then 
			Self.ss_out(ssws + ' Tw');
	end;
	if(ssw = 0) then 
		ssw := Self.ssw-Self.sslMargin-Self.ssx;
	sss := '';
	if(ssfill = 1 or ssborder = 1) then 
	begin
		sss  +=  Self.ssx + ' -' + Self.ssy + ' ' + ssw + ' -' + ssh + ' re ';
		if(ssfill = 1) then 
			sss  +=  (ssborder = 1) ? 'B ' : 'f ';
		else
			sss  +=  'S ';
	end;
	if(is_string(ssborder)) then 
	begin
		ssx := Self.ssx;
		ssy := Self.ssy;
		if(is_int(strpos(ssborder,'L'))) then 
			sss  +=  ssx + ' -' + ssy + ' m ' + ssx + ' -' + (ssy+ssh) + ' l S ';
		if(is_int(strpos(ssborder,'T'))) then 
			sss  +=  ssx + ' -' + ssy + ' m ' + (ssx+ssw) + ' -' + ssy + ' l S ';
		if(is_int(strpos(ssborder,'R'))) then 
			sss  +=  (ssx+ssw) + ' -' + ssy + ' m ' + (ssx+ssw) + ' -' + (ssy+ssh) + ' l S ';
		if(is_int(strpos(ssborder,'B'))) then 
			sss  +=  ssx + ' -' + (ssy+ssh) + ' m ' + (ssx+ssw) + ' -' + (ssy+ssh) + ' l S ';
	end;
	if(sstxt <> '') then 
	begin
		if(ssalign = 'R') then 
			ssdx := ssw-Self.sscMargin-Self.ssGetStringWidth(sstxt);
		else if(ssalign = 'C') then 
			ssdx := (ssw-Self.ssGetStringWidth(sstxt))/2;
		else
			ssdx := Self.sscMargin;
		sstxt := str_replace(')','\\)',str_replace('(','\\(',str_replace('\\','\\\\',sstxt)));
		if(Self.ssColorFlag) then 
			sss  +=  'q ' + Self.ssTextColor + ' ';
		sss  +=  'BT ' + (Self.ssx+ssdx) + ' -' + (Self.ssy+.5*ssh+.3*Self.ssFontSize) + ' Td (' + sstxt + ') Tj ET';
		if(Self.ssColorFlag) then 
			sss  +=  ' Q';
	end;
	if(sss) then 
		Self.ss_out(sss);
	Self.sslasth := ssh;
	if(ssln>0) then 
	begin
		//Go to next line
		Self.ssy += ssh;
		if(ssln = 1) then 
			Self.ssx := Self.sslMargin;
	end
	else
		Self.ssx += ssw;
end;

procedure TJPFpdf.MultiCell(ssw,ssh,sstxt,ssborder := 0,ssalign := 'J',ssfill := 0);
begin
	//Output text with automatic or explicit line breaks
	global ssfpdf_charwidths;

	sscw := &ssfpdf_charwidths[Self.ssFontFamily.Self.ssFontStyle];
	if(ssw = 0) then 
		ssw := Self.ssw-Self.sslMargin-Self.ssx;
	sswmax := (ssw-2*Self.sscMargin)*1000/Self.ssFontSize;
	sss := str_replace("\r",'',sstxt);
	ssnb := strlen(sss);
	if(ssnb>0 and sss[ssnb-1] = "\n") then 
		ssnb--;
	sssep := -1;
	ssi := 0;
	ssj := 0;
	ssl := 0;
	ssns := 0;
	ssnl := 1;
	while(ssi<ssnb) do
	begin
		//Get next character
		ssc := sss[ssi];
		if(ssc = "\n") then 
		begin
			//Explicit line break
			if(ssalign = 'J') then 
			begin
				Self.ssws := 0;
				Self.ss_out('0 Tw');
			end;
			Self.ssCell(ssw,ssh,substr(sss,ssj,ssi-ssj),0,2,ssalign,ssfill);
			ssi++;
			sssep := -1;
			ssj := ssi;
			ssl := 0;
			ssns := 0;
			if(ssborder) then 
			begin
				ssx := Self.ssx;
				ssy := Self.ssy-ssh;
				if(ssnl = 1) then 
					Self.ss_out(ssx + ' -' + ssy + ' m ' + (ssx+ssw) + ' -' + ssy + ' l');
				Self.ss_out(ssx + ' -' + ssy + ' m ' + ssx + ' -' + Self.ssy + ' l ' + (ssx+ssw) + ' -' + ssy + ' m ' + (ssx+ssw) + ' -' + Self.ssy + ' l S');
			end;
			ssnl++;
			continue;
		end;
		if(ssc = ' ') then 
		begin
			sssep := ssi;
			ssls := ssl;
			ssns++;
		end;
		ssl += sscw[ssc];
		if(ssl>sswmax) then 
		begin
			//Automatic line break
			if(sssep = -1) then 
			begin
				if(ssi = ssj) then 
					ssi++;
				if(ssalign = 'J') then 
				begin
					Self.ssws := 0;
					Self.ss_out('0 Tw');
				end;
				Self.ssCell(ssw,ssh,substr(sss,ssj,ssi-ssj),0,2,ssalign,ssfill);
			end
			else
			begin
				if(ssalign = 'J') then 
				begin
					Self.ssws := (ssns>1) ? round((sswmax-ssls)/1000*Self.ssFontSize/(ssns-1),3) : 0;
					Self.ss_out(Self.ssws + ' Tw');
				end;
				Self.ssCell(ssw,ssh,substr(sss,ssj,sssep-ssj),0,2,ssalign,ssfill);
				ssi := sssep+1;
			end;
			sssep := -1;
			ssj := ssi;
			ssl := 0;
			ssns := 0;
			if(ssborder) then 
			begin
				ssx := Self.ssx;
				ssy := Self.ssy-ssh;
				if(ssnl = 1) then 
					Self.ss_out(ssx + ' -' + ssy + ' m ' + (ssx+ssw) + ' -' + ssy + ' l');
				Self.ss_out(ssx + ' -' + ssy + ' m ' + ssx + ' -' + Self.ssy + ' l ' + (ssx+ssw) + ' -' + ssy + ' m ' + (ssx+ssw) + ' -' + Self.ssy + ' l S');
			end;
			ssnl++;
		end
		else
			ssi++;
	end;
	//Last chunk
	if(ssalign = 'J') then 
	begin
		Self.ssws := 0;
		Self.ss_out('0 Tw');
	end;
	Self.ssCell(ssw,ssh,substr(sss,ssj,ssi),0,2,ssalign,ssfill);
	if(ssborder) then 
	begin
		ssx := Self.ssx;
		ssy := Self.ssy-ssh;
		if(ssnl = 1) then 
			Self.ss_out(ssx + ' -' + ssy + ' m ' + (ssx+ssw) + ' -' + ssy + ' l');
		Self.ss_out(ssx + ' -' + ssy + ' m ' + ssx + ' -' + Self.ssy + ' l ' + (ssx+ssw) + ' -' + Self.ssy + ' l ' + (ssx+ssw) + ' -' + ssy + ' l S');
	end;
	Self.ssx := Self.sslMargin;
end;

procedure TJPFpdf.Image(ssfile,ssx,ssy,ssw,ssh := 0,sstype := '')
begin
	//Put an image on the page
	if(not isset(Self.ssimages[ssfile])) then 
	begin
		//First use of image, get info
		if(sstype = '') then 
		begin
			sspos := strrpos(ssfile,'.');
			if(not sspos) then 
				Self.ssError('Image file has no extension and no type was specified: ' + ssfile);
			sstype := substr(ssfile,sspos+1);
		end;
		sstype := strtolower(sstype);
		ssmqr := get_magic_quotes_runtime();
		set_magic_quotes_runtime(0);
		if(sstype = 'jpg' or sstype = 'jpeg') then 
			ssinfo := Self.ss_parsejpg(ssfile);
		else if(sstype = 'png') then 
			ssinfo := Self.ss_parsepng(ssfile);
		else
			Self.ssError('Unsupported image file type: ' + sstype);
		set_magic_quotes_runtime(ssmqr);
		ssinfo['n'] := count(Self.ssimages)+1;
		Self.ssimages[ssfile] := ssinfo;
	end
	else
		ssinfo := Self.ssimages[ssfile];
	//Automatic width or height calculus
	if(ssw = 0) then 
		ssw := round(ssh*ssinfo['w']/ssinfo['h'],2);
	if(ssh = 0) then 
		ssh := round(ssw*ssinfo['h']/ssinfo['w'],2);
	Self.ss_out('q ' + ssw + ' 0 0 ' + ssh + ' ' + ssx + ' -' + (ssy+ssh) + ' cm /I' + ssinfo['n'] + ' Do Q');
end;

procedure TJPFpdf.Ln(ssh := '');
begin
	//Line feed; default value is last cell height
	Self.ssx := Self.sslMargin;
	if(is_string(ssh)) then 
		Self.ssy += Self.sslasth;
	else
		Self.ssy += ssh;
end;

procedure TJPFpdf.GetX();
begin
	//Get x position
	return Self.ssx;
end;

procedure TJPFpdf.SetX(ssx);
begin
	//Set x position
	if(ssx >= 0) then 
		Self.ssx := ssx;
	else
		Self.ssx := Self.ssw+ssx;
end;

procedure TJPFpdf.GetY();
begin
	//Get y position
	return Self.ssy;
end;

procedure TJPFpdf.SetY(ssy);
begin
	//Set y position and reset x
	Self.ssx := Self.sslMargin;
	if(ssy >= 0) then 
		Self.ssy := ssy;
	else
		Self.ssy := Self.ssh+ssy;
end;

procedure TJPFpdf.SetXY(ssx,ssy);
begin
	//Set x and y positions
	Self.ssSetY(ssy);
	Self.ssSetX(ssx);
end;

procedure TJPFpdf.Output(ssfile := '',ssdownload := false);
begin
	//Output PDF to file or browser
	global ssHTTP_ENV_VARS;

	if(Self.ssstate<3) then 
		Self.ssClose();
	if(ssfile = '') then 
	begin
		//Send to browser
		Header('Content-Type: application/pdf');
		if(headers_sent()) then 
			Self.ssError('Some data has already been output to browser, can\'t send PDF file');
		Header('Content-Length: ' + strlen(Self.ssbuffer));
		Header('Content-disposition: inline; filename := doc.pdf');
		echo Self.ssbuffer;
	end
	else
	begin
		if(ssdownload) then 
		begin
			//Download file
			if(isset(ssHTTP_ENV_VARS['HTTP_USER_AGENT']) and strpos(ssHTTP_ENV_VARS['HTTP_USER_AGENT'],'MSIE 5.5')) then 
				Header('Content-Type: application/dummy');
			else
				Header('Content-Type: application/octet-stream');
			if(headers_sent()) then 
				Self.ssError('Some data has already been output to browser, can\'t send PDF file');
			Header('Content-Length: ' + strlen(Self.ssbuffer));
			Header('Content-disposition: attachment; filename := ' + ssfile);
			echo Self.ssbuffer;
		end
		else
		begin
			//Save file locally
			ssf := fopen(ssfile,'wb');
			if(not ssf) then 
				Self.ssError('Unable to create output file: ' + ssfile);
			fwrite(ssf,Self.ssbuffer,strlen(Self.ssbuffer));
			fclose(ssf);
		end;
	end;
end;

{***************************************************************************
*                                                                           *
*                              Private methods                              *
*                                                                           *
***************************************************************************}
procedure TJPFpdf._begindoc();
begin
	//Start document
	Self.ssstate := 1;
	Self.ss_out('%PDF-1.3');
end;
 
procedure TJPFpdf._enddoc();
begin
	//Terminate document
	ssnb := Self.sspage;
	if(not empty(Self.ssAliasNbPages)) then 
	begin
		//Replace number of pages
		for(ssn := 1;ssn <= ssnb;ssn++) do
			Self.sspages[ssn] := str_replace(Self.ssAliasNbPages,ssnb,Self.sspages[ssn]);
	end;
	if(Self.ssDefOrientation = 'P') then 
	begin
		sswPt := Self.ssfwPt;
		sshPt := Self.ssfhPt;
	end
	else
	begin
		sswPt := Self.ssfhPt;
		sshPt := Self.ssfwPt;
	end;
	ssfilter := (Self.sscompress) ? '/Filter /FlateDecode ' : '';
	for(ssn := 1;ssn<= ssnb;ssn++) do
	begin
		//Page
		Self.ss_newobj();
		Self.ss_out('<</Type /Page');
		Self.ss_out('/Parent 1 0 R');
		if(isset(Self.ssOrientationChanges[ssn])) then 
			Self.ss_out('/MediaBox [0 0 ' + sshPt + ' ' + sswPt + ']');
		Self.ss_out('/Resources 2 0 R');
		Self.ss_out('/Contents ' + (Self.ssn+1) + ' 0 R>>');
		Self.ss_out('endobj');
		//Page content
		ssp := (Self.sscompress) ? gzcompress(Self.sspages[ssn]) : Self.sspages[ssn];
		Self.ss_newobj();
		Self.ss_out('<<' + ssfilter + '/Length ' + strlen(ssp) + '>>');
		Self.ss_out('stream');
		Self.ss_out(ssp + 'endstream');
		Self.ss_out('endobj');
	end;
	//Fonts
	ssnf := Self.ssn;
	reset(Self.ssfonts);
	while(list(ssname) = each(Self.ssfonts)) do
	begin
		Self.ss_newobj();
		Self.ss_out('<</Type /Font');
		Self.ss_out('/Subtype /Type1');
		Self.ss_out('/BaseFont /' + ssname);
		if(ssname <> 'Symbol' and ssname <> 'ZapfDingbats') then 
			Self.ss_out('/Encoding /WinAnsiEncoding');
		Self.ss_out('>>');
		Self.ss_out('endobj');
	end;
	//Images
	ssni := Self.ssn;
	reset(Self.ssimages);
	while(list(ssfile,ssinfo) = each(Self.ssimages)) do
	begin
		Self.ss_newobj();
		Self.ss_out('<</Type /XObject');
		Self.ss_out('/Subtype /Image');
		Self.ss_out('/Width ' + ssinfo['w']);
		Self.ss_out('/Height ' + ssinfo['h']);
		if(ssinfo['cs'] = 'Indexed') then 
			Self.ss_out('/ColorSpace [/Indexed /DeviceRGB ' + (strlen(ssinfo['pal'])/3-1) + ' ' + (Self.ssn+1) + ' 0 R]');
		else
			Self.ss_out('/ColorSpace /' + ssinfo['cs']);
		Self.ss_out('/BitsPerComponent ' + ssinfo['bpc']);
		Self.ss_out('/Filter /' + ssinfo['f']);
		if(isset(ssinfo['parms'])) then 
			Self.ss_out(ssinfo['parms']);
		if(isset(ssinfo['trns']) and is_array(ssinfo['trns'])) then 
		begin
			sstrns := '';
			for(ssi := 0;ssi<count(ssinfo['trns']);ssi++) do 
				sstrns  +=  ssinfo['trns'][ssi] + ' ' + ssinfo['trns'][ssi] + ' ';
			Self.ss_out('/Mask [' + sstrns + ']');
		end;
		Self.ss_out('/Length ' + strlen(ssinfo['data']) + '>>');
		Self.ss_out('stream');
		Self.ss_out(ssinfo['data']);
		Self.ss_out('endstream');
		Self.ss_out('endobj');
		//Palette
		if(ssinfo['cs'] = 'Indexed') then 
		begin
			Self.ss_newobj();
			Self.ss_out('<</Length ' + strlen(ssinfo['pal']) + '>>');
			Self.ss_out('stream');
			Self.ss_out(ssinfo['pal']);
			Self.ss_out('endstream');
			Self.ss_out('endobj');
		end;
	end;
	//Pages root
	Self.ssoffsets[1] := strlen(Self.ssbuffer);
	Self.ss_out('1 0 obj');
	Self.ss_out('<</Type /Pages');
	sskids := '/Kids [';
	for(ssi := 0;ssi<Self.sspage;ssi++) do 
		sskids  +=  (3+2*ssi) + ' 0 R ';
	Self.ss_out(sskids + ']');
	Self.ss_out('/Count ' + Self.sspage);
	Self.ss_out('/MediaBox [0 0 ' + sswPt + ' ' + sshPt + ']');
	Self.ss_out('>>');
	Self.ss_out('endobj');
	//Resources
	Self.ssoffsets[2] := strlen(Self.ssbuffer);
	Self.ss_out('2 0 obj');
	Self.ss_out('<</ProcSet [/PDF /Text /ImageB /ImageC /ImageI]');
	Self.ss_out('/Font <<');
	for(ssi := 1;ssi <= count(Self.ssfonts);ssi++) do 
		Self.ss_out('/F' + ssi + ' ' + (ssnf+ssi) + ' 0 R');
	Self.ss_out('>>');
	if(count(Self.ssimages)) then 
	begin
		Self.ss_out('/XObject <<');
		ssnbpal := 0;
		reset(Self.ssimages);
		while(list(,ssinfo) = each(Self.ssimages)) do
		begin
			Self.ss_out('/I' + ssinfo['n'] + ' ' + (ssni+ssinfo['n']+ssnbpal) + ' 0 R');
			if(ssinfo['cs'] = 'Indexed') then 
				ssnbpal++;
		end;
		Self.ss_out('>>');
	end;
	Self.ss_out('>>');
	Self.ss_out('endobj');
	//Info
	Self.ss_newobj();
	Self.ss_out('<</Producer (FPDF ' + FPDF_VERSION + ')');
	if(not empty(Self.sstitle)) then 
		Self.ss_out('/Title (' + Self.ss_escape(Self.sstitle) + ')');
	if(not empty(Self.sssubject)) then 
		Self.ss_out('/Subject (' + Self.ss_escape(Self.sssubject) + ')');
	if(not empty(Self.ssauthor)) then 
		Self.ss_out('/Author (' + Self.ss_escape(Self.ssauthor) + ')');
	if(not empty(Self.sskeywords)) then 
		Self.ss_out('/Keywords (' + Self.ss_escape(Self.sskeywords) + ')');
	if(not empty(Self.sscreator)) then 
		Self.ss_out('/Creator (' + Self.ss_escape(Self.sscreator) + ')');
	Self.ss_out('/CreationDate (D:' + date('YmdHis') + ')>>');
	Self.ss_out('endobj');
	//Catalog
	Self.ss_newobj();
	Self.ss_out('<</Type /Catalog');
	if(Self.ssDisplayMode = 'fullpage') then 
		Self.ss_out('/OpenAction [3 0 R /Fit]');
	else if(Self.ssDisplayMode = 'fullwidth') then 
		Self.ss_out('/OpenAction [3 0 R /FitH null]');
	else if(Self.ssDisplayMode = 'real') then 
		Self.ss_out('/OpenAction [3 0 R /XYZ null null 1]');
	else
		Self.ss_out('/OpenAction [3 0 R /XYZ null null ' + (Self.ssDisplayMode/100) + ']');
	Self.ss_out('/Pages 1 0 R>>');
	Self.ss_out('endobj');
	//Cross-ref
	sso := strlen(Self.ssbuffer);
	Self.ss_out('xref');
	Self.ss_out('0 ' + (Self.ssn+1));
	Self.ss_out('0000000000 65535 f ');
	for(ssi := 1;ssi <= Self.ssn;ssi++) do
		Self.ss_out(sprintf('%010d 00000 n ',Self.ssoffsets[ssi]));
	//Trailer
	Self.ss_out('trailer');
	Self.ss_out('<</Size ' + (Self.ssn+1));
	Self.ss_out('/Root ' + Self.ssn + ' 0 R');
	Self.ss_out('/Info ' + (Self.ssn-1) + ' 0 R>>');
	Self.ss_out('startxref');
	Self.ss_out(sso);
	Self.ss_out('%%EOF');
	Self.ssstate := 3;
end;

procedure TJPFpdf._beginpage(ssorientation);
begin
	Self.sspage++;
	Self.sspages[Self.sspage] := '';
	Self.ssstate := 2;
	Self.ssx := Self.sslMargin;
	Self.ssy := Self.sstMargin;
	Self.sslasth := 0;
	Self.ssFontFamily := '';
	//Page orientation
	if(not ssorientation) then 
		ssorientation := Self.ssDefOrientation;
	else
	begin
		ssorientation := strtoupper(ssorientationbegin0end;);
		if(ssorientation <> Self.ssDefOrientation) then 
			Self.ssOrientationChanges[Self.sspage] := true;
	end;
	if(ssorientation <> Self.ssCurOrientation) then 
	begin
		//Change orientation
		if(ssorientation = 'P') then 
		begin
			Self.sswPt := Self.ssfwPt;
			Self.sshPt := Self.ssfhPt;
			Self.ssw := Self.ssfw;
			Self.ssh := Self.ssfh;
		end
		else
		begin
			Self.sswPt := Self.ssfhPt;
			Self.sshPt := Self.ssfwPt;
			Self.ssw := Self.ssfh;
			Self.ssh := Self.ssfw;
		end;
		Self.ssPageBreakTrigger := Self.ssh-Self.ssbMargin;
		Self.ssCurOrientation := ssorientation;
	end;
	//Set transformation matrix
	Self.ss_out(round(Self.ssk,6) + ' 0 0 ' + round(Self.ssk,6) + ' 0 ' + Self.sshPt + ' cm');
end;

procedure TJPFpdf._endpage();
begin
	//End of page contents
	Self.ssstate := 1;
end;

procedure TJPFpdf._newobj();
begin
	//Begin a new object
	Self.ssn++;
	Self.ssoffsets[Self.ssn] := strlen(Self.ssbuffer);
	Self.ss_out(Self.ssn + ' 0 obj');
end;

procedure TJPFpdf._setfont(ssfamily,ssstyle,sssize);
begin
	global ssfpdf_charwidths;

	ssfamily := strtolower(ssfamily);
	if(ssfamily = '') then 
		ssfamily := Self.ssFontFamily;
	if(ssfamily = 'arial') then 
		ssfamily := 'helvetica';
	if(ssfamily = 'symbol' or ssfamily = 'zapfdingbats') then 
		ssstyle := '';
	ssstyle := strtoupper(ssstyle);
	if(ssstyle = 'IB') then 
		ssstyle := 'BI';
	if(sssize = 0) then 
		sssize := Self.ssFontSizePt;
	//Test if font is already selected
	if(Self.ssFontFamily = ssfamily and Self.ssFontStyle = ssstyle and Self.ssFontSizePt = sssize) then 
		return true;
	//Retrieve Type1 font name
	if(not isset(Self.ssfontnames[ssfamily.ssstyle])) then 
		return false;
	ssfontname := Self.ssfontnames[ssfamily.ssstyle];
	//Test if used for the first time
	if(not isset(Self.ssfonts[ssfontname])) then 
	begin
		ssn := count(Self.ssfonts);
		Self.ssfonts[ssfontname] := ssn+1;
		if(not isset(ssfpdf_charwidths[ssfamily.ssstyle])) then 
		begin
			//include metric file
			ssfile := ssfamily;
			if(ssfamily = 'times' or ssfamily = 'helvetica') then 
				ssfile  +=  strtolower(ssstyle);
			ssfile  +=  '.php';
			if(defined('FPDF_FONTPATH')) then 
				ssfile := FPDF_FONTPATH.ssfile;
			include(ssfile);
			if(not isset(ssfpdf_charwidths[ssfamily.ssstyle])) then 
				Self.ssError('Could not include font metric file');
		end;
	end;
	//Select it
	Self.ssFontFamily := ssfamily;
	Self.ssFontStyle := ssstyle;
	Self.ssFontSizePt := sssize;
	Self.ssFontSize := round(sssize/Self.ssk,2);
	if(Self.sspage>0) then 
		Self.ss_out('BT /F' + Self.ssfonts[ssfontname] + ' ' + Self.ssFontSize + ' Tf ET');
	return true;
end;

procedure TJPFpdf._setfontsize(sssize);
begin
	//Test if size already selected
	if(Self.ssFontSizePt = sssize) then 
		return;
	//Select it
	ssfontname := Self.ssfontnames[Self.ssFontFamily.Self.ssFontStyle];
	Self.ssFontSizePt := sssize;
	Self.ssFontSize := round(sssize/Self.ssk,2);
	if(Self.sspage>0) then 
		Self.ss_out('BT /F' + Self.ssfonts[ssfontname] + ' ' + Self.ssFontSize + ' Tf ET');
end;

procedure TJPFpdf._parsejpg(ssfile);
begin
	//Extract info from a JPEG file
	ssa := GetImageSize(ssfile);
	if(not ssa) then 
		Self.ssError('Missing or incorrect image file: ' + ssfile);
	if(ssa[2] <> 2) then 
		Self.ssError('Not a JPEG file: ' + ssfile);
	if(not isset(ssa['channels']) or ssa['channels'] = 3) then 
		sscolspace := 'DeviceRGB';
	else if(ssa['channels'] = 4) then 
		sscolspace := 'DeviceCMYK';
	else
		sscolspace := 'DeviceGray';
	ssbpc := isset(ssa['bits']) ? ssa['bits'] : 8;
	//Read whole file
	ssf := fopen(ssfile,'rb');
	ssdata := fread(ssf,filesize(ssfile));
	fclose(ssf);
	return array('w' := >ssa[0],'h' := >ssa[1],'cs' := >sscolspace,'bpc' := >ssbpc,'f' := >'DCTDecode','data' := >ssdata);
end;

procedure TJPFpdf._parsepng(ssfile);
begin
	//Extract info from a PNG file
	ssf := fopen(ssfile,'rb');
	if(not ssf) then 
		Self.ssError('Can\'t open image file: ' + ssfile);
	//Check signature
	if(fread(ssf,8) <> chr(137) + 'PNG' + chr(13) + chr(10) + chr(26) + chr(10)) then 
		Self.ssError('Not a PNG file: ' + ssfile);
	//Read header chunk
	fread(ssf,4);
	if(fread(ssf,4) <> 'IHDR') then 
		Self.ssError('Incorrect PNG file: ' + ssfile);
	ssw := Self.ss_freadint(ssf);
	ssh := Self.ss_freadint(ssf);
	ssbpc := ord(fread(ssf,1));
	if(ssbpc>8) then 
		Self.ssError('16-bit depth not supported: ' + ssfile);
	ssct := ord(fread(ssf,1));
	if(ssct = 0) then 
		sscolspace := 'DeviceGray';
	else if(ssct = 2) then 
		sscolspace := 'DeviceRGB';
	else if(ssct = 3) then 
		sscolspace := 'Indexed';
	else
		Self.ssError('Alpha channel not supported: ' + ssfile);
	if(ord(fread(ssf,1)) <> 0) then 
		Self.ssError('Unknown compression method: ' + ssfile);
	if(ord(fread(ssf,1)) <> 0) then 
		Self.ssError('Unknown filter method: ' + ssfile);
	if(ord(fread(ssf,1)) <> 0) then 
		Self.ssError('Interlacing not supported: ' + ssfile);
	fread(ssf,4);
	ssparms := '/DecodeParms <</Predictor 15 /Colors ' + (ssct = 2 ? 3 : 1) + ' /BitsPerComponent ' + ssbpc + ' /Columns ' + ssw + '>>';
	//Scan chunks looking for palette, transparency and image data
	sspal := '';
	sstrns := '';
	ssdata := '';
	do
	begin
		ssn := Self.ss_freadint(ssf);
		sstype := fread(ssf,4);
		if(sstype = 'PLTE') then 
		begin
			//Read palette
			sspal := fread(ssf,ssn);
			fread(ssf,4);
		end
		else if(sstype = 'tRNS') then 
		begin
			//Read transparency info
			sst := fread(ssf,ssn);
			if(ssct = 0) then 
				sstrns := array(substr(sst,1,1));
			else if(ssct = 2) then 
				sstrns := array(substr(sst,1,1),substr(sst,3,1),substr(sst,5,1));
			else
			begin
				sspos := strpos(sst,chr(0));
				if(is_int(sspos)) then 
					sstrns := array(sspos);
			end;
			fread(ssf,4);
		end
		else if(sstype = 'IDAT') then 
		begin
			//Read image data block
			ssdata  +=  fread(ssf,ssn);
			fread(ssf,4);
		end
		else if(sstype = 'IEND') then 
			break;
		else
			fread(ssf,ssn+4);
	end;
	while(ssn);
	if(sscolspace = 'Indexed' and empty(sspal)) then 
		Self.ssError('Missing palette in ' + ssfile);
	fclose(ssf);
	return array('w' => ssw,'h' => ssh,'cs' => sscolspace,'bpc' => ssbpc,'f' => 'FlateDecode','parms' => ssparms,'pal' => sspal,'trns' => sstrns,'data' => ssdata);
end;

procedure TJPFpdf._freadint(ssf);
begin
	//Read a 4-byte integer from file
	ssi := ord(fread(ssf,1))<<24;
	ssi += ord(fread(ssf,1))<<16;
	ssi += ord(fread(ssf,1))<<8;
	ssi += ord(fread(ssf,1));
	return ssi;
end;

procedure TJPFpdf._escape(sss);
begin
	//Add \ before \, ( and )
	return str_replace(')','\\)',str_replace('(','\\(',str_replace('\\','\\\\',sss)));
end;

procedure TJPFpdf._out(sss);
begin
	//Add a line to the document
	if(Self.ssstate = 2) then 
		Self.sspages[Self.sspage]  +=  sss + #10;
	else
		Self.ssbuffer  +=  sss + #10;
end;
//End of class
end;

//Handle silly IE contype request
if(isset(ssHTTP_ENV_VARS['HTTP_USER_AGENT']) and ssHTTP_ENV_VARS['HTTP_USER_AGENT'] = 'contype') then 
begin
	Header('Content-Type: application/pdf');
	exit;
end;

?>
