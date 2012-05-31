unit libjpfpdf;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, zstream;

type
    TJPDadosImage = record
      caminho: string;
      w: double;
      h: double;
      cs: string;
      bpc: integer;
      f: string;
      parms: string;
      pal: string;
      trns: string;
      end;

    TJPInfoJPEG = record
      width: double;
      height: double;
      syscor: string;
      bitscor: integer;
     end;

    TJPFone = record
      nome: string;
      numero: integer;
    end;

    TJPCor = (jpBlack,jpSilver,jpGray);

  { TJPFpdf }

  TJPFpdf = class
  private

  public
  const
    {$i inc_fontes.inc}
    FPDF_VERSION = '1.41';
  var
    sspage: integer;               // current page number
    ssn: integer;                  // current object number
    ssoffsets: array of integer;            // array of object offsets
    ssbuffer: TMemoryStream;             // buffer holding in-memory PDF
    sspages: array of string;              // array containing pages
    ssstate: integer;              // current document state
    sscompress: boolean;           // compression flag
    ssDefOrientation: string;     // default orientation
    ssCurOrientation: string;     // current orientation
    ssOrientationChanges: array of boolean;
    // array indicating orientation changes
    ssfwPt, ssfhPt: double;          // dimensions of page format in points
    ssfw, ssfh: double;              // dimensions of page format in user unit
    sswPt, sshPt: double;          // current dimensions of page in points
    ssw, ssh: double;              // current dimensions of page in user unit
    sslMargin: double;            // left margin
    sstMargin: double;            // top margin
    ssbMargin: double;            // page break margin
    sscMargin: double;            // cell margin
    ssx, ssy: double;  //////////////// mudou de int para dou
    // current position in user unit for cell positionning
    sslasth: double;              // height of last cell printed
    ssk: double;                  // scale factor (number of points in user unit)
    ssLineWidth: double;          // line width in user unit
    ssfontnames: array of string;   // array of Postscript (Type1) font names
    ssfonts: array of TJPFone;              // array of used fonts
    ssimages: array of string;             // array of used images
    ssFontFamily: string;         // current font family
    ssFontStyle: string;          // current font style
    ssFontSizePt: double;         // current font size in points
    ssFontSize: double;           // current font size in user unit
    ssDrawColor: string;          // commands for drawing color
    ssFillColor: string;          // commands for filling color
    ssTextColor: string;          // commands for text color
    ssColorFlag: boolean;
    // indicates whether fill and text colors are different
    ssws: double;                 // word spacing
    ssAutoPageBreak: boolean;      // automatic page breaking
    ssPageBreakTrigger: double;   // threshold used to trigger page breaks
    ssInFooter: boolean;           // flag set when processing footer
    ssDisplayMode: string;           // display mode
    sstitle: string;              // title
    sssubject: string;            // subject
    ssauthor: string;             // author
    sskeywords: string;           // keywords
    sscreator: string;            // creator
    ssAliasNbPages: string;       // alias for total number of pages
    DefDecimal: char;             // guarda o separador decimal default
    //    número da font e caractere
    ssfpdf_charwidths: array[0..13] of array [0..255] of integer;
    // widths fonts

    // substitui gzcompress
    function Compress(StrIn: string; CompLevel: TCompressionLevel = clMax): string;
    // substitui gzuncompress
    function Decompress(StrIn: string): string;
    constructor Fpdf(ssorientation: string = 'p'; ssunit: string = 'mm';
      ssformat: string = 'a4');
    procedure SetDecimal(Decimal: char = 'd');
    procedure SetMargins(ssleft: double; sstop: double);
    procedure SetLeftMargin(ssmargin: double);
    procedure SetAutoPageBreak(ssauto: boolean; ssmargin: double = 0.0);
    procedure SetDisplayMode(ssmode: string; ssz: integer = 100);
    procedure SetCompression(scompress: boolean);
    procedure SetTitle(stitle: string);
    procedure SetSubject(ssubject: string);
    procedure SetAuthor(sauthor: string);
    procedure SetKeywords(skeywords: string);
    procedure SetCreator(screator: string);
    procedure AliasNbPages(ssalias: string = '/{nb/}');
    procedure Error(ssmsg: string);
    procedure Open;
    procedure Close;
    procedure AddPage(ssorientation: string = '');
    procedure Header;
    procedure Footer;
    function PageNo: integer;
    procedure SetDrawColor(ssr: integer; ssg: integer = -1; ssb: integer = -1);
    procedure SetFillColor(ssr: integer; ssg: integer = -1; ssb: integer = -1);
    procedure SetTextColor(ssr: integer; ssg: integer = -1; ssb: integer = -1);
    procedure SetTextColor(cor: TJPCor);
    function GetStringWidth(sss: string): double;
    procedure SetLineWidth(sswidth: double);
    procedure Line(ssx1, ssy1, ssx2, ssy2: double);
    procedure Rect(sx, sy, sw, sh: double; ssstyle: string = '');
    procedure SetFont(sfamily: string; sstyle: string = ''; ssize: double = 0.0);
    procedure SetFontSize(sssize: double);
    procedure Text(sx, sy: double; sstxt: string);
    function AcceptPageBreak: boolean;
    procedure Cell(sw: double; sh: double = 0.0; sstxt: string = ''; // sw de int->dou e sh de int->dou
      ssborder: string = '0'; ssln: integer = 0; ssalign: string = '';
      ssfill: integer = 0);
    procedure MultiCell(sw, sh: double; sstxt: string; ssborder: string = '0';
      ssalign: string = 'J'; ssfill: integer = 0);
    procedure Image(ssfile: string; sx: double; sy: double;
      sw: double; sh: double = 0.0; sstype: string = '');
    procedure Ln(sh: double = 0);
    function GetX: double;
    procedure SetX(sx: double);
    function GetY: double;
    procedure SetY(sy: double);
    procedure SetXY(sx, sy: double);  // tudo mudou de int para dou
    procedure Output(ssfile: string = ''; ssdownload: boolean = False);
    function ValidaFonte(fonte: string): boolean;
    function FonteFoiUsada(fonte: string): boolean;
    function DevolveNomeFone(nomeAbreviado: string): string;

    {***************************************************************************
    *                                                                           *
    *                              Private methods                              *
    *                                                                           *
    ***************************************************************************}
    procedure _begindoc;
    procedure _enddoc;
    procedure _beginpage(ssorientation: string);
    procedure _endpage;
    procedure _newobj;
    function _setfont(ssfamily: string; ssstyle: string; sssize: double): boolean;
    function _setfontsize(sssize: double): boolean;
    function _parsejpg(ssfile: string): TMemoryStream;
    function _parsepng(ssfile: string): TMemoryStream;
    function _freadint(ssf: string): integer;
    function _escape(sss: string): string;
    procedure _out(sss: string);

    // =============================================================

  end;

implementation

{ TJPFpdf }

procedure TJPFpdf.SetMargins(ssleft: double; sstop: double);
begin
  //Set left and top margins
  Self.sslMargin := ssleft;
  Self.sstMargin := sstop;
end;

procedure TJPFpdf.SetLeftMargin(ssmargin: double);
begin
  //Set left margin
  Self.sslMargin := ssmargin;
end;

procedure TJPFpdf.SetAutoPageBreak(ssauto: boolean; ssmargin: double);
begin
  //Set auto page break mode and triggering margin
  Self.ssAutoPageBreak := ssauto;
  Self.ssbMargin := ssmargin;
  Self.ssPageBreakTrigger := Self.ssh - ssmargin;
end;

procedure TJPFpdf.SetDisplayMode(ssmode: string; ssz: integer);
begin
  //Set display mode in viewer
  if ((ssmode = 'fullpage') or (ssmode = 'fullwidth') or (ssmode = 'real') or
    (ssmode = 'default')) then
    Self.ssDisplayMode := ssmode
  else if (ssmode = 'zoom') then
    Self.ssDisplayMode := IntToStr(ssz)
  else
    Error('Incorrect display mode: ' + ssmode);
end;

procedure TJPFpdf.SetCompression(scompress: boolean);
begin
  //Set page compression
  Self.sscompress := scompress;
end;

procedure TJPFpdf.SetTitle(stitle: string);
begin
  //Title of document
  Self.sstitle := stitle;
end;

procedure TJPFpdf.SetSubject(ssubject: string);
begin
  //Subject of document
  Self.sssubject := ssubject;
end;

procedure TJPFpdf.SetAuthor(sauthor: string);
begin
  //Author of document
  Self.ssauthor := sauthor;
end;

procedure TJPFpdf.SetKeywords(skeywords: string);
begin
  //Keywords of document
  Self.sskeywords := skeywords;
end;

procedure TJPFpdf.SetCreator(screator: string);
begin
  //Creator of document
  Self.sscreator := screator;
end;

procedure TJPFpdf.AliasNbPages(ssalias: string);
begin
  //Define an alias for total number of pages
  Self.ssAliasNbPages := ssalias;
end;

procedure TJPFpdf.Error(ssmsg: string);
begin
  //Fatal error
  raise Exception.Create('JPFPDF error: ' + ssmsg);
end;

procedure TJPFpdf.Open;
begin
  //Begin document
  _begindoc;
end;

procedure TJPFpdf.Close;
begin
  //Terminate document
  if (Self.sspage = 0) then
    Error('Document contains no page');
  //Page footer
  Self.ssInFooter := True;
  Footer;
  Self.ssInFooter := False;
  //Close page
  _endpage;
  //Close document
  _enddoc;
end;

procedure TJPFpdf.AddPage(ssorientation: string);
var
  vfamily, vstyle, vdc, vfc, vtc: string;
  vsize: double;
  vlw: double;
  vcf: boolean;
begin
  //Start a new page
  SetDecimal('.');
  vfamily := Self.ssFontFamily;
  vstyle := Self.ssFontStyle;
  vsize := Self.ssFontSizePt;
  vlw := Self.ssLineWidth;
  vdc := Self.ssDrawColor;
  vfc := Self.ssFillColor;
  vtc := Self.ssTextColor;
  vcf := Self.ssColorFlag;
  if (Self.sspage > 0) then
  begin
    //Page footer
    Self.ssInFooter := True;
    Footer;
    Self.ssInFooter := False;
    //Close page
    _endpage;
  end;
  //Start new page
  _beginpage(ssorientation);
  //Set line cap style to square
  _out('2 J');
  //Set line width
  _out(FloatToStr(vlw) + ' w');
  //Set font
  if (vfamily <> '') then
    SetFont(vfamily, vstyle, vsize);
  //Set colors
  if (vdc <> '0 G') then
    _out(vdc);
  if (vfc <> '0 g') then
    _out(vfc);
  Self.ssTextColor := vtc;
  Self.ssColorFlag := vcf;
  //Page header
  Header;
  //Restore line width
  if (Self.ssLineWidth <> vlw) then
  begin
    Self.ssLineWidth := vlw;
    _out(FloatToStr(vlw) + ' w');
  end;
  //Restore font
  if (vfamily <> '') then
    SetFont(vfamily, vstyle, vsize);
  //Restore colors
  if (Self.ssDrawColor <> vdc) then
  begin
    Self.ssDrawColor := vdc;
    _out(vdc);
  end;
  if (Self.ssFillColor <> vfc) then
  begin
    Self.ssFillColor := vfc;
    _out(vfc);
  end;
  Self.ssTextColor := vtc;
  Self.ssColorFlag := vcf;
  SetDecimal;
end;

procedure TJPFpdf.Header;
begin
  // To be implemented in your own inherited class
end;

procedure TJPFpdf.Footer;
begin
  // To be implemented in your own inherited class
end;

function TJPFpdf.PageNo: integer;
begin
  //Get current page number
  Result := Self.sspage;
end;

procedure TJPFpdf.SetDrawColor(ssr: integer; ssg: integer; ssb: integer);
begin
  SetDecimal('.');
  //Set color for all stroking operations
  if (((ssr = 0) and (ssg = 0) and (ssb = 0)) or (ssg = -1)) then
    // substr vai de 0 em diante mais número de caracteres
    // copy vai de 1 em diante mais número de caracteres
    // i := 1;
    // substr('abcdef',0,4) // abcd         é igual a        copy('abcdef',1,4);
    // substr('abcdef',i) // bcdef          é igual a        copy('abcdef',i+1,Length('abcdef')-i);
    //    Self.ssDrawColor := substr(ssr/255,0,5) + ' G';
    Self.ssDrawColor := Copy(FloatToStr(ssr / 255), 1, 5) + ' G'
  else
    Self.ssDrawColor := Copy(FloatToStr(ssr / 255), 1, 5) + ' ' +
      Copy(FloatToStr(ssg / 255), 1, 5) + ' ' +
      Copy(FloatToStr(ssb / 255), 1, 5) + ' RG';
  if (Self.sspage > 0) then
    _out(Self.ssDrawColor);
  SetDecimal;
end;

procedure TJPFpdf.SetFillColor(ssr: integer; ssg: integer; ssb: integer);
begin
  SetDecimal('.');
  //Set color for all filling operations
  if (((ssr = 0) and (ssg = 0) and (ssb = 0)) or (ssg = -1)) then
    Self.ssFillColor := Copy(FloatToStr(ssr / 255), 1, 5) + ' g'
  else
    Self.ssFillColor := Copy(FloatToStr(ssr / 255), 1, 5) + ' ' +
      Copy(FloatToStr(ssg / 255), 1, 5) + ' ' +
      Copy(FloatToStr(ssb / 255), 1, 5) + ' rg';
  Self.ssColorFlag := (Self.ssFillColor <> Self.ssTextColor);
  if (Self.sspage > 0) then
    _out(Self.ssFillColor);
  SetDecimal;
end;

procedure TJPFpdf.SetTextColor(ssr: integer; ssg: integer; ssb: integer);
begin
  SetDecimal('.');
  //Set color for text
  if (((ssr = 0) and (ssg = 0) and (ssb = 0)) or (ssg = -1)) then
    Self.ssTextColor := Copy(FloatToStr(ssr / 255), 1, 5) + ' g'
  else
    Self.ssTextColor := Copy(FloatToStr(ssr / 255), 1, 5) + ' ' +
      Copy(FloatToStr(ssg / 255), 1, 5) + ' ' +
      Copy(FloatToStr(ssb / 255), 1, 5) + ' rg';
  Self.ssColorFlag := (Self.ssFillColor <> Self.ssTextColor);
  SetDecimal;
end;

procedure TJPFpdf.SetTextColor(cor: TJPCor);
begin
  if (cor = jpBlack) then SetTextColor(0,0,0);
end;

function TJPFpdf.GetStringWidth(sss: string): double;
var
  fonte: string;
  chave, vl, vi: integer;
  vw: double;
begin
  fonte := LowerCase(Self.ssFontFamily) + UpperCase(Self.ssFontStyle);
  if ((fonte = 'courier') or (fonte = 'courierB') or (fonte = 'courierI') or
    (fonte = 'courierBI')) then
    chave := 0
  else
  if ((fonte = 'helvetica') or (fonte = 'arial')) then
    chave := 4
  else
  if ((fonte = 'helveticaB') or (fonte = 'arialB')) then
    chave := 5
  else
  if ((fonte = 'helveticaI') or (fonte = 'arialI')) then
    chave := 6
  else
  if ((fonte = 'helveticaBI') or (fonte = 'arialBI')) then
    chave := 7
  else
  if (fonte = 'times') then
    chave := 8
  else
  if (fonte = 'timesB') then
    chave := 9
  else
  if (fonte = 'timesI') then
    chave := 10
  else
  if (fonte = 'timesBI') then
    chave := 11
  else
  if (fonte = 'symbol') then
    chave := 12
  else
  if (fonte = 'zapfdingbats') then
    chave := 13;
  vw := 0;
  vl := Length(sss);
  for vi := 0 to vl - 1 do
    vw += Self.ssfpdf_charwidths[chave][Ord(sss[vi])];
  Result := vw * Self.ssFontSize / 1000;
end;

procedure TJPFpdf.SetLineWidth(sswidth: double);
begin
	//Set line width
  SetDecimal('.');
	Self.ssLineWidth := sswidth;
	if(Self.sspage > 0) then
		_out(FloatToStr(sswidth) + ' w');
  SetDecimal;
end;

procedure TJPFpdf.Line(ssx1, ssy1, ssx2, ssy2: double);
begin
  SetDecimal('.');
	//Draw a line
	_out(FloatToStr(ssx1) + ' -' + FloatToStr(ssy1) + ' m ' + FloatToStr(ssx2) + ' -' + FloatToStr(ssy2) + ' l S');
  SetDecimal;
end;

procedure TJPFpdf.Rect(sx, sy, sw, sh: double; ssstyle: string);
var
  vop: string;
begin
  SetDecimal('.');
	//Draw a rectangle
  ssstyle := UpperCase(ssstyle);
	if(ssstyle = 'F') then
		vop := 'f'
	else if((ssstyle = 'FD') or (ssstyle = 'DF')) then
		vop := 'B'
	else
		vop := 'S';
	_out(FloatToStr(sx) + ' -' + FloatToStr(sy) + ' ' + FloatToStr(sw) + ' -' + FloatToStr(sh) + ' re ' + vop);
  SetDecimal;
end;

procedure TJPFpdf.SetFont(sfamily: string; sstyle: string; ssize: double);
begin
  SetDecimal('.');
	//Select a font; size given in points
	if not(_setfont(sfamily,sstyle,ssize)) then
		Error('Incorrect font family or style: ' + sfamily + ' ' + sstyle);
  SetDecimal;
end;

procedure TJPFpdf.SetFontSize(sssize: double);
begin
  SetDecimal('.');
	//Set font size in points
	_setfontsize(sssize);
  SetDecimal;
end;

procedure TJPFpdf.Text(sx, sy: double; sstxt: string);
var
  sss: string;
begin
  SetDecimal('.');
	//Output a string
  sstxt := StringReplace(StringReplace(StringReplace(sstxt,'\','\\',[rfReplaceAll]),')','\)',[rfReplaceAll]),'(','\(',[rfReplaceAll]);
	sss := 'BT ' + FloatToStr(sx) + ' -' + FloatToStr(sy) + ' Td (' + sstxt + ') Tj ET';
	if (Self.ssColorFlag) then
		sss := 'q ' + Self.ssTextColor + ' ' + sss + ' Q';
	_out(sss);
  SetDecimal;
end;

function TJPFpdf.AcceptPageBreak: boolean;
begin
	//Accept automatic page break or not
	Result := Self.ssAutoPageBreak;
end;

procedure TJPFpdf.Image(ssfile: string; sx: double; sy: double;
  sw: double; sh: double; sstype: string);
begin

  {
  var
    ms: TMemoryStream;
    i: integer;
  begin

    ms := TMemoryStream.Create;
    ms.Position := 0;
    ms.WriteByte($03);  // número hexadecimal
    ms.Position := 0;
    ShowMessage(IntToStr(ms.ReadByte));  // retorna 3

    // O BYTE DA POSIÇÃO 25 CONTEM O PADRAO DE COR DO PNG - VALORES 3 E 6 É RGB
    //
  }

	//Put an image on the page
  {
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
			ssinfo := _parsejpg(ssfile);
		else if(sstype = 'png') then
			ssinfo := _parsepng(ssfile);
		else
			Self.ssError('Unsupported image file type: ' + sstype);
		set_magic_quotes_runtime(ssmqr);
		ssinfo['n'] := count(Self.ssimages)+1;
		Self.ssimages[ssfile] := ssinfo;
	end
	else
		ssinfo := Self.ssimages[ssfile];
	//Automatic width or height calculus
	if(sw = 0) then
		sw := round(sh*ssinfo['w']/ssinfo['h'],2);
	if(sh = 0) then
		sh := round(sw*ssinfo['h']/ssinfo['w'],2);
	_out('q ' + sw + ' 0 0 ' + sh + ' ' + sx + ' -' + (sy+sh) + ' cm /I' + ssinfo['n'] + ' Do Q');
  }
end;

procedure TJPFpdf.Cell(sw: double; sh: double; sstxt: string;
  ssborder: string; ssln: integer; ssalign: string; ssfill: integer);
var
  vws, vx, vy, vdx: double;
  sss: string;
begin
  SetDecimal('.');
  //sstxt := Utf8ToAnsi(sstxt);
  //Output a cell
	if(((Self.ssy + sh) > Self.ssPageBreakTrigger) and not (Self.ssInFooter) and (AcceptPageBreak())) then
	begin
		vx := Self.ssx;
		vws := Self.ssws;
		if(vws > 0) then begin
      Self.ssws := 0;
			_out('0 Tw');
    end;
		AddPage(Self.ssCurOrientation);
		Self.ssx := vx;
		if(vws > 0) then begin
      Self.ssws := vws;
			_out(FloatToStr(vws) + ' Tw');
      end;
	end;
	if(sw = 0) then
		sw := Self.ssw - Self.sslMargin - Self.ssx;
	sss := '';
	if((ssfill = 1) or (ssborder = '1')) then
	begin
		sss  +=  FloatToStr(Self.ssx) + ' -' + FloatToStr(Self.ssy) + ' ' + FloatToStr(sw) + ' -' + FloatToStr(sh) + ' re ';
		if(ssfill = 1) then
      if (ssborder = '1') then sss += 'B ' else sss += 'f '
		else
			sss  +=  'S ';
	end;

	if((Pos('L',ssborder) > 0) or (Pos('T',ssborder) > 0) or (Pos('R',ssborder) > 0) or (Pos('B',ssborder) > 0)) then
	begin
		vx := Self.ssx;
		vy := Self.ssy;
		if(Pos('L',ssborder) > 0) then
			sss  +=  FloatToStr(vx) + ' -' + FloatToStr(vy) + ' m ' + FloatToStr(vx) + ' -' + FloatToStr((vy + sh)) + ' l S ';
		if(Pos('T',ssborder) > 0) then
			sss  +=  FloatToStr(vx) + ' -' + FloatToStr(vy) + ' m ' + FloatToStr((vx + sw)) + ' -' + FloatToStr(vy) + ' l S ';
		if(Pos('R',ssborder) > 0) then
			sss  +=  FloatToStr((vx + sw)) + ' -' + FloatToStr(vy) + ' m ' + FloatToStr((vx + sw)) + ' -' + FloatToStr((vy + sh)) + ' l S ';
		if(Pos('B',ssborder) > 0) then
			sss  +=  FloatToStr(vx) + ' -' + FloatToStr((vy + sh)) + ' m ' + FloatToStr((vx + sw)) + ' -' + FloatToStr((vy + sh)) + ' l S ';
	end;
	if(sstxt <> '') then
	begin
		if(ssalign = 'R') then
			vdx := sw - Self.sscMargin - GetStringWidth(sstxt)
		else if(ssalign = 'C') then
			vdx := (sw - GetStringWidth(sstxt)) / 2
		else
			vdx := Self.sscMargin;
		sstxt := StringReplace(StringReplace(StringReplace(sstxt,'\','\\',[rfReplaceAll]),')','\)',[rfReplaceAll]),'(','\(',[rfReplaceAll]);
		if(Self.ssColorFlag) then
			sss  +=  'q ' + Self.ssTextColor + ' ';
		sss  +=  'BT ' + FloatToStr(Self.ssx + vdx) + ' -' + FloatToStr((Self.ssy + 0.5 * sh + 0.3 * Self.ssFontSize)) + ' Td (' + sstxt + ') Tj ET';
		if(Self.ssColorFlag) then
			sss  +=  ' Q';
	end;
	if (sss <> '') then
		_out(sss);
	Self.sslasth := sh;
	if(ssln > 0) then
	begin
		//Go to next line
		Self.ssy += sh;
		if(ssln = 1) then
			Self.ssx := Self.sslMargin;
	end
	else
		Self.ssx += sw;
  SetDecimal;
end;

procedure TJPFpdf.MultiCell(sw, sh: double; sstxt: string; ssborder: string;
  ssalign: string; ssfill: integer);
var
  vb,vb2: string;
  vc: char;
  fonte,vs: string;
  chave,vnb,vsep,vi,vj,vl,vns,vnl,vls: integer;
  vwmax,vx,vy: double;
begin
fonte := LowerCase(Self.ssFontFamily) + UpperCase(Self.ssFontStyle);
if ((fonte = 'courier') or (fonte = 'courierB') or (fonte = 'courierI') or
  (fonte = 'courierBI')) then
  chave := 0
else
if ((fonte = 'helvetica') or (fonte = 'arial')) then
  chave := 4
else
if ((fonte = 'helveticaB') or (fonte = 'arialB')) then
  chave := 5
else
if ((fonte = 'helveticaI') or (fonte = 'arialI')) then
  chave := 6
else
if ((fonte = 'helveticaBI') or (fonte = 'arialBI')) then
  chave := 7
else
if (fonte = 'times') then
  chave := 8
else
if (fonte = 'timesB') then
  chave := 9
else
if (fonte = 'timesI') then
  chave := 10
else
if (fonte = 'timesBI') then
  chave := 11
else
if (fonte = 'symbol') then
  chave := 12
else
if (fonte = 'zapfdingbats') then
  chave := 13;
if(sw = 0) then
	sw := Self.ssw - Self.sslMargin - Self.ssx;
vwmax := (sw - 2 * Self.sscMargin) * 1000 / Self.ssFontSize;
sstxt := #10 + sstxt;
vs := StringReplace(sstxt,#13,'',[rfReplaceAll]);
vnb := Length(vs);
if((vnb > 0) and (vs[vnb - 1] = #10)) then
	vnb := vnb - 1;
vb := '';
if(ssborder <> '') then
begin
	if(ssborder = '1') then
	begin
		ssborder := 'LTRB';
		vb := 'LRT';
		vb2 := 'LR';
	end
	else
	begin
		vb2 := '';
		if(Pos('L',ssborder) > 0) then
			vb2 += 'L';
		if(Pos('R',ssborder) > 0) then
			vb2 += 'R';
    if(Pos('T',ssborder) > 0) then vb := vb2 + 'T' else vb := vb2;
	end;
end;
vsep := -1;
vi := 0;
vj := 0;
vl := 0;
vns := 0;
vnl := 1;
while(vi < vnb) do
begin
	//Get next character
	vc := vs[vi];
	if(vc = #10) then
	begin
		//Explicit line break
		if(Self.ssws > 0) then
		begin
			Self.ssws := 0;
		  _out('0 Tw');
		end;
    if (vi > 1) then Cell(sw, sh, Copy(vs, vj, vi-vj), vb, 2, ssalign, ssfill) else
      Cell(sw, 0, Copy(vs, vj, vi-vj), vb, 2, ssalign, ssfill);
		vi := vi + 1;
		vsep := -1;
		vj := vi;
		vl := 0;
		vns := 0;
		vnl := vnl + 1;
		if((ssborder <> '') and (vnl = 2)) then
			vb := vb2;
		continue;
	end;
	if(vc = ' ') then
	begin
		vsep := vi;
		vls := vl;
		vns := vns + 1;
	end;
	vl += Self.ssfpdf_charwidths[chave][Ord(vc)];
	if(vl > vwmax) then
	begin
		//Automatic line break
		if(vsep = -1) then
		begin
	  	if(vi = vj) then
				vi := vi + 1;
			if(Self.ssws > 0) then
			begin
				Self.ssws := 0;
				_out('0 Tw');
			end;
			Cell(sw, sh, Copy(vs,vj,vi-vj), vb, 2, ssalign, ssfill);
		end
		else
		begin
			if(ssalign = 'J') then
			begin
        if (vns > 1) then Self.ssws := StrToFloat(FloatToStrF((vwmax - vls) / 1000 * Self.ssFontSize / (vns - 1),ffNumber,14,3)) else
          Self.ssws := 0;
				_out(FloatToStr(Self.ssws) + ' Tw');
			end;
		 	Cell(sw, sh, Copy(vs,vj,vsep-vj), vb, 2, ssalign, ssfill);
			vi := vsep + 1;
		end;
		vsep := -1;
		vj := vi;
		vl := 0;
		vns := 0;
		vnl := vnl + 1;
		if((ssborder = '') and (vnl = 2)) then
			vb := vb2;
	end
	else
		vi := vi + 1;
end;
//Last chunk
if(Self.ssws > 0) then
begin
	Self.ssws := 0;
	_out('0 Tw');
end;
if((ssborder <> '') and (Pos('B',ssborder) > 0)) then
	vb += 'B';
Cell(sw, sh, Copy(vs, vj, vi-vj), vb, 2, ssalign, ssfill);
Self.ssx := Self.sslMargin;
end;

procedure TJPFpdf.Ln(sh: double);
begin
  SetDecimal('.');
	//Line feed; default value is last cell height
	Self.ssx := Self.sslMargin;
	if(sh <= 0) then
		Self.ssy += Self.sslasth
	else
		Self.ssy += sh;
  SetDecimal;
end;

function TJPFpdf.GetX: double;
begin
  SetDecimal('.');
	//Get x position
	Result := Self.ssx;
  SetDecimal;
end;

procedure TJPFpdf.SetX(sx: double);
begin
  SetDecimal('.');
	//Set x position
	if(sx >= 0) then
		Self.ssx := sx
	else
		Self.ssx := Self.ssw + sx;
  SetDecimal;
end;

function TJPFpdf.GetY: double;
begin
  SetDecimal('.');
	//Get y position
	Result := Self.ssy;
  SetDecimal;
end;

procedure TJPFpdf.SetY(sy: double);
begin
  SetDecimal('.');
	//Set y position and reset x
	Self.ssx := Self.sslMargin;
	if(ssy >= 0) then
		Self.ssy := sy
	else
		Self.ssy := Self.ssh + sy;
  SetDecimal;
end;

procedure TJPFpdf.SetXY(sx, sy: double);
begin
  SetDecimal('.');
	//Set x and y positions
	SetY(sy);
	SetX(sx);
  SetDecimal;
end;

procedure TJPFpdf.Output(ssfile: string; ssdownload: boolean);
begin
  SetDecimal('.');
	//Output PDF to file or browser

	if(Self.ssstate < 3) then begin
		Close;
  end;
	if(ssfile = '') then
	begin
		//Send to browser
//		Header('Content-Type: application/pdf');
//		if(headers_sent()) then
//			Self.ssError('Some data has already been output to browser, can\'t send PDF file');
//		Header('Content-Length: ' + strlen(Self.ssbuffer));
//		Header('Content-disposition: inline; filename := doc.pdf');
//		echo Self.ssbuffer;
	end
	else
	begin
		if(ssdownload) then
		begin
			//Download file
//			if(isset(ssHTTP_ENV_VARS['HTTP_USER_AGENT']) and strpos(ssHTTP_ENV_VARS['HTTP_USER_AGENT'],'MSIE 5.5')) then
//				Header('Content-Type: application/dummy');
//			else
//				Header('Content-Type: application/octet-stream');
//			if(headers_sent()) then
//				Self.ssError('Some data has already been output to browser, can\'t send PDF file');
//			Header('Content-Length: ' + strlen(Self.ssbuffer));
//			Header('Content-disposition: attachment; filename := ' + ssfile);
//			echo Self.ssbuffer;
		end
		else
		begin
			//Save file locally
      try
        Self.ssbuffer.SaveToFile(ssfile);
      except
        Error('Unable to create output file: ' + ssfile);
      end;
	end;
end;
end;

procedure TJPFpdf._begindoc;
begin
	//Start document
	Self.ssstate := 1;
	_out('%PDF-1.3');
end;

function TJPFpdf._setfont(ssfamily: string; ssstyle: string; sssize: double): boolean;
var
  vfontname: string;
  vn: integer;
begin
  SetDecimal('.');
	ssfamily := LowerCase(ssfamily);
	if(ssfamily = '') then
		ssfamily := Self.ssFontFamily;
	if(ssfamily = 'arial') then
		ssfamily := 'helvetica';
	if((ssfamily = 'symbol') or (ssfamily = 'zapfdingbats')) then
		ssstyle := '';
	ssstyle := UpperCase(ssstyle);
	if(ssstyle = 'IB') then
		ssstyle := 'BI';
	if(sssize = 0) then
		sssize := Self.ssFontSizePt;
	//Test if font is already selected
	if((Self.ssFontFamily = ssfamily) and (Self.ssFontStyle = ssstyle) and (Self.ssFontSizePt = sssize)) then begin
		Result := true;
    SetDecimal;
    Exit;
  end;
	//Retrieve Type1 font name
	if not(ValidaFonte(ssfamily + ssstyle)) then begin
	  Result := false;
    SetDecimal;
    Exit;
  end;
	vfontname := DevolveNomeFone(ssfamily + ssstyle);
	//Test if used for the first time
	if not(FonteFoiUsada(ssfamily + ssstyle)) then
	begin
		vn :=  Length(Self.ssfonts);
    SetLength(Self.ssfonts, vn + 1);
		Self.ssfonts[vn].numero := vn + 1;
    Self.ssfonts[vn].nome := vfontname;
	end;
	//Select it
	Self.ssFontFamily := ssfamily;
	Self.ssFontStyle := ssstyle;
	Self.ssFontSizePt := sssize;
	Self.ssFontSize := StrToFloat(FloatToStrF(sssize / Self.ssk, ffNumber, 14, 2));
  for vn := 0 to Length(Self.ssfonts) do begin
    if (Self.ssfonts[vn].nome = vfontname) then break;
  end;
if(Self.sspage > 0) then
		_out('BT /F' + IntToStr(Self.ssfonts[vn].numero) + ' ' + FloatToStrF(Self.ssFontSize, ffNumber, 14, 2) + ' Tf ET');
	Result := true;
  SetDecimal;
end;

procedure TJPFpdf._enddoc;
var
  vnb, vn, vo, vnbpal,vi, vnf,vu: integer;
  vwPt, vhPt: double;
  vfilter, vkids, vtrns, vp, vname: string;
begin
  SetDecimal('.');
	//Terminate document
	vnb := Self.sspage;
	if not(Self.ssAliasNbPages = '') then
	begin
		//Replace number of pages
    for vn := 1 to vnb do
			Self.sspages[vn] := StringReplace(Self.sspages[vn],Self.ssAliasNbPages,IntToStr(vnb),[]);
	end;
	if(Self.ssDefOrientation = 'P') then
	begin
		vwPt := Self.ssfwPt;
		vhPt := Self.ssfhPt;
	end
	else
	begin
		vwPt := Self.ssfhPt;
		vhPt := Self.ssfwPt;
	end;
  if (Self.sscompress) then vfilter := '/Filter /FlateDecode ' else vfilter := '';
  for vn := 1 to vnb do
	begin
		//Page
		_newobj();
		_out('<</Type /Page');
		_out('/Parent 1 0 R');
// RESOLVER
		if(Self.ssOrientationChanges[vn]) then
			_out('/MediaBox [0 0 ' + FloatToStr(vhPt) + ' ' + FloatToStr(vwPt) + ']');
// ---------------------
		_out('/Resources 2 0 R');
		_out('/Contents ' + IntToStr(Self.ssn + 1) + ' 0 R>>');
		_out('endobj');
		//Page content
    if (Self.sscompress) then vp := Compress(Self.sspages[vn]) else vp := Self.sspages[vn];
		_newobj();
		_out('<<' + vfilter + '/Length ' + IntToStr(Length(vp)) + '>>');
		_out('stream');
		_out(vp + 'endstream');
		_out('endobj');
	end;
	//Fonts
	vnf := Self.ssn;
	// reset(Self.ssfonts);
 // while(list(ssname) = each(Self.ssfonts)) do
  for vu := 0 to Length(Self.ssfonts) - 1 do
	begin
		_newobj();
		_out('<</Type /Font');
		_out('/Subtype /Type1');
		_out('/BaseFont /' +  Self.ssfonts[vu].nome);
		if((Self.ssfonts[vu].nome <> 'Symbol') and (Self.ssfonts[vu].nome <> 'ZapfDingbats')) then
			_out('/Encoding /WinAnsiEncoding');
		_out('>>');
		_out('endobj');
	end;
	//Images
{	vni := Self.ssn;
	reset(Self.ssimages);
	while(list(vfile,vinfo) = each(Self.ssimages)) do
	begin
		_newobj();
		_out('<</Type /XObject');
		_out('/Subtype /Image');
		_out('/Width ' + vinfo['w']);
		_out('/Height ' + vinfo['h']);
		if(vinfo['cs'] = 'Indexed') then
			_out('/ColorSpace [/Indexed /DeviceRGB ' + (strlen(vinfo['pal'])/3-1) + ' ' + (Self.ssn+1) + ' 0 R]');
		else
			_out('/ColorSpace /' + vinfo['cs']);
		_out('/BitsPerComponent ' + vinfo['bpc']);
		_out('/Filter /' + vinfo['f']);
		if(isset(vinfo['parms'])) then
			_out(vinfo['parms']);
		if(isset(vinfo['trns']) and is_array(vinfo['trns'])) then
		begin
			vtrns := '';
			for(vi := 0;vi<count(vinfo['trns']);vi++) do
				vtrns  +=  vinfo['trns'][vi] + ' ' + vinfo['trns'][vi] + ' ';
			_out('/Mask [' + vtrns + ']');
		end;
		_out('/Length ' + strlen(vinfo['data']) + '>>');
		_out('stream');
		_out(vinfo['data']);
		_out('endstream');
		_out('endobj');
		//Palette
		if(vinfo['cs'] = 'Indexed') then
		begin
			_newobj();
			_out('<</Length ' + strlen(vinfo['pal']) + '>>');
			_out('stream');
			_out(vinfo['pal']);
			_out('endstream');
			_out('endobj');
		end;
	end;  }
	//Pages root
	Self.ssoffsets[1] := Self.ssbuffer.Size;
	_out('1 0 obj');
	_out('<</Type /Pages');
	vkids := '/Kids [';

  for vi := 0 to Self.sspage - 1 do
  	vkids  +=  IntToStr(3 + 2 * vi) + ' 0 R ';

	_out(vkids + ']');
	_out('/Count ' + IntToStr(Self.sspage));
	_out('/MediaBox [0 0 ' + FloatToStr(vwPt) + ' ' + FloatToStr(vhPt) + ']');
	_out('>>');
	_out('endobj');
	//Resources
	Self.ssoffsets[2] := Self.ssbuffer.Size;
	_out('2 0 obj');
	_out('<</ProcSet [/PDF /Text /ImageB /ImageC /ImageI]');
	_out('/Font <<');
	for vi := 1 to Length(Self.ssfonts) do
		_out('/F' + IntToStr(vi) + ' ' + IntToStr(vnf + vi) + ' 0 R');
	_out('>>');
{	if(count(Self.ssimages)) then
	begin
		_out('/XObject <<');
		vnbpal := 0;
		reset(Self.ssimages);
		while(list(,vinfo) = each(Self.ssimages)) do
		begin
			_out('/I' + vinfo['n'] + ' ' + (vni+vinfo['n']+vnbpal) + ' 0 R');
			if(vinfo['cs'] = 'Indexed') then
				vnbpal++;
		end;
		_out('>>');
	end; }
	_out('>>');
	_out('endobj');
	//Info
	_newobj();
	_out('<</Producer (FPDF ' + FPDF_VERSION + ')');
	if(Self.sstitle <> '') then
		_out('/Title (' + _escape(Self.sstitle) + ')');
	if(Self.sssubject <> '') then
		_out('/Subject (' + _escape(Self.sssubject) + ')');
	if(Self.ssauthor <> '') then
		_out('/Author (' + _escape(Self.ssauthor) + ')');
	if(Self.sskeywords <> '') then
		_out('/Keywords (' + _escape(Self.sskeywords) + ')');
	if(Self.sscreator <> '') then
		_out('/Creator (' + _escape(Self.sscreator) + ')');
	_out('/CreationDate (D:' + FormatDateTime('ddmmyyyhhmmss',date) + ')>>');
	_out('endobj');
	//Catalog
	_newobj();
	_out('<</Type /Catalog');
	if(Self.ssDisplayMode = 'fullpage') then
		_out('/OpenAction [3 0 R /Fit]')
	else if(Self.ssDisplayMode = 'fullwidth') then
		_out('/OpenAction [3 0 R /FitH null]')
	else if(Self.ssDisplayMode = 'real') then
		_out('/OpenAction [3 0 R /XYZ null null 1]')
	else
		_out('/OpenAction [3 0 R /XYZ null null ' + FloatToStr(StrToInt(Self.ssDisplayMode) / 100) + ']');
	_out('/Pages 1 0 R>>');
	_out('endobj');
	//Cross-ref
	vo := Self.ssbuffer.Size;
	_out('xref');
	_out('0 ' + IntToStr(Self.ssn + 1));
	_out('0000000000 65535 f ');
  for vi := 1 to Self.ssn do
		_out(Format('%.10d 00000 n ',[Self.ssoffsets[vi]]));
	//Trailer
	_out('trailer');
	_out('<</Size ' + IntToStr(Self.ssn + 1));
	_out('/Root ' + IntToStr(Self.ssn) + ' 0 R');
	_out('/Info ' + IntToStr(Self.ssn - 1) + ' 0 R>>');
	_out('startxref');
	_out(IntToStr(vo));
	_out('%%EOF');
	Self.ssstate := 3;
  SetDecimal;
end;

procedure TJPFpdf._beginpage(ssorientation: string);
begin
  SetDecimal('.');
	Self.sspage := Self.sspage + 1;
  SetLength(Self.sspages, Length(Self.sspages) + 1);
	Self.sspages[Self.sspage] := '';
	Self.ssstate := 2;
	Self.ssx := Self.sslMargin;
	Self.ssy := Self.sstMargin;
	Self.sslasth := 0;
	Self.ssFontFamily := '';
	//Page orientation
	if (ssorientation = '') then
		ssorientation := Self.ssDefOrientation
	else
	begin
		ssorientation := UpperCase(ssorientation);
//    SetLength(Self.ssOrientationChanges,Length(Self.ssOrientationChanges) + 1);
		if(ssorientation <> Self.ssDefOrientation) then
			Self.ssOrientationChanges[Self.sspage] := true else
        Self.ssOrientationChanges[Self.sspage] := false;
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
		Self.ssPageBreakTrigger := Self.ssh - Self.ssbMargin;
		Self.ssCurOrientation := ssorientation;
	end;
	//Set transformation matrix
	_out(FloatToStrF(Self.ssk, ffNumber, 14,6) + ' 0 0 ' + FloatToStrF(Self.ssk, ffNumber, 14,6) + ' 0 ' + FloatToStr(Self.sshPt) + ' cm');
  SetDecimal;
end;

procedure TJPFpdf._endpage;
begin
	//End of page contents
	Self.ssstate := 1;
end;

procedure TJPFpdf._newobj;
begin
	//Begin a new object
 	Self.ssn := Self.ssn + 1;
	Self.ssoffsets[Self.ssn] := Self.ssbuffer.Size;
	_out(IntToStr(Self.ssn) + ' 0 obj');
end;

function TJPFpdf._setfontsize(sssize: double): boolean;
var
  vfontname: string;
  n,i: integer;
begin
  SetDecimal('.');
  n := 0;
	//Test if size already selected
	if(Self.ssFontSizePt = sssize) then
		Exit;
  Result := true;
	//Select it
	vfontname := DevolveNomeFone(Self.ssFontFamily + Self.ssFontStyle);
	Self.ssFontSizePt := sssize;
	Self.ssFontSize := StrToFloat(FloatToStrF(sssize / Self.ssk, ffNumber, 14, 2));

  for i := 0 to Length(Self.ssfonts)-1 do
  begin
    if  (Self.ssfonts[i].nome = vfontname) then begin
      n := Self.ssfonts[i].numero;
      break;
    end;
  end;
  if (n = 0) then Error('Fonte não foi encontrada: ' + vfontname);
  if(Self.sspage > 0) then
		_out('BT /F' + IntToStr(n) + ' ' + FloatToStrF(Self.ssFontSize, ffNumber, 14, 2) + ' Tf ET');
end;

function TJPFpdf._parsejpg(ssfile: string): TMemoryStream;
begin
  // Implementar na próxima versão
end;

function TJPFpdf._parsepng(ssfile: string): TMemoryStream;
begin
  // Implementar na próxima versão
end;

function TJPFpdf._freadint(ssf: string): integer;
begin
  // Provavelmente não será usado ou será modificado
end;

function TJPFpdf._escape(sss: string): string;
begin
	//Add \ before \, ( and )
  Result := StringReplace(StringReplace(StringReplace(sss,'\','\\',[rfReplaceAll]),')','\)',[rfReplaceAll]),'(','\(',[rfReplaceAll]);
end;

procedure TJPFpdf._out(sss: string);
begin
	//Add a line to the document
	if(Self.ssstate = 2) then
		Self.sspages[Self.sspage]  +=  sss + #10
	else begin
    sss := sss + #10;
		Self.ssbuffer.Write(Pointer(sss)^,Length(sss) * SizeOf(char));
  end;
end;

function TJPFpdf.Compress(StrIn: string; CompLevel: TCompressionLevel): string;
var
  cs: TCompressionStream;
  ss2: TStringStream;
begin
  ss2 := TStringStream.Create('');
  cs := tcompressionstream.Create(complevel, ss2);
  try
    cs.Write(strin[1], length(strin));
    cs.Free;
    Result := ss2.DataString;
    ss2.Free;
  except
    on e: Exception do
    begin
      Result := '';
      cs.Free;
      ss2.Free;
      raise;
    end;
  end;
end;

function TJPFpdf.Decompress(StrIn: string): string;
const
  bufsize = 65536;
var
  dcs: TDecompressionStream;
  ss1: TStringStream;
  br: integer;
  buf: string;
begin
  ss1 := tstringstream.Create(StrIn);
  dcs := tdecompressionstream.Create(ss1);
  try
    Result := '';
    repeat
      setlength(buf, bufsize);
      br := dcs.Read(buf[1], bufsize);
      Result := Result + Copy(buf, 1, br);
    until br < bufsize;
    dcs.Free;
    ss1.Free;
  except
    on e: Exception do
    begin
      Result := '';
      dcs.Free;
      ss1.Free;
      raise;
    end;
  end;
end;

constructor TJPFpdf.Fpdf(ssorientation: string; ssunit: string; ssformat: string);
var
  aformat: array[0..1] of double;
  ssmargin: double;
begin
  //Initialization of properties
  Self.DefDecimal := FormatSettings.DecimalSeparator;
  SetDecimal('.');
  Self.sspage := 0;
  Self.ssn := 2;
  Self.ssbuffer := TMemoryStream.Create;
//  Self.ssbuffer := TMemoryStream.Create;
  Self.ssbuffer.Position := 0;
  SetLength(Self.sspages, 2);
  SetLength(Self.ssOrientationChanges, 64000000);
  SetLength(Self.ssoffsets, 64000000);
  Self.ssstate := 0;
  //SetLength(Self.ssfonts, 14);
  SetLength(Self.ssimages, 2);
  Self.ssInFooter := False;
  Self.ssFontFamily := '';
  Self.ssFontStyle := '';
  Self.ssFontSizePt := 12;
  Self.ssDrawColor := '0 G';
  Self.ssFillColor := '0 g';
  Self.ssTextColor := '0 g';
  Self.ssColorFlag := False;
  Self.ssws := 0;
  //Font names
  SetLength(Self.ssfontnames, 14);
  Self.ssfontnames[0] := 'Courier'; // courier
  Self.ssfontnames[1] := 'Courier-Bold'; // courierB
  Self.ssfontnames[2] := 'Courier-Oblique'; // courierI
  Self.ssfontnames[3] := 'Courier-BoldOblique'; // courierBI
  Self.ssfontnames[4] := 'Helvetica'; // helvetica
  Self.ssfontnames[5] := 'Helvetica-Bold'; // helveticaB
  Self.ssfontnames[6] := 'Helvetica-Oblique'; // helveticaI
  Self.ssfontnames[7] := 'Helvetica-BoldOblique'; // helveticaBI
  Self.ssfontnames[8] := 'Times-Roman'; // times
  Self.ssfontnames[9] := 'Times-Bold'; // timesB
  Self.ssfontnames[10] := 'Times-Italic'; // timesI
  Self.ssfontnames[11] := 'Times-BoldItalic'; // timesBI
  Self.ssfontnames[12] := 'Symbol'; // symbol
  Self.ssfontnames[13] := 'ZapfDingbats'; // zapfdingbats
  //Fonts Char Sizes
  ssfpdf_charwidths[0] := FONT_COURIER_FULL;
  ssfpdf_charwidths[1] := FONT_COURIER_FULL;
  ssfpdf_charwidths[2] := FONT_COURIER_FULL;
  ssfpdf_charwidths[3] := FONT_COURIER_FULL;
  ssfpdf_charwidths[4] := FONT_HELVETICA_ARIAL;
  ssfpdf_charwidths[5] := FONT_HELVETICA_ARIAL_BOLD;
  ssfpdf_charwidths[6] := FONT_HELVETICA_ARIAL_ITALIC;
  ssfpdf_charwidths[7] := FONT_HELVETICA_ARIAL_BOLD_ITALIC;
  ssfpdf_charwidths[8] := FONT_TIMES;
  ssfpdf_charwidths[9] := FONT_TIMES_BOLD;
  ssfpdf_charwidths[10] := FONT_TIMES_ITALIC;
  ssfpdf_charwidths[11] := FONT_TIMES_BOLD_ITALIC;
  ssfpdf_charwidths[12] := FONT_SYMBOL;
  ssfpdf_charwidths[13] := FONT_ZAPFDINGBATS;
  //Scale factor
  if (ssunit = 'pt') then
    Self.ssk := 1
  else if (ssunit = 'mm') then
    Self.ssk := 72 / 25.4
  else if (ssunit = 'cm') then
    Self.ssk := 72 / 2.54
  else if (ssunit = 'in') then
    Self.ssk := 72
  else
    Error('Incorrect unit: ' + ssunit);
  //Page format
{  if(is_string(ssformat)) then
  begin}
  ssformat := LowerCase(ssformat);
  if (ssformat = 'a3') then
  begin
    aformat[0] := 841.89;
    aformat[1] := 1190.55;
  end
  else if (ssformat = 'a4') then
  begin
    aformat[0] := 595.28;
    aformat[1] := 841.89;
  end
  else if (ssformat = 'a5') then
  begin
    aformat[0] := 420.94;
    aformat[1] := 595.28;
  end
  else if (ssformat = 'letter') then
  begin
    aformat[0] := 612;
    aformat[1] := 792;
  end
  else if (ssformat = 'legal') then
  begin
    aformat[0] := 612;
    aformat[1] := 1008;
  end
  else
    Error('Unknown page format: ' + ssformat);
  Self.ssfwPt := aformat[0];
  Self.ssfhPt := aformat[1];
{  end    //                  TAMANHO PERSONALIZADO ssformat sintaxe '9999.99,9999.99' largura,altura
  else
  begin
    Self.ssfwPt := round(ssformat[0]*Self.ssk,2);
    Self.ssfhPt := round(ssformat[1]*Self.ssk,2);
  end;}
  Self.ssfw := StrToFloat(FloatToStrF(Self.ssfwPt / Self.ssk, ffNumber, 14, 2));
  Self.ssfh := StrToFloat(FloatToStrF(Self.ssfhPt / Self.ssk, ffNumber, 14, 2));
  //Page orientation
  ssorientation := LowerCase(ssorientation);
  if ((ssorientation = 'p') or (ssorientation = 'portrait')) then
  begin
    Self.ssDefOrientation := 'P';
    Self.sswPt := Self.ssfwPt;
    Self.sshPt := Self.ssfhPt;
  end
  else if ((ssorientation = 'l') or (ssorientation = 'landscape')) then
  begin
    Self.ssDefOrientation := 'L';
    Self.sswPt := Self.ssfhPt;
    Self.sshPt := Self.ssfwPt;
  end
  else
    Error('Incorrect orientation: ' + ssorientation);
  Self.ssCurOrientation := Self.ssDefOrientation;
  // StrToFloat(FloatToStrF(Self.ssfwPt/Self.ssk,ffNumber,14,2));
  Self.ssw := StrToFloat(FloatToStrF(Self.sswPt / Self.ssk, ffNumber, 14, 2));
  Self.ssh := StrToFloat(FloatToStrF(Self.sshPt / Self.ssk, ffNumber, 14, 2));
  //Page margins (1 cm)
  ssmargin := StrToFloat(FloatToStrF(28.35 / Self.ssk, ffNumber, 14, 2));
  SetMargins(ssmargin, ssmargin);
  //Interior cell margin (1 mm)
  Self.sscMargin := ssmargin / 10;
  //Line width (0.2 mm)
  Self.ssLineWidth := StrToFloat(FloatToStrF(0.567 / Self.ssk, ffNumber, 14, 3));
  //Automatic page break
  SetAutoPageBreak(True, 2 * ssmargin);
  //Full width display mode
  SetDisplayMode('fullwidth');
  //Compression
  SetCompression(False);
  SetDecimal;
end;

procedure TJPFpdf.SetDecimal(Decimal: char);
begin
  if ((Decimal = '.') or (Decimal = ',')) then
    FormatSettings.DecimalSeparator := '.'
  else
    FormatSettings.DecimalSeparator := DefDecimal;
end;

function TJPFpdf.ValidaFonte(fonte: string): boolean;
begin
  if (
  (fonte = 'courier') or
  (fonte = 'courierB') or
  (fonte = 'courierI') or
  (fonte = 'courierBI') or
  (fonte = 'helvetica') or
  (fonte = 'helveticaB') or
  (fonte = 'helveticaI') or
  (fonte = 'helveticaBI') or
  (fonte = 'times') or
  (fonte = 'timesB') or
  (fonte = 'timesI') or
  (fonte = 'timesBI') or
  (fonte = 'symbol') or
  (fonte = 'zapfdingbats')
  ) then Result := true else Result := false;
end;

function TJPFpdf.FonteFoiUsada(fonte: string): boolean;
var
  i: integer;
begin
  Result := false;
  if (ValidaFonte(fonte)) then begin
    for i := 0 to Length(Self.ssfonts)-1 do
    begin
      if  (Self.ssfonts[i].nome = DevolveNomeFone(fonte)) then begin
        Result := true;
        break;
      end;
    end;
  end;
end;

function TJPFpdf.DevolveNomeFone(nomeAbreviado: string): string;
begin
  Result := '';
  if (nomeAbreviado = 'courier') then Result := 'Courier' else
  if (nomeAbreviado = 'courierB')  then Result := 'Courier-Bold' else
  if (nomeAbreviado = 'courierI')  then Result := 'Courier-Oblique' else
  if (nomeAbreviado = 'courierBI')  then Result := 'Courier-BoldOblique' else
  if (nomeAbreviado = 'helvetica')  then Result := 'Helvetica' else
  if (nomeAbreviado = 'helveticaB')  then Result := 'Helvetica-Bold' else
  if (nomeAbreviado = 'helveticaI')  then Result := 'Helvetica-Oblique' else
  if (nomeAbreviado = 'helveticaBI')  then Result := 'Helvetica-BoldOblique' else
  if (nomeAbreviado = 'times')  then Result := 'Times-Roman' else
  if (nomeAbreviado = 'timesB')  then Result := 'Times-Bold' else
  if (nomeAbreviado = 'timesI')  then Result := 'Times-Italic' else
  if (nomeAbreviado = 'timesBI')  then Result := 'Times-BoldItalic' else
  if (nomeAbreviado = 'symbol')  then Result := 'Symbol' else
  if (nomeAbreviado = 'zapfdingbats')  then Result := 'ZapfDingbats' else
    Error('Nome abreviado de fonte inválido: ' + nomeAbreviado);
end;

end.

