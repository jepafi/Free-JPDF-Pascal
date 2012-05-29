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
procedure FPDF(ssorientation := 'P',ssunit := 'mm',ssformat := 'A4')
procedure SetAutoPageBreak(ssauto,ssmargin := 0)
procedure SetDisplayMode(ssmode,ssz := 100)
procedure SetCompression(sscompress)
procedure SetTitle(sstitle)
procedure SetSubject(sssubject)
procedure SetAuthor(ssauthor)
procedure SetKeywords(sskeywords)
procedure SetCreator(sscreator)
procedure AliasNbPages(ssalias := 'beginnbend;')
procedure Error(ssmsg)
procedure Open()
procedure Close()
procedure AddPage(ssorientation := '')
procedure Header()
procedure Footer()
function PageNo()
procedure SetDrawColor(ssr,ssg := -1,ssb := -1)
procedure SetFillColor(ssr,ssg := -1,ssb := -1)
procedure SetTextColor(ssr,ssg := -1,ssb := -1)
function GetStringWidth(sss)
procedure SetLineWidth(sswidth)
procedure Line(ssx1,ssy1,ssx2,ssy2)
procedure Rect(ssx,ssy,ssw,ssh,ssstyle := '')
procedure SetFont(ssfamily,ssstyle := '',sssize := 0)
procedure SetFontSize(sssize)
procedure Text(ssx,ssy,sstxt)
function AcceptPageBreak()
procedure Cell(ssw,ssh := 0,sstxt := '',ssborder := 0,ssln := 0,ssalign := '',ssfill := 0)
procedure MultiCell(ssw,ssh,sstxt,ssborder := 0,ssalign := 'J',ssfill := 0)
procedure Image(ssfile,ssx,ssy,ssw,ssh := 0,sstype := '')
procedure Ln(ssh := '')
function GetX()
procedure SetX(ssx)
function GetY()
procedure SetY(ssy)
procedure SetXY(ssx,ssy)
procedure Output(ssfile := '',ssdownload := false)

{***************************************************************************
*                                                                           *
*                              Private methods                              *
*                                                                           *
***************************************************************************}
procedure _begindoc()
procedure _enddoc()
procedure _beginpage(ssorientation)
procedure _endpage()
procedure _newobj()
function _setfont(ssfamily,ssstyle,sssize): boolean;
function _setfontsize(sssize): boolean;
function _parsejpg(ssfile)
function _parsepng(ssfile)
function _freadint(ssf)
function _escape(sss)
procedure _out(sss)
