unit libjpfpdfextends;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libjpfpdf, math;

type

  TJPFpdfExtends = class;
  TPDFEvent = procedure(aPDF: TJPFpdfExtends) of object;

  { TJPFpdfExtends }

  TJPFpdfExtends = class(TJPFpdf)
  private
    FOnDoFooter: TPDFEvent;
    FOnDoHeader: TPDFEvent;
    FRowBorderHeight: Double;
    FRowLinesHeight: Double;
    FWidths: array of Double;
    FAligns: array of Char;
    FTitles: array of String;
    FRepeatTitleOnNewPage: Boolean;
    FTitlesUseBorders: Boolean;
    FTitleFill: Boolean;
    function NbLines(aW: Double; aTxt: String): Integer;
    procedure CheckPageBreak(aH: Double);
    procedure SetRowBorderHeight(AValue: Double);
    procedure SetRowLinesHeight(AValue: Double);

  public
    // Directions:
    // R: Left to Right, U: Bottom to Top, D: Top To Bottom, L: Right to Left
    constructor Create;
    constructor Create(orientation: TPDFOrientation; pageUnit: TPDFUnit;
      pageFormat: TPDFPageFormat); overload;
    constructor Create(orientation: TPDFOrientation; pageUnit: TPDFUnit;
      pageSizeW: Double; pageSizeH: Double = 0); overload;
    procedure TextWithDirection(aX, aY: Double; aText: String;
      aDirection: String = 'R');
    procedure TextWithRotation(aX, aY: Double; aText: String; aTxtAngle: Double;
      aFontAngle: Double = 0);
    procedure SetWidths(aW: array of Double);
    procedure SetAligns(aA: array of Char);
    procedure SetTitles(aT: array of String; aUseBorders: Boolean = True;
      aRepeatTitleOnNewPage: Boolean = True; aFill: Boolean = True);
    procedure Row(aData: array of String; aUseBorders: Boolean = True;
      aFill: Boolean = False);
    procedure DrawTitles;
    procedure Header; override;
    procedure Footer; override;

    property OnDoHeader: TPDFEvent read FOnDoHeader write FOnDoHeader;
    property OnDoFooter: TPDFEvent read FOnDoFooter write FOnDoFooter;

    property RowBorderHeight: Double read FRowBorderHeight write SetRowBorderHeight;
    property RowLinesHeight: Double  read FRowLinesHeight  write SetRowLinesHeight;
  end;

implementation

{ TJPFpdfExtends }

function TJPFpdfExtends.NbLines(aW: Double; aTxt: String): Integer;
var
  Vcw: array [0..255] of integer;
  nb, sep, i, j, l, nl: Integer;
  wmax: Double;
  s: String;
  c: Char;
begin
  //Computes the number of lines a MultiCell of width w will take
  Vcw := CurrentFontCW;
  if (aW = 0) then
    aW := Self.dw - Self.rMargin - Self.cpX;

  wmax := (aW - 2 * Self.cMargin) * 1000 / Self.cFontSize;

  s := StringReplace(aTxt, #13, '', [rfReplaceAll]);
  nb := Length(s);
  if ((nb > 0) and (s[nb] = #10)) then
    nb -= 1;

  sep := -1;
  i := 0;
  j := 0;
  l := 0;
  nl := 1;

  while (i < nb) do
  begin
    c := s[i + 1];
    if (c = #10) then
    begin
      i += 1;
      sep := -1;
      j := i;
      l := 0;
      nl := nl + 1;
      Continue;
    end;
    if (c = ' ') then
      sep := i;

    l := l + Vcw[Ord(c)];
    if (l > wmax) then
    begin
      if (sep = -1) then
      begin
	if (i = j) then
	  i += 1;
      end
      else
        i := sep + 1;

      sep := -1;
      j := i;
      l := 0;
      nl := nl + 1;
    end
    else
      i += 1;
  end;
  Result := nl;
end;

procedure TJPFpdfExtends.CheckPageBreak(aH: Double);
begin
  //If the height h would cause an overflow, add a new page immediately
  if (GetY + aH > PageBreakTrigger) then
  begin
    AddPage(CurOrientation);
    if ((Length(FTitles) > 0) and (FRepeatTitleOnNewPage)) then
      DrawTitles;
  end;
end;

procedure TJPFpdfExtends.SetRowBorderHeight(AValue: Double);
begin
  if FRowBorderHeight = AValue then Exit;
  FRowBorderHeight := AValue;
end;

procedure TJPFpdfExtends.SetRowLinesHeight(AValue: Double);
begin
  if FRowLinesHeight = AValue then Exit;
  FRowLinesHeight := AValue;
end;

constructor TJPFpdfExtends.Create;
begin
  inherited;
  FRowBorderHeight := 5;
  FRowLinesHeight := 5;
end;

constructor TJPFpdfExtends.Create(orientation: TPDFOrientation;
  pageUnit: TPDFUnit; pageFormat: TPDFPageFormat);
begin
  inherited Create(orientation, pageUnit, pageFormat);
  FRowBorderHeight := 5;
  FRowLinesHeight := 5;
end;

constructor TJPFpdfExtends.Create(orientation: TPDFOrientation;
  pageUnit: TPDFUnit; pageSizeW: Double; pageSizeH: Double);
begin
  inherited Create(orientation, pageUnit, pageSizeW, pageSizeH);
  FRowBorderHeight := 5;
  FRowLinesHeight := 5;
end;

procedure TJPFpdfExtends.TextWithDirection(aX, aY: Double; aText: String;
  aDirection: String);
var
  vS, vFmt: String;
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := #0;

  vFmt := 'BT %d %d %d %d %.2n -%.2n Tm (%s) Tj ET';

  if (aDirection = 'R') then
    vS := Format(vFmt, [1, 0, 0, 1, aX, aY, _escape(aText)], vFS)
  else
  if (aDirection = 'L') then
    vS := Format(vFmt, [-1, 0, 0, -1, aX, aY, _escape(aText)], vFS)
  else
  if (aDirection = 'U') then
    vS := Format(vFmt, [0, 1, -1, 0, aX, aY, _escape(aText)], vFS)
  else
  if (aDirection = 'D') then
    vS := Format(vFmt, [0, -1, 1, 0, aX, aY, _escape(aText)], vFS)
  else
    vS := Format('BT %.2n -%.2n Td (%s) Tj ET', [aX, aY, _escape(aText)], vFS);

  if (Self.pColorFlag) then
    vS := 'q ' + Self.pTextColor + ' ' + vS + ' Q';

  _out(vS);
end;

procedure TJPFpdfExtends.TextWithRotation(aX, aY: Double; aText: String;
  aTxtAngle: Double; aFontAngle: Double);
var
  vS: String;
  vTxtDx, vTxtDy, vFontDx, vFontDy: Double;
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := #0;

  aFontAngle += 90 + aTxtAngle;
  aTxtAngle  *= pi / 180;
  aFontAngle *= pi / 180;

  vTxtDx  := cos(aTxtAngle);
  vTxtDy  := sin(aTxtAngle);
  vFontDx := cos(aFontAngle);
  vFontDy := sin(aFontAngle);

  vS := Format('BT %.2n %.2n %.2n %.2n %.2n -%.2n Tm (%s) Tj ET',
               [vTxtDx, vTxtDy, vFontDx, vFontDy, aX, aY, _escape(aText)], vFS);

  if (Self.pColorFlag) then
    vS := 'q ' + Self.pTextColor + ' ' + vS + ' Q';

  _out(vS);
end;

procedure TJPFpdfExtends.SetWidths(aW: array of Double);
var
  i: Integer;
begin
  SetLength(FWidths, 0);
  SetLength(FWidths, Length(aW));
  For i := 0 to Length(aW)-1 do
    FWidths[i] := aW[i];
end;

procedure TJPFpdfExtends.SetAligns(aA: array of Char);
var
  i: Integer;
begin
  SetLength(FAligns, 0);
  SetLength(FAligns, Length(aA));
  For i := 0 to Length(aA)-1 do
    FAligns[i] := aA[i];
end;

procedure TJPFpdfExtends.SetTitles(aT: array of String; aUseBorders: Boolean;
  aRepeatTitleOnNewPage: Boolean; aFill: Boolean);
var
  i: Integer;
begin
  SetLength(FTitles, 0);
  SetLength(FTitles, Length(aT));
  For i := 0 to Length(aT)-1 do
    FTitles[i] := aT[i];

  FTitleFill            := aFill;
  FTitlesUseBorders     := aUseBorders;
  FRepeatTitleOnNewPage := aRepeatTitleOnNewPage;
end;

procedure TJPFpdfExtends.Row(aData: array of String; aUseBorders: Boolean;
  aFill: Boolean);
var
  Vnb, i: Integer;
  h, w, x, y: Double;
  a: Char;
begin
  //Calculate the height of the row
  Vnb := 0;
  for i := 0 to Length(aData) - 1 do
    Vnb := Max(Vnb, NbLines(FWidths[i],aData[i]));

  h := RowBorderHeight * Vnb;
  //h := Self.cFontSizePt/2 * Vnb;
  //Issue a page break first if needed
  CheckPageBreak(h);
  //Draw the cells of the row
  For i := 0 to Length(aData) - 1 do
  begin
    w := FWidths[i];
    if (i < Length(FAligns)) then
      a := FAligns[i]
    else
      a := 'L';

    //Save the current position
    x := GetX;
    y := GetY;
    //Draw the border
    if aUseBorders then
    begin
      if aFill then
        Rect(x, y, w, h, 'DF')
      else
        Rect(x, y, w, h);
    end;

    //Print the text
    MultiCell(w, RowLinesHeight, aData[i], '0', a);
    //MultiCell(w, Self.cFontSizePt/2, aData[i], '0', a);
    //Put the position to the right of the cell
    SetXY(x + w, y);
  end;
  //Go to the next line
  Ln(h);
end;

procedure TJPFpdfExtends.DrawTitles;
begin
  if (Length(FTitles) > 0) then
    Row(FTitles, FTitlesUseBorders, FTitleFill);
end;

procedure TJPFpdfExtends.Header;
begin
  if Assigned(OnDoHeader) then
    OnDoHeader(Self);
end;

procedure TJPFpdfExtends.Footer;
begin
  if Assigned(OnDoFooter) then
    OnDoFooter(Self);
end;

end.

