program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, libjpfpdf;

type
  TPdf = class(TJPFpdf)
  public
    procedure Header; override;
    procedure Footer; override;
  end;

var
  lPdf: TJPFpdf;
  lData: TStrings;

{ TPdf }

procedure TPdf.Header;
begin
  inherited;
  // Arial bold 15
  SetFont(ffHelvetica,fsBold,15);
  // Move to the right
  Cell(80);
  // Title
  Cell(30,10,'Title','1',0,'C');
  // Line break
  Ln(20);
end;

procedure TPdf.Footer;
begin
  // Position at 1.5 cm from bottom
  SetY(Self.fh -15);
  // Arial italic 8
  SetFont(ffHelvetica,fsItalic,8);
  // Page number
  Cell(0, 10, 'Page ' + IntToStr(PageNo), '0', 0, 'C');
  inherited;
end;

begin
  lPdf := TPdf.Create;
  lData := TStringList.Create;
  try
    lData.LoadFromFile('info.txt');

    lPdf.AddPage;
    lPdf.SetFont(ffHelvetica,fsNormal,10);

    lPdf.MultiCell(0, 6.5, lData.Text);

    lPdf.SaveToFile('report.pdf');
  finally
    lData.Free;
    lPdf.Free;
  end;

  writeln('Done.');
end.

