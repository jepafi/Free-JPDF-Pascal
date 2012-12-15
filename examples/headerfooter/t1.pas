program t1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, libjpfpdf;

var
  lPdf: TJPFpdf;
  lData: TStrings;
begin
  lPdf := TJPFpdf.Create;
  lData := TStringList.Create;
  try
    lData.LoadFromFile('info.txt');
    lPdf.AddPage;
  finally
    lData.Free;
    lPdf.Free;
  end;

  writeln('Done.');
end.

