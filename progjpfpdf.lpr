program progjpfpdf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, libjpfpdf, sysutils;

var
  JPFpdf1: TJPFpdf;
  t: TStrings;
begin
  JPFpdf1 := TJPFpdf.Create;
  t := TStringList.Create;
  t.LoadFromFile(ExtractFilePath(ParamStr(0))+'20k_c1.txt');
  with JPFpdf1 do begin
    AddPage;
    SetFont(ffTimes,fsBold,16);
    SetDrawColor(cRed);
    SetLineWidth(0.3);
    SetFillColor(cYellow);
    Cell(0, 10, 'JUSTIFIED TEXT','LTBR',0,'C',1);
    Ln(20);
    SetFont(ffHelvetica,fsNormal,14);
    MultiCell(0, 6.5, t.Text);
    AddPage(poLandscape);
    SetFont(ffHelvetica,fsBold,16);
    Cell(40,10,'Free Jpdf Pascal','0',0,'',0);
    Ln(0);
    SetFont(ffHelvetica,fsBold,16);
    SetTextColor(98,147,199);
    Cell(80,10,'Free Jpdf Pascal','0',0,'',0);
    Line(90,10,100,100);
    SetFillColor(177,32,10);
    Rect(50,50,100,100,'F');
    SetFont(ffTimes,fsBoldItalic,40);
    SetTextColor(92,255,102);
    SetUnderline(True);
    Text(20,180,'PDF for Free Pascal Web');
    AddPage;
    SetFont(ffHelvetica,fsBold,16);
    Cell(60,10,'Free Jpdf Pascal','1',0,'C',0);
    Cell(60,10,'Free Jpdf Pascal','1',0,'C',0);
    Image(ExtractFilePath(ParamStr(0))+'image1.jpg',40,40,80,75);
    Image(ExtractFilePath(ParamStr(0))+'image2.png',40,120,120,75);
    Image(ExtractFilePath(ParamStr(0))+'image3.png',40,200,100,60);
    SetFont(ffCourier,fsBoldItalic,16,true);
    SetTextColor(0,0,0);
    Text(100,140,'Free Jpdf Pascal');
    AddPage;
    Code25(60,60,'775796075446',1.3,25);
    SetFont(ffTimes,fsBoldItalic,16,true);
    Writer(10,'Free Pascal is always ');
    SetFont(ffTimes,fsBold,16,true);
    Writer(10,'under development.');
    SetFont(ffTimes,fsItalic,16,false);
    Writer(10,' If you want to see how the development is progressing  ');
    SetFont(ffTimes,fsNormal,16,false);
    Writer(10,'you can take a peek at the developer versions.');
    Output(GetUserDir + '/JPFpdfTESTE.pdf');
    Free;
  end;
end.

