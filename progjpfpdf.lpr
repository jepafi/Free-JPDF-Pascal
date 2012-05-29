program progjpfpdf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, libjpfpdf, sysutils;


var
  JPFpdf1: TJPFpdf;
  i,i2: Integer;
  st: string;

begin
  JPFpdf1 := TJPFpdf.Fpdf();
  JPFpdf1.Open;
  JPFpdf1.AddPage();
  JPFpdf1.SetFont('arial','B',16);
  JPFpdf1.Cell(40,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.Ln(0);
  JPFpdf1.SetFont('courier','B',16);
  JPFpdf1.SetTextColor(98,147,199);
  JPFpdf1.Cell(80,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.Line(90,10,100,100);
  JPFpdf1.SetFillColor(177,32,10);
  JPFpdf1.Rect(50,50,100,100,'F');
  JPFpdf1.SetFont('Times','BI',40);
  JPFpdf1.SetTextColor(92,255,102);
  JPFpdf1.Text(20,180,'PDF para Free Pascal Web');
  JPFpdf1.AddPage();
  JPFpdf1.SetFont('arial','B',16);
  JPFpdf1.Cell(40,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.Ln(0);
  JPFpdf1.SetFont('courier','BI',16);
  JPFpdf1.Cell(80,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.AddPage();
  JPFpdf1.SetFont('arial','B',16);
  JPFpdf1.Cell(40,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.Ln(0);
  JPFpdf1.SetFont('courier','B',16);
  JPFpdf1.Cell(80,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.AddPage();
  JPFpdf1.SetFont('arial','B',16);
  JPFpdf1.Cell(40,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.Ln(0);
  JPFpdf1.SetFont('courier','B',16);
  JPFpdf1.Cell(80,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.AddPage();
  JPFpdf1.SetFont('arial','B',16);
  JPFpdf1.Cell(40,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.Ln(0);
  JPFpdf1.SetFont('courier','B',16);
  JPFpdf1.Cell(80,10,'Free Jpdf Pascal','0',0,'',0);
  JPFpdf1.Output(GetUserDir + '/JPFpdfTESTE.pdf');
end.

