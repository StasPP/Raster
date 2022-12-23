program Raster_E;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  NewPoint in 'NewPoint.pas' {Form2},
  GeoReport in 'GeoReport.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
