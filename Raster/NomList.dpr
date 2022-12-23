program NomList;

uses
  Forms,
  Nom_Unit in 'Nom_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
