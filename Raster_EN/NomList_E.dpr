program NomList_E;

uses
  Forms,
  Nom_Unit_E in 'Nom_Unit_E.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
