unit GeoReport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    SaveAsdb: TSpeedButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveAsdbClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm3.Button2Click(Sender: TObject);
begin
  close;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
    Memo1.Perform(EM_LINESCROLL,0,Memo1.Lines.Count-1);
    SaveAsdb.Enabled := Form1.Geoready;
end;

procedure TForm3.SaveAsdbClick(Sender: TObject);
begin
  hide;
  Form1.Repaint;
  close;
  Form1.SaveAsdb.Click;
end;

end.
