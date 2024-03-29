unit NewPoint;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, GeoString;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Label5: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    XX: TEdit;
    YY: TEdit;
    Panel2: TPanel;
    Image1: TImage;
    Bevel1: TBevel;
    Label8: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton11: TSpeedButton;
    SpeedButton10: TSpeedButton;
    X: TSpinEdit;
    Y: TSpinEdit;
    Label7: TLabel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure XChange(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    N : Integer;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var B, L :Double;
begin
  case Form1.Projections.ItemIndex of
    0: begin
       B := StrToLatLon(YY.Text, false);
       L := StrToLatLon(XX.Text, true);
    end;
    1,2 : begin
       B := StrToFloat2(YY.Text);
       L := StrToFloat2(XX.Text);
    end;
  end;

  if Form1.ClickMode = 2 then
  begin
     Form1.AddPoint(Form1.StringGrid1, X.Value, Y.Value, B, L, N);
  end;

  if Form1.ClickMode = 3 then
     Form1.AddPoint(Form1.StringGrid2, X.Value, Y.Value, B, L, N);

  Form1.KillFrame.Enabled := Form1.StringGrid2.RowCount > 4;
  Form1.CountControl.Enabled := Form1.StringGrid1.RowCount > 2;

  if (CheckBox1.Checked) and (Form1.ClickMode = 2) then
       if Form1.StringGrid1.RowCount > 3 then
       try
          Form1.CountControl.OnClick(nil);
       except
         close;
       end;
  close;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
   close;
end;

procedure TForm2.XChange(Sender: TObject);
begin
   {with Form1 do
   Begin
     Image2.Picture.Bitmap.Canvas.Brush.Color := clBtnShadow;

     Image2.Picture.Bitmap.Canvas.FillRect(Rect(0,0,100,100));
     Image2.Picture.Bitmap.Canvas.CopyRect(Rect(0,0,100,100),
                               Image.Picture.Bitmap.Canvas, Rect(X.Value - Zoom[ZoomI]+1,
                               Y.Value - Zoom[ZoomI]+1, X.Value  + Zoom[ZoomI], Y.Value+ Zoom[ZoomI]));
     Image2.Canvas.Pen.Color := clRed;
     Image2.Canvas.MoveTo(Image2.Width div 2, Image2.Height);
     Image2.Canvas.LineTo(Image2.Width div 2, 0);
     Image2.Canvas.MoveTo(0, Image2.Height div 2);
     Image2.Canvas.LineTo(Image2.Width , Image2.Height div 2);

   End; }

   Form1.DrawMini(X.Value, Y.Value);

   Image1.Picture.Bitmap.Assign(Form1.Image2.Picture.Bitmap);
end;

procedure TForm2.SpeedButton10Click(Sender: TObject);
begin
  Form1.SpeedButton10.OnClick(nil);
  SpeedButton11.Enabled := Form1.SpeedButton11.Enabled;
  SpeedButton10.Enabled := Form1.SpeedButton10.Enabled;
  X.OnChange(nil);
end;

procedure TForm2.SpeedButton11Click(Sender: TObject);
begin
   Form1.SpeedButton11.OnClick(nil);
   SpeedButton11.Enabled := Form1.SpeedButton11.Enabled;
   SpeedButton10.Enabled := Form1.SpeedButton10.Enabled;
   X.OnChange(nil);
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
   SpeedButton11.Enabled := Form1.SpeedButton11.Enabled;
   SpeedButton10.Enabled := Form1.SpeedButton10.Enabled;

   Label3.Caption:=  Form1.StringGrid1.Cells[3,0]+':';
   Label4.Caption:=  Form1.StringGrid1.Cells[4,0]+':';
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  if Form1.ClickMode < 3 then
  Begin
    if N=0 then
      Label7.Caption := '����� �����'
      else
        Label7.Caption := '����� '+IntToStr(N);
  End
   else
    Begin
     if N=0 then
      Label7.Caption := '����� ����� �����'
      else
        Label7.Caption := '����� '+IntToStr(N);

    End;

    Panel1.Enabled := Form1.ClickMode < 3;

end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 form1.RefreshBtn.OnClick(nil);
 form1.UpdatePreview;
end;

procedure TForm2.SpeedButton4Click(Sender: TObject);
begin
  X.Value := X.Value -1;
end;

procedure TForm2.SpeedButton3Click(Sender: TObject);
begin
  X.Value := X.Value +1;
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  Y.Value := Y.Value -1;
end;

procedure TForm2.SpeedButton2Click(Sender: TObject);
begin
  Y.Value := Y.Value +1;
end;

end.
