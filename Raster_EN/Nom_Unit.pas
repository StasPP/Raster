unit Nom_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, XPMan, GeoString, GeoFiles, GeoFunctions, GeoClasses;


type
  TMapNum = record
     M : Integer;
     num: array [1..6] of integer;
     litera : Char;
     isDouble: boolean;
  end;

  TMapBounds = record
     B, L: array [1..4] of Double;
  end;
  
type
  TForm1 = class(TForm)
    Bevel1: TBevel;
    Panel1: TPanel;
    Liter: TComboBox;
    Label1: TLabel;
    Panel0: TPanel;
    Label2: TLabel;
    Number: TComboBox;
    Scale: TComboBox;
    OutputPanel: TPanel;
    InputPanel: TPanel;
    Image1: TImage;
    CSPanel: TPanel;
    X1: TStaticText;
    Button1: TButton;
    DoubleList: TCheckBox;
    CS42: TRadioButton;
    OtherCS: TRadioButton;
    CSPanel2: TPanel;
    Datums: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    ProjList: TComboBox;
    Label5: TLabel;
    Zone: TSpinEdit;
    Button2: TButton;
    ListNamePanel: TPanel;
    Y1: TStaticText;
    Y4: TStaticText;
    X4: TStaticText;
    Y2: TStaticText;
    X2: TStaticText;
    Y3: TStaticText;
    X3: TStaticText;
    Panel2: TPanel;
    Label6: TLabel;
    Num500: TComboBox;
    OkPanel: TPanel;
    Button3: TButton;
    Button4: TButton;
    Panel3: TPanel;
    Label7: TLabel;
    Num200: TComboBox;
    SaveDialog1: TSaveDialog;
    Panel4: TPanel;
    Label8: TLabel;
    Num100: TComboBox;
    Panel5: TPanel;
    Label9: TLabel;
    Num50: TComboBox;
    Panel6: TPanel;
    Label10: TLabel;
    Num25: TComboBox;
    Shape1: TShape;
    Shape2: TShape;
    Label11: TLabel;
    Panel7: TPanel;
    SgY: TStaticText;
    SgX: TStaticText;
    Label12: TLabel;
    BreakFrame: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure LiterChange(Sender: TObject);
    procedure OtherCSClick(Sender: TObject);

    function GetMapCoords(Map:TMapNum): TMapBounds;

    procedure Button4Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ScaleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NumberChange(Sender: TObject);
    procedure ProjListChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

    procedure FindList(M:integer; Sx, Sy : double);
  private

    { Private declarations }
  public
  
    { Public declarations }
  end;

const
  Scales: array [0..5] of Integer =
  (1000000, 500000, 200000, 100000, 50000, 25000);

var
  Form1: TForm1;
  Map : TMapNum;
  MapB : TMapBounds;
  SK42, WGS : Integer;
  WaitForActivate : Boolean;

implementation

{$R *.dfm}


function IntToRoman(num: Cardinal): String;  
const 
  Nvals = 13; 
  vals: array [1..Nvals] of word = (1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000); 
  roms: array [1..Nvals] of string[2] = ('I', 'IV', 'V', 'IX', 'X', 'XL', 'L', 'XC', 'C', 'CD', 'D', 'CM', 'M'); 
var 
  b: 1..Nvals; 
begin 
  result := ''; 
  b := Nvals; 
  while num > 0 do 
  begin 
    while vals[b] > num do 
      dec(b); 
    dec (num, vals[b]); 
    result := result + roms[b] 
  end; 
end;

procedure TForm1.Button1Click(Sender: TObject);
var Num, Num500k, Num200k, Num100k, Num50k, Num25k: Integer;
    Lit : Char;
    x, y, h, B2, L2: Double;
    I, SK2, ZoneN : Integer;
    Sx,Sy : array [1..4] of String;
    AutoZone : boolean;
begin
 ZoneN  := Zone.Value;
 AutoZone := false;
 Lit := Liter.Items[Liter.ItemIndex][1];
 Num := StrToInt(Number.Items[Number.ItemIndex]);
 Num500k := Num500.ItemIndex+1;
 Num200k := Num200.ItemIndex+1;
 Num100k := Num100.ItemIndex+1;
 Num50k := Num50.ItemIndex+1;
 Num25k := Num25.ItemIndex+1;

 Case Scale.ItemIndex of
    0:  /// 1:1000000
    begin
      ListNamePanel.Caption := Lit+'-';
      if not DoubleList.Checked then
      Begin
        /// ���������
        ListNamePanel.Caption := ListNamePanel.Caption + IntToStr(Num);
      End
        else
          begin
            /// ���������
            if Num mod 2 = 0 then
               Num := Num - 1;

            ListNamePanel.Caption := ListNamePanel.Caption + IntToStr(Num)+'_'+IntToStr(Num+1) ;
          end;
    end;

    1:  /// 1:500000
    begin
      ListNamePanel.Caption := Lit + '-' + IntToStr(Num)+ '-';
      if not DoubleList.Checked then
      Begin
        /// ���������

        ListNamePanel.Caption := ListNamePanel.Caption + Num500.Items[Num500k-1];
      End
        else
          begin
            /// ���������
            if Num500k mod 2 = 0 then
               Num500k := Num500k - 1;

            ListNamePanel.Caption := ListNamePanel.Caption
                                     + Num500.Items[Num500k-1] + '_'
                                     + Num500.Items[Num500k-1+1];
          end;

    end;
    2:  /// 1:200000
    begin
      ListNamePanel.Caption := Lit + '-' + IntToStr(Num)+ '-';
      if not DoubleList.Checked then
      Begin
        /// ���������
        ListNamePanel.Caption := ListNamePanel.Caption + Num200.Items[Num200k-1];
      End
        else
          begin
            /// ���������
            if Num200k mod 2 = 0 then
               Num200k := Num200k - 1;

            ListNamePanel.Caption := ListNamePanel.Caption
                                     + Num200.Items[Num200k-1] + '_'
                                     + Num200.Items[Num200k-1+1];
          end;

    end;

    3:  /// 1:100000
    begin
      ListNamePanel.Caption := Lit + '-' + IntToStr(Num)+ '-';
      if not DoubleList.Checked then
      Begin
        /// ���������
        ListNamePanel.Caption := ListNamePanel.Caption + Num100.Items[Num100k-1];
      End
        else
          begin
            /// ���������
            if Num100k mod 2 = 0 then
               Num100k := Num100k - 1;

            ListNamePanel.Caption := ListNamePanel.Caption
                                     + Num100.Items[Num100k-1] + '_'
                                     + Num100.Items[Num100k-1+1];
          end;


    end;
    4:  /// 1:50000
    begin
      ListNamePanel.Caption := Lit + '-' + IntToStr(Num)+ '-' + Num100.Items[Num100k-1]+ '-' ;
      if not DoubleList.Checked then
      Begin
        /// ���������
        ListNamePanel.Caption := ListNamePanel.Caption + Num50.Items[Num50k-1];
      End
        else
          begin
            /// ���������
            if Num50k mod 2 = 0 then
               Num50k := Num50k - 1;

            ListNamePanel.Caption := ListNamePanel.Caption
                                     + Num50.Items[Num50k-1] + '_'
                                     + Num50.Items[Num50k-1+1];
          end;

    end;

    5: /// 1:25000
    begin
      ListNamePanel.Caption := Lit + '-' + IntToStr(Num) + '-'
                               + Num100.Items[Num100k-1] + '-'
                               + Num50.Items[Num50k-1] + '-';
      if not DoubleList.Checked then
      Begin
        /// ���������
        ListNamePanel.Caption := ListNamePanel.Caption + Num25.Items[Num25k-1];
      End
        else
          begin
            /// ���������
            if Num25k mod 2 = 0 then
               Num25k := Num25k - 1;

            ListNamePanel.Caption := ListNamePanel.Caption
                                     + Num25.Items[Num25k-1] + '_'
                                     + Num25.Items[Num25k-1+1];
          end;

    end;
  end;


  Map.M := Scales[Scale.ItemIndex];
  Map.Litera := Lit;
  Map.Num[1] := Num;
  Map.Num[2] := Num500k;
  Map.Num[3] := Num200k;
  Map.Num[4] := Num100k;
  Map.Num[5] := Num50k;
  Map.Num[6] := Num25k;
  Map.isDouble := DoubleList.Checked;

  MapB := GetMapCoords(Map);

  if CS42.Checked then
  begin
    /// � �� 42 ������-�������
    for I := 1 to 4 do
    begin
      Sy [I] := DegToDMS(MapB.B[I],12);
      Sx [I] := DegToDMS(MapB.L[I],12);
    end;
  end
    else
    for I := 1 to 4 do
    begin
       /// � ������������ ��
       SK2 := FindDatumByCaption(Datums.Items[Datums.ItemIndex]);
       Geo1ForceToGeo2(MapB.B[I], MapB.L[I], 0, SK42, SK2, B2, L2, h);

       case ProjList.ItemIndex of
         0:
         begin
           Sy [I] := DegToDMS(B2,12);
           Sx [I] := DegToDMS(L2,12);
         end;
         1:
         begin
           GeoToGaussKruger(B2,L2,x,y,ZoneN,AutoZone);
           Sy [I] := format('%n',[x]);
           Sx [I] := format('%n',[y]);
         end;
         2:
         begin
           GeoToUTM(SK2, B2,L2, False, x,y, ZoneN, AutoZone);
           Sy [I] := format('%n',[x]);
           Sx [I] := format('%n',[y]);
         end;
       end;
    end;

  X1.Caption := Sx[1];    Y1.Caption := Sy[1];
  X2.Caption := Sx[2];    Y2.Caption := Sy[2];
  X3.Caption := Sx[3];    Y3.Caption := Sy[3];
  X4.Caption := Sx[4];    Y4.Caption := Sy[4];

  Label11.Caption :=  ListNamePanel.Caption;
end;

function TForm1.GetMapCoords(Map: TMapNum): TMapBounds;

  function ExtractLitera(Litera:Char):Integer;
  begin
     Litera := UpCase(Litera);
     Result := Ord(Litera);
     if (Result > 90) or (Result < 50) then
       Result := -1;
  end;

  const B0 = 52;
        L0 = 78;

  var NumLit, I, J, i2, j2, i3, j3, ColCount : integer;
      dB, dL, dB2, dL2 : Real;
begin

  NumLit := ExtractLitera(Map.Litera);

  for I := 1 to 4 do
  begin
    Result.B[I] := 0;
    Result.L[I] := 0;
  end;

  if NumLit = -1 then
    exit;

  case Map.M of
    1000000 :
    begin
      Result.B[1] := (NumLit - 78 + 1)*4 + B0;
      Result.B[2] := Result.B[1];
      Result.B[3] := (NumLit - 78)*4 + B0;
      Result.B[4] := Result.B[3];

      Result.L[1] := (Map.Num[1] - 44)*6 + L0;
      if Map.isDouble then
        Result.L[2] := (Map.Num[1] - 44 + 2)*6 + L0
          else
            Result.L[2] := (Map.Num[1] - 44 + 1)*6 + L0;
      Result.L[3] := Result.L[2];
      Result.L[4] := Result.L[1];


    end;

    500000 :
    begin
      ColCount := 2;

      I := trunc((Map.Num[2]-1)/ColCount)+1;
      J := Map.Num[2] - (I-1)*ColCount;

      dB := 4*(i-1) / ColCount;
      dL := 6*(j-1) / ColCount;

      dB2 := 4*(i) / ColCount;
      if Map.isDouble then
        dL2 := 6*(j+1) / ColCount
          else
            dL2 := 6*(j) / ColCount;

      Result.B[1] := (NumLit - 78 + 1)*4 + B0 - dB;
      Result.B[2] := Result.B[1];
      Result.B[3] := (NumLit - 78 + 1)*4 + B0 - dB2;
      Result.B[4] := Result.B[3];

      Result.L[1] := (Map.Num[1] - 44)*6 + L0 + dL;
      Result.L[2] := (Map.Num[1] - 44)*6 + L0 + dL2;

      Result.L[3] := Result.L[2];
      Result.L[4] := Result.L[1];

    end;

    200000 :
    begin
      ColCount := 6;

      I := trunc((Map.Num[3]-1)/ColCount)+1;
      J := Map.Num[3] - (I-1)*ColCount;

      dB := 4*(i-1) / ColCount;
      dL := 6*(j-1) / ColCount;

      dB2 := 4*(i) / ColCount;
      if Map.isDouble then
        dL2 := 6*(j+1) / ColCount
          else
            dL2 := 6*(j) / ColCount;

      Result.B[1] := (NumLit - 78 + 1)*4 + B0 - dB;
      Result.B[2] := Result.B[1];
      Result.B[3] := (NumLit - 78 + 1)*4 + B0 - dB2;
      Result.B[4] := Result.B[3];

      Result.L[1] := (Map.Num[1] - 44)*6 + L0 + dL;
      Result.L[2] := (Map.Num[1] - 44)*6 + L0 + dL2;

      Result.L[3] := Result.L[2];
      Result.L[4] := Result.L[1];

    end;

    100000 :
    begin
     ColCount := 12;

      I := trunc((Map.Num[4]-1)/ColCount)+1;
      J := Map.Num[4] - (I-1)*ColCount;

      dB := 4*(i-1) / ColCount;
      dL := 6*(j-1) / ColCount;

      dB2 := 4*(i) / ColCount;
      if Map.isDouble then
        dL2 := 6*(j+1) / ColCount
          else
            dL2 := 6*(j) / ColCount;

      Result.B[1] := (NumLit - 78 + 1)*4 + B0 - dB;
      Result.B[2] := Result.B[1];
      Result.B[3] := (NumLit - 78 + 1)*4 + B0 - dB2;
      Result.B[4] := Result.B[3];

      Result.L[1] := (Map.Num[1] - 44)*6 + L0 + dL;
      Result.L[2] := (Map.Num[1] - 44)*6 + L0 + dL2;

      Result.L[3] := Result.L[2];
      Result.L[4] := Result.L[1];
    end;

    50000 :
    begin
      ColCount := 12;

      I := trunc((Map.Num[4]-1)/ColCount)+1;
      J := Map.Num[4] - (I-1)*ColCount;

      i2 := trunc((Map.Num[5]-1)/2)+1;
      j2 := Map.Num[5] - (i2-1)*2;

      dB := 4*(I-1 + (i2-1)/2) / ColCount;
      dL := 6*(J-1 + (j2-1)/2) / ColCount;

      dB2 := 4*(I-1 + i2/2) / ColCount;
      if Map.isDouble then
        dL2 := 6*(J-1+ (j2+1)/2) / ColCount
          else
            dL2 := 6*(J-1+ j2/2) / ColCount;

      Result.B[1] := (NumLit - 78 + 1)*4 + B0 - dB;
      Result.B[2] := Result.B[1];
      Result.B[3] := (NumLit - 78 + 1)*4 + B0 - dB2;
      Result.B[4] := Result.B[3];

      Result.L[1] := (Map.Num[1] - 44)*6 + L0 + dL;
      Result.L[2] := (Map.Num[1] - 44)*6 + L0 + dL2;

      Result.L[3] := Result.L[2];
      Result.L[4] := Result.L[1];

    end;

    25000 :
    begin
      ColCount := 12;

      I := trunc((Map.Num[4]-1)/ColCount)+1;
      J := Map.Num[4] - (I-1)*ColCount;

      i2 := trunc((Map.Num[5]-1)/2)+1;
      j2 := Map.Num[5] - (i2-1)*2;

      i3 := trunc((Map.Num[6]-1)/2)+1;
      j3 := Map.Num[6] - (i3-1)*2;

      dB := 4*(I-1 + (i2-1 + (i3-1)/2 )/2) / ColCount;
      dL := 6*(J-1 + (j2-1 + (j3-1)/2 )/2) / ColCount;

      dB2 := 4*(I-1 + (i2-1 + i3/2 )/2) / ColCount;
      if Map.isDouble then
        dL2 := 6*(J-1+ (j2-1 + (j3+1)/2 )/2) / ColCount
          else
            dL2 := 6*(J-1+ (j2-1 + j3/2 )/2) / ColCount;

      Result.B[1] := (NumLit - 78 + 1)*4 + B0 - dB;
      Result.B[2] := Result.B[1];
      Result.B[3] := (NumLit - 78 + 1)*4 + B0 - dB2;
      Result.B[4] := Result.B[3];

      Result.L[1] := (Map.Num[1] - 44)*6 + L0 + dL;
      Result.L[2] := (Map.Num[1] - 44)*6 + L0 + dL2;

      Result.L[3] := Result.L[2];
      Result.L[4] := Result.L[1];
    end;
  end;
end;

procedure TForm1.LiterChange(Sender: TObject);
begin
  if Liter.Items[Liter.ItemIndex][1] in ['P'..'T'] then
    DoubleList.Checked := true
     else
       DoubleList.Checked := false;
  Button1.OnClick(nil);
end;

procedure TForm1.OtherCSClick(Sender: TObject);
begin
  CSPanel2.Visible := OtherCS.Checked;
  Button1.OnClick(nil);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 close;
end;

procedure TForm1.FormActivate(Sender: TObject);
var S: TStringList;
   SearchX, SearchY, h :Double;
   SearchM : Integer;
begin
 if not WaitForActivate then
 exit;

 Scale.OnChange(nil);

 if ParamStr(1) = '-r' then
 begin
   CSPanel.Visible := False;
   OkPanel.Visible := True;
   CSPanel.Visible := True;
   Button2.Visible := False;

   if not fileexists('tmp\find.txt') then
     exit;

   S := TStringList.Create;
   try
      S.LoadFromFile('tmp\find.txt');
      SearchM := StrToInt(s[0]);
      Geo1ForceToGeo2(StrToFloat2(s[1]),StrToFloat2(s[2]),0,WGS,SK42,SearchX,SearchY,h);
      FindList(SearchM,SearchX,SearchY);
   except
   end;   
   S.Destroy;
 end;

 if ParamStr(1) = '-r2' then
 begin
   CSPanel.Visible := False;
   OkPanel.Visible := False;
   CSPanel.Visible := True;
   Button2.Visible := True;

   if not fileexists('tmp\find.txt') then
     exit;

   S := TStringList.Create;
   try
      S.LoadFromFile('tmp\find.txt');
      SearchM := StrToInt(s[0]);
      Geo1ForceToGeo2(StrToFloat2(s[1]),StrToFloat2(s[2]),0,WGS,SK42,SearchX,SearchY,h);
      FindList(SearchM,SearchX,SearchY);
   except
   end;   
   S.Destroy;
 end;
end;

procedure TForm1.ScaleChange(Sender: TObject);
begin
  Panel2.Visible := False;
  Panel3.Visible := False;
  Panel4.Visible := False;
  Panel5.Visible := False;
  Panel6.Visible := False;

  Panel2.Visible := Scale.ItemIndex = 1;
  Panel3.Visible := Scale.ItemIndex = 2;
  Panel6.Visible := Scale.ItemIndex > 4;
  Panel5.Visible := Scale.ItemIndex > 3;
  Panel4.Visible := Scale.ItemIndex > 2;

  Button1.OnClick(nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
var I:Integer;
begin
  GeoInit('Data\Sources.loc');

  if  Datums.Items.Count = 0 then
  begin
        for I := 0 to Length(DatumList)-1 do
          Datums.Items.Add(DatumList[i].Caption);
        Datums.ItemIndex := 0;
  end;

  SK42 := FindDatum('SK42');
  WGS := FindDatum('WGS84') ;

  Num200.Clear;
  For I := 1 to 36 do
    Num200.Items.Add(IntToRoman(I));
  Num200.ItemIndex := 0;

  For I := 1 to 144 do
    if I<10 then
        Num100.Items.Add('00'+IntToStr(I))
      else
        if I<100 then
            Num100.Items.Add('0'+IntToStr(I))
        else
            Num100.Items.Add(IntToStr(I));
  Num100.ItemIndex := 0;

  WaitForActivate := true;
end;

procedure TForm1.NumberChange(Sender: TObject);
begin
  If ProjList.ItemIndex = 1 then
    Zone.Value := StrToInt(Number.Items[Number.ItemIndex])-30
    else
      If ProjList.ItemIndex = 2 then
        Zone.Value := StrToInt(Number.Items[Number.ItemIndex]);
        
  Button1.OnClick(nil);
end;

procedure TForm1.ProjListChange(Sender: TObject);
begin
  Zone.Visible := ProjList.ItemIndex > 0;
  Label5.Visible := Zone.Visible;

  Number.OnChange(nil);
  Button1.OnClick(nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
var S : TStringList;
    FName : String;
begin
  if SaveDialog1.Execute then
  Begin
     FName := Savedialog1.FileName;

     if FName[Length(FName)-3]<>'.' then
        Fname := FName + '.txt';

     if Fileexists(FName) then
       if not (MessageDlg('���������� ������������ ����: ' + #13 + FName,
              mtConfirmation, mbYesNoCancel, 0) = 6) then
                 exit;

     S := TStringList.Create;
     S.Add(ListNamePanel.Caption);
     S.Add(X1.Caption +'  '+ Y1.Caption);
     S.Add(X1.Caption +'  '+ Y1.Caption);
     S.Add(X1.Caption +'  '+ Y1.Caption);
     S.Add(X1.Caption +'  '+ Y1.Caption);
     S.SaveToFile(FName);
  End;
end;

procedure TForm1.Button3Click(Sender: TObject);
var S : TStringList;
    FName : String;
    i,j : integer;
    x, y, x2, y2, h, xi, yi : Double;
begin

     ForceDirectories('tmp\');
     FName := 'tmp\bounds.txt';

     S := TStringList.Create;
     for i := 1 to 4 do
     Begin
       Geo1ToGeo2(MapB.B[I], MapB.L[I], 0, SK42, WGS, x, y, h);
       S.Add(FloatToStr(X));
       S.Add(FloatToStr(Y));

       if BreakFrame.Checked then
       Begin
          if i<4 then
            Geo1ToGeo2(MapB.B[I+1], MapB.L[I+1], 0, SK42, WGS, x2, y2, h)
              else
                Geo1ToGeo2(MapB.B[1], MapB.L[1], 0, SK42, WGS, x2, y2, h);

          for j := 1 to 9 do
          begin
            xi := X + j*(X2-X)/10;
            yi := Y + j*(Y2-Y)/10;

            S.Add(FloatToStr(Xi));
            S.Add(FloatToStr(Yi));
          end;
       End;
     End;
     S.SaveToFile(FName);

     close;
end;

procedure TForm1.FindList(M: integer; Sx, Sy: double);
  var i, j, k, ScaleN : integer;
    NewNum, NewLit: Integer;
    FindOk : Boolean;

  const B0 = 52;
        L0 = 78;
begin
 Scale.ItemIndex := 0;
 Liter.ItemIndex := 0;
 Number.ItemIndex := 0;

 Panel7.Visible := True;
 SgX.Caption := DegToDMS(Sx,12);
 SgY.Caption := DegToDMS(Sy,12);

 for I := 0 to 5 do
   if M = Scales[I] then
   begin
     Scale.ItemIndex := I;
     Scale.OnChange(nil);
     ScaleN := I;
     break;
   end;

   for i:= 0 To Liter.Items.Count-1 do
   Begin
      Liter.ItemIndex := i;

      NewLit := Ord(UpCase(Liter.Items[I][1]));

      if (Sx >= (NewLit - 78)*4 + B0) and (Sx <= (NewLit - 78 + 1)*4 + B0) then
         break;
   End;

   for i:= 0 To Number.Items.Count-1 do
   Begin
      Number.ItemIndex := i;

      NewNum := StrToInt(Number.Items[I]);

      if  (Sy >= (NewNum - 44)*6 + L0) and (Sy <= (NewNum +1 - 44)*6 + L0) then
         break;
   End;
   Button1.OnClick(nil);
   
   case ScaleN of
     1:  /// 1: 500 000
        for i := 0 to Num500.Items.Count-1 do
        begin
          Num500.ItemIndex := i;
          Button1.OnClick(nil);

          if (Sx >= MapB.B[3]) and (Sx <= MapB.B[1]) then
            if (Sy >= MapB.L[1]) and (Sy <= MapB.L[3])  then
                 break;
        end;
      2:  /// 1: 200 000
        for i := 0 to Num200.Items.Count-1 do
        begin
          Num200.ItemIndex := i;
          Button1.OnClick(nil);

          if (Sx >= MapB.B[3]) and (Sx <= MapB.B[1]) then
            if (Sy >= MapB.L[1]) and (Sy <= MapB.L[3])  then
                 break;
        end;
       3:  /// 1: 100 000
        for i := 0 to Num100.Items.Count-1 do
        begin
          Num100.ItemIndex := i;
          Button1.OnClick(nil);

          if (Sx >= MapB.B[3]) and (Sx <= MapB.B[1]) then
            if (Sy >= MapB.L[1]) and (Sy <= MapB.L[3])  then
                 break;
        end;
        4:  /// 1: 100 000  1:50 000
        Begin
          FindOk := false;

          for i := 0 to Num100.Items.Count-1 do
          begin
            Num100.ItemIndex := i;

            for j := 0 to Num50.Items.Count-1 do
            Begin
              Num50.ItemIndex := j;
              Button1.OnClick(nil);
              if (Sx >= MapB.B[3]) and (Sx <= MapB.B[1]) then
                if (Sy >= MapB.L[1]) and (Sy <= MapB.L[3])  then
                  FindOk := true;
              if findOk then
                break;
            End;

            if findOk then
              break;
          end;

        End;
        5:  /// 1: 100 000  1:50 000  1:25 000
        Begin
          FindOk := false;

          for i := 0 to Num100.Items.Count-1 do
          begin
            Num100.ItemIndex := i;

            for j := 0 to Num50.Items.Count-1 do
            Begin
              Num50.ItemIndex := j;

              for k := 0 to Num25.Items.Count-1 do
              begin
                Num25.ItemIndex := k;
                Button1.OnClick(nil);

                if (Sx >= MapB.B[3]) and (Sx <= MapB.B[1]) then
                  if (Sy >= MapB.L[1]) and (Sy <= MapB.L[3])  then
                    FindOk := true;
                if findOk then
                  break;
              end;

              if findOk then
                break;

            End;

            if findOk then
              break;

          end;

        End;

   end;



  

end;

end.
