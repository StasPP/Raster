unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, Grids, StdCtrls, ComCtrls, Channels, Buttons, Jpeg,
  GraphicEx, GIFImage, GeoFunctions, GeoFiles, GeoString, GeoClasses,
  AsphyreDef, pxfm, AsphyreBmpLoad, AsphyreDb, ImageFX, ImgList, XPMan, Math,
  ExtDlgs;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    OziExplorer1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Panel2: TPanel;
    Load1: TSpeedButton;
    RefreshBtn: TSpeedButton;
    Label14: TLabel;
    Panel3: TPanel;
    Preview: TImage;
    Panel4: TPanel;
    Datums: TComboBox;
    SaveAsdb: TSpeedButton;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    BM2: TSpeedButton;
    BM3: TSpeedButton;
    Bevel1: TBevel;
    CheckBox2: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Projections: TComboBox;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Label4: TLabel;
    MMap: TBevel;
    Panel6: TPanel;
    Panel7: TPanel;
    StringGrid2: TStringGrid;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel5: TPanel;
    KillFrame: TSpeedButton;
    ReturnFrame: TSpeedButton;
    BM1: TSpeedButton;
    ASDB1: TMenuItem;
    PreShape: TShape;
    Label8: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    Label6: TLabel;
    Shape1: TShape;
    SpeedButton9: TSpeedButton;
    N9: TMenuItem;
    N10: TMenuItem;
    Image2: TImage;
    Bevel2: TBevel;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    AutoList: TSpeedButton;
    SaveDialog2: TSaveDialog;
    PB: TProgressBar;
    CountControl: TButton;
    Label5: TLabel;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    PopupMenu3: TPopupMenu;
    OpenDialog2: TOpenDialog;
    SpeedButton1: TSpeedButton;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    OziExplorer2: TMenuItem;
    BMP1: TMenuItem;
    ASDB2: TMenuItem;
    SPD: TSavePictureDialog;
    N11: TMenuItem;
    N20: TMenuItem;
    OpenDialog3: TOpenDialog;
    N21: TMenuItem;
    N22: TMenuItem;
    procedure N8Click(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure RefreshBtnClick(Sender: TObject);

    procedure DrawDots;
    procedure DrawFrameDots;

    procedure Load1Click(Sender: TObject);
    procedure OpenImg(FileName:String);
    procedure CropImg;
    procedure CountTiles;
    procedure UpdatePreview;
    procedure UpdateBuff;

    procedure ClearDots;
    procedure ClearFrameDots;

    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BM1Click(Sender: TObject);
    procedure BM2Click(Sender: TObject);
    procedure BM3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBar1Change(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBar2Change(Sender: TObject);

    procedure TabStringGrid(StringGrid:TStringGrid);
    procedure DelStringGrid(StringGrid:TStringGrid);

    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SaveAsdbClick(Sender: TObject);
    procedure ScrollBar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBar1Change(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure N10Click(Sender: TObject);
    procedure PreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PreShapeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AutoListClick(Sender: TObject);

    procedure ResetSets;
    procedure N12Click(Sender: TObject);
    procedure N14Click(Sender: TObject);
    procedure N13Click(Sender: TObject);

    procedure DrawMini(X,Y:Integer);

    procedure GetRasterMapData;

    procedure DelPoint(StringGrid: TStringGrid; DRow: Integer);
    procedure ProjectionsChange(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure N17Click(Sender: TObject);
    procedure N16Click(Sender: TObject);
    procedure N15Click(Sender: TObject);
    procedure N18Click(Sender: TObject);
    procedure N19Click(Sender: TObject);
    procedure KillFrameClick(Sender: TObject);
    procedure ReturnFrameClick(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure N7Click(Sender: TObject);
    procedure BMP1Click(Sender: TObject);
    procedure StringGrid2DblClick(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure DatumsChange(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure OziExplorer1Click(Sender: TObject);

    procedure OpenOzi(LoadImage: Boolean);
    procedure SpeedButton5Click(Sender: TObject);
    procedure CountControlClick(Sender: TObject);
    procedure IsolatedControl;

    procedure N21Click(Sender: TObject);
    procedure OziExplorer2Click(Sender: TObject);
    procedure N22Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseLeave(Sender: TObject);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image2MouseLeave(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddPoint(StringGrid: TStringGrid; X,Y: Integer; B, L :Double; N:Integer);

  public
    ClickMode : integer;
    ZoomI : Integer;
    SK42, WGS, CS : Integer;
    GeoReady, MFound : Boolean;

    ShowReport : Boolean;
    MouseOnMap : Boolean;
    MouseOnPre : Boolean;
  end;

type
  TAffine = record
    a, b, C, D : Double;
  end;

const
  Zoom : array [0..7] of Integer = (4,5,6,8,10,15,20,50);
  Tab : char = #$9 ;
  CDMode = false;
var
  Form1: TForm1;
  FName, ImgFName: String;
  Image: TImage;

  WaitForTab : Boolean;

  Scale: double;
  ImgShift: TPoint;

//  N1,N2,a_L,a_R, SolL,SolR: TPoint;
//  Dop_L,Dop_R:array[1..8] of TPoint;

  MM: TPoint;
  DoMM: boolean;
  Buff1 : TRGBChannel;
  SolText: String ='';

  NX, NY : Integer;  /// ���-�� ������
  PColor : TColor;  /// ���������� ����

  TmpDir, MyDir: String;

  PreX0, PreY0 :integer;

  Coeffs : TAffine;

  MainZone :integer;
implementation

uses NewPoint, GeoReport;

{$R *.dfm}

procedure ClearDir(Const Dir:String);

  procedure AddFiles(Dir:string; var FileList:TStringList);
  var
   SearchRec : TSearchrec; //������ ��� ������
  begin
    if FindFirst(Dir + '*.*', faAnyFile, SearchRec) = 0 then
    begin
      if (SearchRec.Name<> '')
        and(SearchRec.Name <> '.')
        and(SearchRec.Name <> '..')
        and not ((SearchRec.Attr and faDirectory) = faDirectory) then
          FileList.Add(SearchRec.Name);
      while FindNext(SearchRec) = 0 do
        if (SearchRec.Name <> '')
          and(SearchRec.Name <> '.')
          and(SearchRec.Name <> '..')
          and not ((SearchRec.Attr and faDirectory) = faDirectory)  then
             FileList.Add(SearchRec.Name);
      FindClose(Searchrec);
    end;
  end;

var
 FileList : TStringList;
 I:Integer;
begin
  FileList := TStringList.Create;
  AddFiles (Dir, FileList);

    for I := 0 to FileList.Count - 1 do
    try
      DeleteFile(PChar(Dir+FileList[i]));
    except
    end;
    FileList.Destroy
end;


function arctan3(c1,c2: double): double;
begin
    result := arctan2(c1,c2);
    if result <0 then
       result := result + 2*pi;

    if result > 2*pi then
       result := result - 2*pi;
end;

function GetColN(str, sep: string; n: integer): integer;
var j,stl,b :integer;
begin

    Result:=0;
    stl:=0;
    b:=1;

    for j:=1 to n do
     if (copy(Str,j,1)=sep) and (copy(Str,j-1,1)<>sep) then
     begin
       inc(stl);
       b:=j+1;
     end;

    Result:=stl;
end;

function GetCols(str, sep: string; ColN, ColCount:integer; DelSpaces: Boolean): string; overload;
var j,stl,b :integer;
begin

    Result:='';
    stl:=0;
    b:=1;

    for j:=1 to length(Str)+1 do
    Begin

      if ((copy(Str,j,1)=sep)or(j=length(Str)+1))and(copy(Str,j-1,1)<>sep) then
      begin

       if (stl>=ColN) and (Stl<ColN+ColCount) then
       Begin
        if result='' then
          Result:=(Copy(Str,b,j-b))
            else
              Result:=Result+' '+(Copy(Str,b,j-b));
       End;

       inc(stl);
       b:=j+1;

       if stl>ColN+ColCount then
          break;
      end;

    End;

    if result <> '' then
      for j:= length(Result)+1 DownTo 1 do
      begin
        if ((Result[j] = '.') or (Result[j] = ','))and(Result[j]<>sep) then
           Result[j] := DecimalSeparator;
        if DelSpaces then
          if (Result[j] = ' ') then
             Delete(Result,j,1);
      end;
end;


function GetCols(str, sep: string; ColN, ColCount:integer): string; overload;
var j,stl,b :integer;
begin

    Result:='';
    stl:=0;
    b:=1;

    for j:=1 to length(Str)+1 do
    Begin

      if ((copy(Str,j,1)=sep)or(j=length(Str)+1))and(copy(Str,j-1,1)<>sep) then
      begin

       if (stl>=ColN) and (Stl<ColN+ColCount) then
       Begin
        if result='' then
          Result:=(Copy(Str,b,j-b))
            else
              Result:=Result+' '+(Copy(Str,b,j-b));
       End;

       inc(stl);
       b:=j+1;

       if stl>ColN+ColCount then
          break;
      end;

    End;

    if result <> '' then
      for j:= 1 to length(Result)+1 do
        if ((Result[j] = '.') or (Result[j] = ','))and(Result[j]<>sep) then
           Result[j] := DecimalSeparator;
end;


function ExecAndWait(const FileName,
                     Params: ShortString;
                     const WinState: Word): boolean; export; 
var 
  StartInfo: TStartupInfo; 
  ProcInfo: TProcessInformation; 
  CmdLine: ShortString;
begin 
  { �������� ��� ����� ����� ���������, � ����������� ���� �������� � ������ Win9x } 
  CmdLine := '"' + Filename + '" ' + Params; 
  FillChar(StartInfo, SizeOf(StartInfo), #0); 

  with StartInfo do
  begin 
    cb := SizeOf(StartInfo);
    dwFlags := STARTF_USESHOWWINDOW; 
    wShowWindow := WinState; 
  end; 
  Result := CreateProcess(nil, PChar( String( CmdLine ) ), nil, nil, false, 
                          CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, 
                          PChar(ExtractFilePath(Filename)),StartInfo,ProcInfo); 
  { ������� ���������� ���������� } 
  if Result then 
  begin 
    WaitForSingleObject(ProcInfo.hProcess, INFINITE); 
    { Free the Handles } 
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
  end; 
end;

procedure TForm1.N8Click(Sender: TObject);
begin
 close;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//if CheckBox1.Checked then
 // Begin
  { if Button=mbLeft then
   begin
    N1.X:=round(X/Scale[1]+Shifts[1].X);
    N1.Y:=round(Y/Scale[1]+Shifts[1].Y);

    label10.Caption:='N1 - X: '+ inttostr(N1.X) +' Y: ' + inttostr(N1.Y);
   end;  }


   {if Button=mbRight then
   begin

      MyDotL.X:=round(X/Scale[1]+Shifts[1].X);
      MyDotL.Y:=round(Y/Scale[1]+Shifts[1].Y);

      form3.Top:=Image1.Top+Form1.Top+Y+40+Panel1.Height;
      form3.Left:=Image1.Left+Form1.Left+X;

      form3.MyHVal.Value:=H0val.Value;
      get2ndpoint;

      Refresh1.OnClick(Sender);
      Refresh2.OnClick(Sender);

      form3.showmodal;
   end;   }

   

   if Button <> mbLeft then
   exit;

   if ClickMode >= 2 then
   Begin

     Form2.Top := Top + ScrollBox1.Top + Y - 20;
     Form2.Left := Left + ScrollBox1.Left + X - 50;

     if Form2.Left<0 then
      Form2.Left:=0;

     if Form2.Left+Form2.Width > Screen.Width then
        Form2.Left := Screen.Width -  Form2.Width;

     if Form2.Top<0 then
      Form2.Top:=0;

     if Form2.Top+Form2.Height > Screen.Height then
        Form2.Top := Screen.Height -  Form2.Height;


     Form2.X.MaxValue := Image.Picture.Bitmap.Width-1;
     Form2.Y.MaxValue := Image.Picture.Bitmap.Height-1;

     Form2.X.Value := X + ImgShift.X;
     Form2.Y.Value := Y + ImgShift.Y;

     if StaticText4.Caption <>'' then
     begin
       Form2.XX.Text := StaticText4.Caption;
       Form2.YY.Text := StaticText3.Caption;
     end;

     Form2.N := 0;

     try
        Image2.Picture.Bitmap.Canvas.Brush.Color := clBtnShadow;

        Image2.Picture.Bitmap.Canvas.FillRect(Rect(0,0,100,100));
        Image2.Picture.Bitmap.Canvas.CopyRect(Rect(0,0,100,100),
                               Image.Picture.Bitmap.Canvas, Rect(X + ImgShift.X - Zoom[ZoomI]+1,
                               Y + ImgShift.Y - Zoom[ZoomI]+1, X + ImgShift.X + Zoom[ZoomI],
                               Y + ImgShift.Y + Zoom[ZoomI]));
        Image2.Canvas.Pen.Color := clRed;
        Image2.Canvas.MoveTo(Image2.Width div 2, Image2.Height);
        Image2.Canvas.LineTo(Image2.Width div 2, 0);
        Image2.Canvas.MoveTo(0, Image2.Height div 2);
        Image2.Canvas.LineTo(Image2.Width , Image2.Height div 2);

        Form2.Image1.Picture.Bitmap.Assign(Image2.Picture.Bitmap);
     except
     end;

     Form2.ShowModal;
   End;

  RefreshBtn.OnClick(Sender);
  
  { if checkbox3.Checked then
   begin
      N2.X:=N1.X;
      N2.Y:=N1.Y;
       Refresh2.OnClick(Sender);
   end; }
 //end;
end;

procedure TForm1.Image2MouseLeave(Sender: TObject);
begin
    MouseOnPre := false;
end;

procedure TForm1.Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
    MouseOnPre := true
end;

procedure TForm1.IsolatedControl;
var i, j, K : integer;

    PointCount, Zone, CoefCount : integer;

    ImgX, ImgY, MapX, MapY : array [0..100] of Double;
    a, b, C, D, SF, beta : array [0..2000] of Double; //// Coefficients
    aa, ab, aC, aD, aSF, abeta, RMS, Err : Double; //// Average results

    //CCoeffs : TAffine;
    MapB, MapL, _B, _L, _h, _MapX, _MapY : Double; //// TMP Data
begin
  ///
  with Form3 do
  Begin
    Memo1.Lines.Add('');
    Memo1.Lines.Add('------');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('������������� �������� �������� �����:');
    Memo1.Lines.Add('');
  End;

  //// ���� 1. �������� ������� �����, �������� ���������� � �������� ��� �������������

 // Form3.Memo1.Lines.Add('�����: ');
 //Form3.Memo1.Lines.Add('');
  PointCount := StringGrid1.RowCount-1;

  if PointCount > 100 then
    PointCount := 100;

  for I := 1 to PointCount do
  Begin
    ImgX[I-1] := StrToFloat(StringGrid1.Cells[1,I]);
    ImgY[I-1]  := StrToFloat(StringGrid1.Cells[2,I]);

    case Projections.ItemIndex of
      0:
      begin
         _L := StrToFloat(StringGrid1.Cells[3,I]);
         _B  := StrToFloat(StringGrid1.Cells[4,I]);

         if CS <> WGS then
            Geo1ForceToGeo2(StrToFloat(StringGrid1.Cells[4,I]),
                            StrToFloat(StringGrid1.Cells[3,I]),0,
                            CS, WGS, _B, _L, _h);

         GeoToUTM(WGS, _B, _L, false, MapB, MapL, zone, i=1);

      end;
      1,2:
      begin
         MapL  := StrToFloat(StringGrid1.Cells[3,I]);
         MapB  := StrToFloat(StringGrid1.Cells[4,I]);
      end;
    end;

    MapX[I-1] := MapL;
    MapY[I-1]  := - MapB;

   { Form3.Memo1.Lines.Add(StringGrid1.Cells[0,I]+ '  '
                          + format('%n',[ImgX[I-1]]) + '  '
                          + format('%n',[ImgY[I-1]]) + '  '
                          + format('%n',[MapX[I-1]]) + '  '
                          + format('%n',[-MapY[I-1]]) + '  ');}
  End;

  //// ���� 2. C����� ��������� ��� ������ �����
  CoefCount := 0;

  {Form3.Memo1.Lines.Add('');
  Form3.Memo1.Lines.Add('------');
  Form3.Memo1.Lines.Add('');
  Form3.Memo1.Lines.Add('������ ��������: ');
  Form3.Memo1.Lines.Add('');}

  for K := 0 to PointCount - 1 do
  BEGIN


    For I := 0 To  PointCount -2 Do
    For J := I+1 To  PointCount -1 Do
    Begin
       //
      if (I=K)or(J=K) then
         continue;
         
      SF[CoefCount] := ( sqrt ( sqr(ImgX[j]-ImgX[i]) + sqr(ImgY[j]-ImgY[i]) )
                / sqrt ( sqr(MapY[j]-MapY[i]) + sqr(MapX[j]-MapX[i]) ));

      beta[CoefCount] := -(arctan3((ImgY[j]-ImgY[i]),(ImgX[j]-ImgX[i])) - arctan3((MapY[j]-MapY[i]),(MapX[j]-MapX[i])));


      if beta[CoefCount] < -pi then
         beta[CoefCount] := beta[CoefCount] + 2*pi;
      if beta[CoefCount] > pi then
         beta[CoefCount] := beta[CoefCount] - 2*pi;


      a[CoefCount] := 1/SF[CoefCount]*Cos(beta[CoefCount]);
      b[CoefCount] := 1/SF[CoefCount]*Sin(beta[CoefCount]);

      C[CoefCount] := MapX[i] - (a[CoefCount]*ImgX[i] - b[CoefCount]*ImgY[i]);
      D[CoefCount] := MapY[i] - (a[CoefCount]*ImgY[i] + b[CoefCount]*ImgX[i]);


      {Form3.Memo1.Lines.Add(StringGrid1.Cells[0,I+1] + ' - ' + StringGrid1.Cells[0,J+1]);
      Form3.Memo1.Lines.Add('  �������: ' + format('%.4f',[SF[CoefCount]]));

      Form3.Memo1.Lines.Add('  ����: a1: '
                            + format('%.4f', [(arctan3((ImgY[j]-ImgY[i]),(ImgX[j]-ImgX[i])))*180/pi])
                            +'  a2: '+
                            format('%.4f',[(arctan3((MapY[j]-MapY[i]),(MapX[j]-MapX[i])))*180/pi])
                            +'   ��������: '+
                            format('%.4f',[(beta[CoefCount]*180/pi)]));

      Form3.Memo1.Lines.Add('  �����: X: ' + format('%n',[C[CoefCount]]) + '  Y: ' + format('%n',[-D[CoefCount]]));
                    }
      inc(CoefCount);
    End;


  //// ���� 2b. C����� ������� ���������

  //aa:= 0; ab :=0;
    aC := 0; aD := 0;
    aBeta := 0; aSF := 0;

    For i := 0 to CoefCount-1 Do
    Begin
     aSF := aSF + SF[I]/CoefCount;
     aBeta := aBeta + Beta[I]/CoefCount;
     aC := aC + C[I]/CoefCount;
     aD := aD + D[I]/CoefCount;
    End;

    aa := 1/aSF*Cos(abeta);
    ab := 1/aSF*Sin(abeta);

    if CDMode then
    Begin
     aC := MapX[1] - (aa*ImgX[1] - ab*ImgY[1]);
     aD := MapY[1] - (aa*ImgY[1] + ab*ImgX[1]);
    End;

  {aC := MapX[0] - (aa*ImgX[0] - ab*ImgY[0]);
  aD := MapY[0] - (aa*ImgY[0] + ab*ImgX[0]); }

  //// ���� 3. ����������

   { CCoeffs.a := aa;  Coeffs.b := ab;
    CCoeffs.C := aC;  Coeffs.D := aD;}

  //// ���� 5. �����

    with Form3 do
    begin

      _mapX := aa*imgX[k] - ab*imgY[k] + aC   - mapX[k];
      _mapY := ab*imgX[k] + aa*imgY[k] + aD   - mapY[k];

      Err := Sqrt(Sqr(_mapX)+Sqr(_mapY));
    //  RMS := RMS + Err*Err;

      Memo1.Lines.Add(StringGrid1.Cells[0,k+1]+': ');
      Memo1.Lines.Add('   ' +  format('%n',[ Err ])+' �');

  //    Err := Err*aSF;

      _mapX :=(aa*MapX[k] + ab*MapY[k] - ab*aD - aa*aC)/(aa*aa + ab*ab);
      _mapY :=(MapY[k] - ab*_MapX - aD)/aa;

      Err := Sqrt(Sqr(ImgX[k]-_mapX)+Sqr(ImgY[k]-_mapY));

      Memo1.Lines.Add('   ' +  format('%n', [Err])+' ����.');
      Memo1.Lines.Add('');

    //Showmodal;
    end;

  END;

end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var _x, _y, B, L, h : double;
begin
  MouseOnMap := true;
  SetFocus;


  if ClickMode = -1 then
    exit;

  if ClickMode=1 then
    Image1.Cursor := crSizeAll;

  if TrackBar1.Enabled then
      TrackBar1.SetFocus;

  StaticText1.Caption := IntToStr(trunc(ImgShift.x) + X);
  StaticText2.Caption := IntToStr(trunc(ImgShift.y) + Y);

  if (ssMiddle in Shift) or (ssRight in Shift) or ((ssLeft in Shift)and(ClickMode=1))  then
  begin

   Image1.Cursor := crSizeAll;

   DoMM :=true;

   ImgShift.x:=round(ImgShift.x+(MM.X-X));
   ImgShift.y:=round(ImgShift.y+(MM.Y-Y));

   MM.X:=X;
   MM.Y:=Y;

   RefreshBtn.OnClick(Sender);

   ScrollBar1.Position:=round(100*ImgShift.X/(Image.Picture.Width -Image1.Width /Scale));
   ScrollBar2.Position:=round(100*ImgShift.Y/(Image.Picture.height-Image1.Height/Scale));


  end
   else
   begin
     DoMM := false;
     if ClickMode<>1 then
       Image1.Cursor := crCross;
   end;

  MM.X:=X;
  MM.Y:=Y;

  DrawMini(X + ImgShift.X ,Y + ImgShift.Y );

  if GeoReady then
  Begin
   _X := Coeffs.a*(X+ ImgShift.X) - Coeffs.b*(Y+ ImgShift.Y) + Coeffs.C;
   _Y := - (-Coeffs.b*(X+ ImgShift.X) + Coeffs.a*(Y+ ImgShift.Y) + Coeffs.D);

   StaticText3.Caption := format('%n',[_y]);
   StaticText4.Caption := format('%n',[_x]);

   if Projections.ItemIndex = 0 then
   Begin
       UTMToGeo(WGS,_y,_x, false, B, L);

       if CS<>WGS then
       begin
         _x := B;
         _y := L;
         Geo1ForceToGeo2(_x, _y, 0, WGS, CS, B, L, h)
       end;

       StaticText3.Caption := DegToDMS(B,5);
       StaticText4.Caption := DegToDMS(L,5);
   End;

   

   ///
  End;
end;

procedure SetPageSize(Value: integer; ScrollBar: TScrollBar);
var
  ScrollInfo: TScrollInfo;
begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPage := Value;
  ScrollInfo.fMask := SIF_PAGE;
  SetScrollInfo(ScrollBar.Handle, SB_CTL, ScrollInfo, True);
end;

procedure TForm1.RefreshBtnClick(Sender: TObject);
begin

  Form1.Refresh;

  if image = nil then
  exit;

  if image<>nil then
  try
     if  Image1.Picture.Bitmap.Height <>  Image1.Height then
         Image1.Picture.Bitmap.Height:=Image1.Height;
     if  Image1.Picture.Bitmap.Width <>  Image1.Width then
         Image1.Picture.Bitmap.Width:=Image1.Width;


     if DoMM=true then
     Begin

      if ImgShift.X<0 then
        ImgShift.X:=0;
      if ImgShift.Y<0 then
        ImgShift.Y:=0;
      if ImgShift.X>trunc(Image.Picture.Width-Image1.Width/Scale) then
        ImgShift.X:=trunc(Image.Picture.Width-Image1.Width/Scale);
      if ImgShift.Y>trunc(Image.Picture.Height-Image1.Height/Scale) then
        ImgShift.Y:=trunc(Image.Picture.Height-Image1.Height/Scale);

      if (Image.Picture.Width-Image1.Width/Scale)<0 then
          ImgShift.X:=0;
      if (Image.Picture.Height-Image1.Height/Scale)<0 then
          ImgShift.Y:=0;


          
     end else
     begin
       if (Image.Picture.Width-Image1.Width/Scale)>0 then
          ImgShift.X:=trunc(ScrollBar1.Position/100*(Image.Picture.Width-Image1.Width/Scale))
          else  ImgShift.X:=0;
       if (Image.Picture.Height-Image1.Height/Scale)>0 then
          ImgShift.Y:=trunc(ScrollBar2.Position/100*(Image.Picture.Height-Image1.Height/Scale))
          else  ImgShift.Y:=0;
     end;


    // DoMM:=false;

    with  Image1.Canvas do
    Begin
      Brush.Color:=clBtnFace;
      FillRect(Rect(0,0,Image1.Width,Image1.Height));

      if Image<> nil then
      begin

        if Buff1.Active then
        begin
          SetStretchBltMode(Handle, ColorOnColor);

          BitBlt(Handle, -ImgShift.X,-ImgShift.Y, Image.Picture.Width, Image.Picture.Height,
              Buff1.Canvas.Handle, 0, 0, SRCCOPY);

          {StretchBlt(Handle, trunc(-ImgShift.X*Scale),trunc(-ImgShift.Y*Scale),
              trunc((Image.Picture.Width)*Scale),
              trunc((Image.Picture.Height)*Scale),
              Buff1.Canvas.Handle, 0, 0, Buff1.Size.X, Buff1.Size.Y, SRCCOPY);}
        end;

       if ClickMode < 3 then
       begin
        DrawFrameDots;
        DrawDots;
       end
        else
          begin
             DrawDots;
             DrawFrameDots;
          end;

      end;
    end;


    if Panel3.Visible then
    Try

      PreShape.Top := MMap.Top + PreY0 div 2 + 1 + trunc(ImgShift.Y/Image.Picture.Height*(200-PreY0));
      PreShape.Left := MMap.Left + PreX0 div 2 + 1 + trunc(ImgShift.X/Image.Picture.Width*(200-PreX0));

      PreShape.Width := trunc((200-PreX0)*Image1.Width/Image.Picture.Width);
      PreShape.Height := trunc((200-PreY0)*Image1.Height/Image.Picture.Height);

      if PreShape.Left >  MMap.Left + MMap.Width then
        PreShape.Left :=  MMap.Left + MMap.Width;

      if PreShape.Top > MMap.Top + MMap.Height then
         PreShape.Top := MMap.Top + MMap.Height;

      if PreShape.Top + PreShape.Height > MMap.Top + MMap.Height then
        PreShape.Height := MMap.Height -  (PreShape.Top - MMap.Top);
      if PreShape.Left + PreShape.Width > MMap.Left + MMap.Width then
        PreShape.Width := MMap.Width -  (PreShape.Left - MMap.Left);
    Except
    End;
  except
  end;
end;

procedure TForm1.Load1Click(Sender: TObject);
var i :integer;
begin
  if OpenDialog1.Execute then
    try

      ResetSets;

      OpenImg(OpenDialog1.FileName);

      Panel1.Visible := false;
      Panel7.Visible := false;

      StaticText3.Caption := '';
      StaticText4.Caption := '';

      Fname := OpenDialog1.FileName;

      for i := Length(FName) DownTo 1 Do
        if FName[i]='\' then
         break;
      ImgFName := Copy(Fname,I+1,Length(Fname)-I);

      UpdateBuff;

      ClearDots;

      ClearFrameDots;

      Label5.Visible := false;
    except
      MessageDlg('������ �������� �����: '+#13+OpenDialog1.FileName,mtError,[mbOk],1);
    end;

  GeoReady := false;
  MFound := false;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;

  RefreshBtn.OnClick(Sender);
  UpdatePreview;
end;

procedure TForm1.OpenImg(FileName: String);
var ext : string;
    GIF : TGIFImage;
    TGA : TTargaGraphic;
    PNG : TPNGGraphic;
    TIFF : TTIFFGraphic;
    JPG : TJpegImage;
begin

 if Filename = '' then
    exit;

  ext := copy(Filename,Length(Filename)-2,3);
  ext:= Lowercase(ext);

  if (ext= 'bmp')or(ext = 'dib') then
     Image.Picture.LoadFromFile(Filename);

  if (ext='jpg')or(ext='peg') then
  Begin
     JPG := TJpegImage.Create;
     JPG.LoadFromFile(Filename);
     Image.Picture.Bitmap.Assign(JPG);
     JPG.Destroy
  End;

  if ext = 'png' then
  begin
    PNG := TPNGGraphic.Create;
    PNG.LoadFromFile(Filename);
    Image.Picture.Bitmap.Assign(PNG);
    PNG.Destroy;
  end;

  if ext = 'tga' then
  begin
    TGA := TTARGAGraphic.Create;
    TGA.LoadFromFile(Filename);
    Image.Picture.Bitmap.Assign(TGA);
    TGA.Destroy;
  end;

  if ext = 'iff' then
  begin
    TIFF := TTIFFGraphic.Create;
    TIFF.LoadFromFile(Filename);
    Image.Picture.Bitmap.Assign(TIFF);
    TIFF.Destroy;
  end;

  if ext = 'gif' then
  begin
    GIF := TGIFImage.Create;
    GIF.LoadFromFile(Filename);
    Image.Picture.Bitmap.Assign(GIF);
    GIF.Destroy;
  end;

  

  Panel3.Visible := True;
  PreX0 :=0;
  PreY0 :=0;

  try
    if Image.Picture.Bitmap.Width >  image.Picture.Bitmap.Height then
      PreY0 := 200 - trunc(Image.Picture.Bitmap.Height/Image.Picture.Bitmap.Width*200)
    else
      if Image.Picture.Bitmap.Width >  image.Picture.Bitmap.Height  then
          PreX0 := 200 - trunc(Image.Picture.Bitmap.Width/Image.Picture.Bitmap.Height*200);


    UpdatePreview;

    PreShape.Visible := true;

  except
  end;

  ClickMode := 1;
  BM1.Enabled := True;
  BM2.Enabled := True;
  BM3.Enabled := True;
  BM1.Flat := False;
  BM2.Flat := True;
  BM3.Flat := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var I:Integer;
begin
  GeoInit('Data\Sources.loc','','������ �������� ������ ���������!');
  DoubleBuffered:=True;

 for I := 0 to ComponentCount-1 do
    if Components[I] is TPanel  then
      TPanel(Components[I]).ControlStyle := ControlStyle-[csParentBackGround];

  if  Datums.Items.Count = 0 then
  begin
        for I := 0 to Length(DatumList)-1 do
          if DatumList[i].Hidden = false then
          
          Datums.Items.Add(DatumList[i].Caption);
        Datums.ItemIndex := 0;
  end;

  SK42 := FindDatum('SK42');
  WGS := FindDatum('WGS84') ;

  MyDir :=  GetCurrentDir +'\';
  TMPDir := GetCurrentDir +'\Tmp\';
  if not FileExists(TMPDir) then
    ForceDirectories(TMPDir);

  Image := TImage.Create(self);

  Buff1 := TRGBChannel.Create;

  Scale := 1;

  ImgShift.X := 0;
  ImgShift.Y := 0;

  ZoomI := 4;
  ClickMode := -1;

  GeoReady := false;
  MFound := False;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearDir(TmpDir);
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var step:integer;
begin
  if MouseOnMap then
  begin
   if ScrollBar2.Position < 100 then
   begin
      step := round(0.2*Image.Picture.Height/(Image1.Height/Scale));
      if step < 1 then
         step := 1;
      ScrollBar2.Position := ScrollBar2.Position + step;
      ScrollBar2.OnChange(nil);
      Image1.OnMouseMove(nil,[],MM.x,MM.Y);
   end;
  end else
    if MouseOnPre then
      SpeedButton10.Click;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var step:integer;
begin
  if MouseOnMap then
  begin
   if ScrollBar2.Position > 0 then
   begin
      step := round(0.2*Image.Picture.Height/(Image1.Height/Scale));
      if step < 1 then
         step := 1;
      ScrollBar2.Position := ScrollBar2.Position - step;
      ScrollBar2.OnChange(nil);
      Image1.OnMouseMove(nil,[],MM.x,MM.Y);
   end;
 end
  else
   if MouseOnPre then
      SpeedButton11.Click;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbMiddle) or (Button=mbRight) or ((ClickMode=1)and(Button=mbLeft)) then
  begin
    MM.X:=X;
    MM.Y:=Y;
  end;
  if Label5.Visible then
   if Button = mbRight then
     Label5.OnClick(nil);
end;

procedure TForm1.Image1MouseLeave(Sender: TObject);
begin
    MouseOnMap := false
end;

procedure TForm1.BM1Click(Sender: TObject);
begin
 clickMode := 1;

 BM1.Flat := false;
 BM2.Flat := true;
 BM3.Flat := true;

 Panel1.Visible := false;
 Panel7.Visible := false;
 Label5.Visible := false;

 RefreshBtn.OnClick(Sender);
end;

procedure TForm1.BM2Click(Sender: TObject);
begin
 clickMode := 2;

 BM1.Flat := true;
 BM2.Flat := false;
 BM3.Flat := true;

 Panel1.Visible := true;
 Panel7.Visible := false;
 if label5.Caption <>'' then
   Label5.Visible := true;

 RefreshBtn.OnClick(Sender);
end;

procedure TForm1.BM3Click(Sender: TObject);
begin
 clickMode := 3;

 BM1.Flat := true;
 BM2.Flat := true;
 BM3.Flat := false;

 Panel7.Visible := true;
 Panel1.Visible := false;
 if label5.Caption <>'' then
   Label5.Visible := true;

 RefreshBtn.OnClick(Sender);
end;

procedure TForm1.FormResize(Sender: TObject);
var i :integer;
begin

  for i:= 0 to StringGrid1.ColCount-1 do
    StringGrid1.ColWidths[i] := (StringGrid1.Width - 30) div StringGrid1.ColCount;


  for i:= 0 to StringGrid2.ColCount-1 do
    StringGrid2.ColWidths[i] := (StringGrid2.Width - 30) div StringGrid2.ColCount;


 try
   RefreshBtn.Click;
 except
 end;
end;



procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if StringGrid1.RowCount > 1 then
  begin

    Image.Destroy;
    Buff1.Destroy;
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  DoMM:=false;
  if TrackBar1.Position<0 then
    Scale:=1/(-TrackBar1.Position)
      else  if TrackBar1.Position>0 then
        Scale:=TrackBar1.Position
           else
              Scale:=1;

  if Scale<1 then
  Label1.Caption:='x'+format('%.2f',[Scale])
    else
      Label1.Caption:='x'+IntToStr(round(Scale));

  RefreshBtn.OnClick(Sender);
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
//  DoMM:=false;
  RefreshBtn.OnClick(Sender);
end;

procedure TForm1.ScrollBar2Change(Sender: TObject);
begin
// DoMM:=false;
 RefreshBtn.OnClick(Sender);
end;

type TGridCracker = class(TStringGrid);

procedure SetCaretPosition(Grid: TStringGrid; col, row, x_pos: Integer);
begin
  Grid.Col := Col;
  Grid.Row := Row;
  with TGridCracker(Grid) do
  InplaceEditor.SelStart := x_pos;
end;

procedure TForm1.DelStringGrid(StringGrid: TStringGrid);
var i, j :integer;
    needDel :Boolean;
begin
  NeedDel:= true;

  if (StringGrid.RowCount < 3) or (StringGrid.Row < 2) then
   exit;


  if StringGrid.Cells[StringGrid.Col,StringGrid.Row]='' then
  Begin

      if  StringGrid.Col > 0 then
        StringGrid.Col :=  StringGrid.Col-1
          else
          Begin

               for i:= 0 to StringGrid.ColCount-1 do
                 if StringGrid.Cells[i,StringGrid.Row]<>'' then
                   begin
                     NeedDel :=false;
                     Break;
                   end;
                if NeedDel then
                begin
                  for i:= StringGrid.Row to StringGrid.RowCount-1 do
                  for j:= 0 to StringGrid.ColCount-1 do
                     StringGrid.Cells[j,i] :=
                        StringGrid.Cells[j,i+1];

                  if StringGrid.Row <> StringGrid.RowCount-1 then
                    StringGrid.Row := StringGrid.Row -1;
                  StringGrid.RowCount := StringGrid.RowCount-1;
                  
                end
                 else
                   StringGrid.Row := StringGrid.Row -1;

              StringGrid.Col :=  StringGrid.ColCount-1;

          End;

      StringGrid.Cells[StringGrid.Col,StringGrid.Row] :=  StringGrid.Cells[StringGrid.Col,StringGrid.Row] + ' ';
      SetCaretPosition(StringGrid,StringGrid.Col,StringGrid.Row,length( StringGrid.Cells[StringGrid.Col,StringGrid.Row]));
  End;
end;

procedure TForm1.TabStringGrid(StringGrid: TStringGrid);
begin
  with StringGrid do
  if Col < ColCount-1 then
    Col:=Col+1
    else
    begin
      Col:=0;
      if Row >= RowCount-1 then
        RowCount := RowCount +1;
      Row:=Row+1;
    end;

     // StringGrid.fo
end;


procedure TForm1.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (Key= vk_Tab) or (Key = vk_Return) then
   TabStringGrid(StringGrid1);
 if (Key= vk_Delete) or (Key = vk_Back) then
   DelStringGrid(StringGrid1);
end;

procedure TForm1.StringGrid2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key= vk_Tab) or (Key = vk_Return) then
   TabStringGrid(StringGrid2);
 if (Key= vk_Delete) or (Key = vk_Back) then
   DelStringGrid(StringGrid2);
end;

procedure TForm1.CropImg;
var X,Y, i,j :integer;
  ExtendImg, CropImg, SmallImg, TinyImg: TBitMap;
begin
  if Image.Picture = nil then
   exit;

  ExtendImg:= TBitMap.Create;
  CropImg  := TBitMap.Create;
  SmallImg := TBitMap.Create;
  TinyImg  := TBitMap.Create;

  ExtendImg.Assign(Image.Picture.Graphic);

  X :=  ExtendImg.Width;
  Y :=  ExtendImg.Height;

  if X mod 512 <> 0 then
     ExtendImg.Width := trunc(X/512 + 1)*512;

  if Y mod 512 <> 0 then
     ExtendImg.Height := trunc(Y/512 + 1)*512;

  NX := trunc(ExtendImg.Width/512);
  NY := trunc(ExtendImg.Height/512);

  ExtendImg.Canvas.Brush.Color := clFuchsia;
  ExtendImg.Canvas.FillRect(Rect(X,0,ExtendImg.Width,ExtendImg.Height));
  ExtendImg.Canvas.FillRect(Rect(0,Y,ExtendImg.Width,ExtendImg.Height));

  PColor := ClFuchsia;

  if (X < ExtendImg.Width)or(Y < ExtendImg.Height)
   then
     PColor := ExtendImg.Canvas.Pixels[ExtendImg.Width-1,ExtendImg.Height-1];

  CropImg.Width  := 512;
  CropImg.Height := 512;
  SmallImg.Width  := 128;
  SmallImg.Height := 128;
  TinyImg.Width  := 32;
  TinyImg.Height := 32;

  for i := 0 to NX -1 do
    for j := 0 to NY -1 do
     begin
       CropImg.Canvas.CopyRect(Rect(0,0,512,512),
                               ExtendImg.Canvas, Rect(i*512, j*512, (i+1)*512, (j+1)*512));

       CropImg.SaveToFile(TmpDir+IntToStr(i)+'_'+IntToStr(j)+'.bmp');



       SetStretchBltMode(SmallImg.Canvas.Handle, HalfTone);

       StretchBlt(SmallImg.Canvas.Handle, 0, 0, 128 , 128,
               CropImg.Canvas.Handle,
               0, 0, 512, 512, SRCCopy);

       SmallImg.SaveToFile(TmpDir+IntToStr(i)+'_'+IntToStr(j)+'_s.bmp');

       SetStretchBltMode(TinyImg.Canvas.Handle, HalfTone);

       StretchBlt(TinyImg.Canvas.Handle, 0, 0, 32 , 32,
               CropImg.Canvas.Handle,
               0, 0, 512, 512, SRCCopy);

       TinyImg.SaveToFile(TmpDir+IntToStr(i)+'_'+IntToStr(j)+'_t.bmp');
     end;

  ExtendImg.SaveToFile(TmpDir+'current.bmp');

  ExtendImg.Destroy;
  CropImg.Destroy;
end;

procedure TForm1.SaveAsdbClick(Sender: TObject);
var
 PxFm: TPxFm;
 pSize: TPoint;
 Image: TBitmap;
 Dest: TBitmap;
 Tolerance: Integer;
 MaskColor: Cardinal;
 IsMasked: Boolean;
 Stream: TMemoryStream;
 FStream: TFileStream;
 Asdb :TASdb;

 AName : String;
 i, j : integer;
 iMax : integer;
begin

 if not GeoReady then
 exit;

 Savedialog2.FileName := ImgFName+'.asdb';

 if not Savedialog2.Execute then
   exit;

 AName := Savedialog2.FileName;

 if Copy(AName, Length(Aname)-4,5)<>'.asdb' then
     AName := AName + '.asdb';

    
 if fileexists(AName) then
 Begin
   if MessageDlg('������������ ������������ ����? '+#13+AName, MtConfirmation, [mbYes, mbNo],0) <> 6 then
     exit;
 End;

 PB.Visible := true;
 PB.Position := 0;

 Repaint;
 CropImg;
 CountTiles;
 PB.Position := 10;
 Repaint;

 Asdb := TAsdb.Create(Form1);

 // update VTDb archive
 ASDb.FileName:= AName;

 if (not ASDb.Update()) then
  begin
   ShowMessage('Failed opening ASDb archive!');
   Exit;
  end;

 // change the following format, if necessary
 PxFm.Format:= COLOR_A8R8G8B8;

 // retreive Texture Size from edit boxes
 PxFm.TextureWidth := 512;
 PxFm.TextureHeight:= 512;

 // retreive Pattern Size from edit boxes
 PxFm.PatternWidth := 512;
 PxFm.PatternHeight:= 512;

 // this variable is used for better readability only
 pSize:= Point(PxFm.PatternWidth, PxFm.PatternHeight);

 // this size can be smaller than pattern size to add padding
 PxFm.VisibleWidth := PxFm.PatternWidth;
 PxFm.VisibleHeight:= PxFm.PatternHeight;

 // retreive mask color and tolerance
 IsMasked:= true;
 MaskColor:= PColor and $FFFFFF;
 Tolerance:= 1;

 repaint;

 for i := 0 to NX-1 do
    for j := 0 to NY-1 do
    Begin
      // load source bitmap
      Image:= TBitmap.Create();
      if (not LoadBitmap(TmpDir+IntToStr(i)+'_'+IntToStr(j)+'.bmp', Image, ifAuto)) then
      begin
        ShowMessage('Failed loading source bitmap!');
        Image.Free();
        //Exit;
        Continue;
      end;

      PxFm.PatternCount:= (Image.Width div pSize.X) * (Image.Height div pSize.Y);

      Dest:= TBitmap.Create();
      TileBitmap(Dest, Image, Point(PxFm.TextureWidth, PxFm.TextureHeight),
          pSize, pSize, IsMasked, MaskColor, Tolerance);

      Image.Free();

      PxFm.TextureCount:= Dest.Height div PxFm.TextureHeight;

      Stream:= TMemoryStream.Create();
      WriteBitmapPxFm(Stream, Dest, PxFm);

      Dest.Free();

      Stream.Seek(0, soFromBeginning);

      // write PxFm-formatted image data to ASDb
      if (not ASDb.WriteStream(ImgFName+'_'+IntToStr(i)+'_'+IntToStr(j), Stream, recGraphics)) then
      begin
        ShowMessage('Failed writing stream to VTDb archive.');
      end; {else ShowMessage(Edit2.Text + ' key added!');}

      Stream.Free();

      PB.Position := trunc((i*NY+j)/NX/NY*50+10);
      PB.Repaint;
   end;


 // ADD SMALL
 PxFm.Format:= COLOR_A8R8G8B8;
 PxFm.TextureWidth := 128;
 PxFm.TextureHeight:= 128;
 PxFm.PatternWidth := 128;
 PxFm.PatternHeight:= 128;
 pSize:= Point(PxFm.PatternWidth, PxFm.PatternHeight);
 PxFm.VisibleWidth := PxFm.PatternWidth;
 PxFm.VisibleHeight:= PxFm.PatternHeight;
 IsMasked:= true;
 MaskColor:= PColor and $FFFFFF;
 Tolerance:= 1;
 repaint;

 for i := 0 to NX-1 do
    for j := 0 to NY-1 do
    Begin
      Image:= TBitmap.Create();
      if (not LoadBitmap(TmpDir+IntToStr(i)+'_'+IntToStr(j)+'_s.bmp', Image, ifAuto)) then
      begin
        ShowMessage('Failed loading source bitmap!');
        Image.Free();
        Continue;
      end;

      PxFm.PatternCount:= (Image.Width div pSize.X) * (Image.Height div pSize.Y);
      Dest:= TBitmap.Create();
      TileBitmap(Dest, Image, Point(PxFm.TextureWidth, PxFm.TextureHeight),
          pSize, pSize, IsMasked, MaskColor, Tolerance);
      Image.Free();
      PxFm.TextureCount:= Dest.Height div PxFm.TextureHeight;
      Stream:= TMemoryStream.Create();
      WriteBitmapPxFm(Stream, Dest, PxFm);
      Dest.Free();
      Stream.Seek(0, soFromBeginning);

      // write PxFm-formatted image data to ASDb
      if (not ASDb.WriteStream(ImgFName+'_'+IntToStr(i)+'_'+IntToStr(j)+'_s', Stream, recGraphics)) then
      begin
        ShowMessage('Failed writing stream to VTDb archive.');
      end; {else ShowMessage(Edit2.Text + ' key added!');}

      Stream.Free();
      PB.Position := trunc((i*NY+j)/NX/NY*30+60);
      PB.Repaint;
   end;

 // ADD TINY
 PxFm.Format:= COLOR_A8R8G8B8;
 PxFm.TextureWidth := 32;
 PxFm.TextureHeight:= 32;
 PxFm.PatternWidth := 32;
 PxFm.PatternHeight:= 32;
 pSize:= Point(PxFm.PatternWidth, PxFm.PatternHeight);
 PxFm.VisibleWidth := PxFm.PatternWidth;
 PxFm.VisibleHeight:= PxFm.PatternHeight;
 IsMasked:= true;
 MaskColor:= PColor and $FFFFFF;
 Tolerance:= 1;
 repaint;

 for i := 0 to NX-1 do
    for j := 0 to NY-1 do
    Begin
      Image:= TBitmap.Create();
      if (not LoadBitmap(TmpDir+IntToStr(i)+'_'+IntToStr(j)+'_t.bmp', Image, ifAuto)) then
      begin
        ShowMessage('Failed loading source bitmap!');
        Image.Free();
        Continue;
      end;

      PxFm.PatternCount:= (Image.Width div pSize.X) * (Image.Height div pSize.Y);
      Dest:= TBitmap.Create();
      TileBitmap(Dest, Image, Point(PxFm.TextureWidth, PxFm.TextureHeight),
          pSize, pSize, IsMasked, MaskColor, Tolerance);
      Image.Free();
      PxFm.TextureCount:= Dest.Height div PxFm.TextureHeight;
      Stream:= TMemoryStream.Create();
      WriteBitmapPxFm(Stream, Dest, PxFm);
      Dest.Free();
      Stream.Seek(0, soFromBeginning);

      // write PxFm-formatted image data to ASDb
      if (not ASDb.WriteStream(ImgFName+'_'+IntToStr(i)+'_'+IntToStr(j)+'_t', Stream, recGraphics)) then
      begin
        ShowMessage('Failed writing stream to VTDb archive.');
      end; {else ShowMessage(Edit2.Text + ' key added!');}

      Stream.Free();
      PB.Position := trunc((i*NY+j)/NX/NY*10+90);
      PB.Repaint;
   end;


 FStream := TFileStream.Create(Tmpdir+ImgFName+'.txt', fmOpenRead);
 Asdb.WriteStream(ImgFName+'.txt', FStream,  recFile);

 Fstream.Free;
 Asdb.Free;
 //Asdb.Destroy;

 PB.Visible := false;
 PB.Position := 0;

 ShowMessage('���� ASDB �������� �������!');
end;


procedure TForm1.ScrollBar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
 DoMM:=false;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
 DoMM:=false;
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
begin
 Panel3.Visible := False;
 N10.Checked := Panel3.Visible;
 
 RefreshBtn.OnClick(nil);
end;

procedure TForm1.N10Click(Sender: TObject);
begin
  Panel3.Visible := not Panel3.Visible;
  N10.Checked := Panel3.Visible;

  RefreshBtn.OnClick(nil);
end;

procedure TForm1.PreviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   ImgShift.Y := Trunc( (Y-PreY0/2 - PreShape.Height/2 )*Image.Picture.Height/(200-PreY0) );
   ImgShift.X := Trunc( (X-PreX0/2 - PreShape.Width/2 )*Image.Picture.Width/(200-PreX0) );

   ScrollBar1.Position:=round(100*ImgShift.X/(Image.Picture.Width -Image1.Width /Scale));
   ScrollBar2.Position:=round(100*ImgShift.Y/(Image.Picture.height-Image1.Height/Scale));

   DoMM := false;
   RefreshBtn.OnClick(nil);
end;



procedure TForm1.PreShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   ImgShift.Y := Trunc( (Y-PreY0/2+ PreShape.Top - MMap.Top - PreShape.Height/2 )*Image.Picture.Height/(200-PreY0) );
   ImgShift.X := Trunc( (X-PreX0/2+ PreShape.Left - MMap.Left - PreShape.Width/2 )*Image.Picture.Width/(200-PreX0) );

   ScrollBar1.Position:=round(100*ImgShift.X/(Image.Picture.Width -Image1.Width /Scale));
   ScrollBar2.Position:=round(100*ImgShift.Y/(Image.Picture.height-Image1.Height/Scale));

   DoMM := false;
   RefreshBtn.OnClick(nil);
end;

procedure TForm1.PreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
     Preview.OnMouseUp(nil,mbLeft,[],X,Y);
end;

procedure TForm1.PreShapeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if ssLeft in Shift then
     PreShape.OnMouseUp(nil,mbLeft,[],X,Y);
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
begin
 SpeedButton11.Enabled := true;
 
 if ZoomI > 0 then
 dec(ZoomI);

 if ZoomI <= 0 then
  SpeedButton10.Enabled := false;

 Image1.OnMouseMove(nil,[],MM.x,MM.Y);
 Image1.OnMouseLeave(nil);
end;

procedure TForm1.SpeedButton11Click(Sender: TObject);
begin
 SpeedButton10.Enabled := true;

 if ZoomI < 7 then
 inc(ZoomI);

 if ZoomI >= 7 then
  SpeedButton11.Enabled := false;

 Image1.OnMouseMove(nil,[],MM.x,MM.Y);
 Image1.OnMouseLeave(nil);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  Projections.OnChange(nil);
  StringGrid1.Cells[0,0] := '���';
  StringGrid1.Cells[1,0] := 'X �� �����������';
  StringGrid1.Cells[2,0] := 'Y �� �����������';
  StringGrid2.Cells[0,0] := '���';
  StringGrid2.Cells[1,0] := 'X �� �����������';
  StringGrid2.Cells[2,0] := 'Y �� �����������';

  RefreshBtn.OnClick(nil);
end;

procedure TForm1.AutoListClick(Sender: TObject);
var i, zone, ImgX, ImgY : integer;
    S : TStringList;
    B, L, X, Y, h : Double;
begin
  SetCurrentDir(MyDir);

  N6.Click;

  if FileExists(TmpDir +'Bounds.txt') then
     DeleteFile(TmpDir +'Bounds.txt');

 Hide;

 ExecAndWait(PChar(MyDir+'NomList.exe'),'-r', sw_restore);

 Show;


 ///// ������� ���� �� ����� tmp
 S := TStringList.Create;
 try
   S.LoadFromFile(tmpDir+'bounds.txt');

   for i := 0 to (S.Count-1) div 2 do
   Begin
     B := StrToFloat(s[i*2]);
     L := StrToFloat(s[i*2+1]);

     case Projections.ItemIndex of
     0:  GeoToUTM(WGS, B, L, false, y, x, Mainzone, false);   {01-06-21}
     1:
     begin
         if CS<> WGS then
            Geo1ForceToGeo2(B, L, 0, WGS, CS, B, L, h);

         GeoToGaussKruger(CS,B, L, y, x, zone, i=0);
     end;
     2:
     begin
         if CS<> WGS then
            Geo1ForceToGeo2(B, L, 0, WGS, CS, B, L, h);

          GeoToUTM(CS, B, L, false, y, x, zone, i=0);
     end;

    end;


    ImgX := round( (Coeffs.a*X + Coeffs.b*(-Y) -
             Coeffs.b*Coeffs.D - Coeffs.a*Coeffs.C) /
            (Coeffs.a*Coeffs.a + Coeffs.b*Coeffs.b));

    ImgY := round( ((-Y) - Coeffs.b*ImgX - Coeffs.D) / Coeffs.a );


    AddPoint(StringGrid2, round(Imgx),
              round(Imgy), 0, 0, 0);
   End;

  except
  end;

  S.Destroy;
  KillFrame.Enabled := StringGrid2.RowCount >3;

  RefreshBtn.Click;
  UpdatePreview;

end;

procedure TForm1.ResetSets;
begin
      ClickMode := -1;
      BM1.Flat := True;
      BM2.Flat := True;
      BM3.Flat := True;
      BM1.Enabled := False;
      BM2.Enabled := False;
      BM3.Enabled := False;
      Panel1.Visible := false;
      Panel3.Visible := false;
      Panel7.Visible := false;
      SaveAsdb.Enabled :=False;
      CountControl.Enabled :=False;
      ReturnFrame.Enabled :=False;
      KillFrame.Enabled :=False;
      AutoList.Enabled :=False;

      GeoReady := false;
      MFound := False;
end;

procedure TForm1.N12Click(Sender: TObject);
begin
  Image1.OnMouseUp(Sender,mbLeft,[],MM.X, MM.Y);

  GeoReady := false;
  MFound := False;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;
end;

procedure TForm1.AddPoint(StringGrid: TStringGrid; X, Y: Integer; B,
  L: Double; N : Integer);

var i, j, StN : integer;
    NameExist: Boolean;
    StrP : String;
begin
 StrP := '����� ';
 if ClickMode = 3 then
    StrP := '����� ';


 if N=0 then
 Begin
 j:=0;
 repeat
   inc(j);
   NameExist := false;
   For i := 1 to StringGrid.RowCount-1 do
    if (StringGrid.Cells[0,i]) = StrP + IntToStr(j) then
    begin
      NameExist := true;
      break;
    end;
   until NameExist = false;
 End
  else
    j := N;

  Stn :=-1;

  For i := 1 to StringGrid.RowCount-1 do
   if (StringGrid.Cells[0,i]) = StrP + IntToStr(j) then
   Begin
      Stn := i;
      break;
   End;

 if Stn = -1 then
 Begin
   If StringGrid.RowCount=2 then
   Begin
     if (StringGrid.Cells[0,1])='' then
        StN := 1
      else
      Begin
         StringGrid.RowCount := StringGrid.RowCount +1;
         StN := StringGrid.RowCount - 1;
      end;
   End
    Else
    Begin
         StringGrid.RowCount := StringGrid.RowCount +1;
         StN := StringGrid.RowCount - 1;
    End;
 End;

 if StringGrid.ColCount >= 3 then
 Begin
   StringGrid.Cells[0,Stn] := StrP + IntToStr(j);
   StringGrid.Cells[1,Stn] := IntToStr(X);
   StringGrid.Cells[2,Stn] := IntToStr(Y);
 End;

 if StringGrid.ColCount >= 5 then
 Begin
   StringGrid.Cells[3,Stn] := FloatToStr(L);
   StringGrid.Cells[4,Stn] := FloatToStr(B);
 End;

 StringGrid.Row := Stn;
end;

procedure TForm1.N14Click(Sender: TObject);
var x, y: integer;
begin
     if StringGrid1.Cells[0,StringGrid1.Row] = '' then
     exit;

     Form2.Top := (Screen.Height - Form2.Height) div 2;
     Form2.Left := (Screen.Width - Form2.Width) div 2;

     Form2.X.MaxValue := Image.Picture.Bitmap.Width-1;
     Form2.Y.MaxValue := Image.Picture.Bitmap.Height-1;

     x := StrToInt(StringGrid1.Cells[1,StringGrid1.Row]);
     y := StrToInt(StringGrid1.Cells[2,StringGrid1.Row]) ;

     Form2.X.Value := X;
     Form2.Y.Value := Y;

     Form2.XX.Text := (StringGrid1.Cells[3,StringGrid1.Row]);
     Form2.YY.Text := (StringGrid1.Cells[4,StringGrid1.Row]); 

     Form2.N := trunc(StrToFloat2((StringGrid1.Cells[0,StringGrid1.Row])));

     try
        Image2.Picture.Bitmap.Canvas.Brush.Color := clBtnShadow;

        Image2.Picture.Bitmap.Canvas.FillRect(Rect(0,0,100,100));
        Image2.Picture.Bitmap.Canvas.CopyRect(Rect(0,0,100,100),
                               Image.Picture.Bitmap.Canvas, Rect(X  - Zoom[ZoomI]+1,
                               Y  - Zoom[ZoomI]+1, X + Zoom[ZoomI], Y  + Zoom[ZoomI]));
        Image2.Canvas.Pen.Color := clRed;
        Image2.Canvas.MoveTo(Image2.Width div 2, Image2.Height);
        Image2.Canvas.LineTo(Image2.Width div 2, 0);
        Image2.Canvas.MoveTo(0, Image2.Height div 2);
        Image2.Canvas.LineTo(Image2.Width , Image2.Height div 2);

        Form2.Image1.Picture.Bitmap.Assign(Image2.Picture.Bitmap);
     except
     end;

     Form2.ShowModal;

  GeoReady := false;
  MFound := False;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;
end;

procedure TForm1.N13Click(Sender: TObject);
begin
  DelPoint(StringGrid1, StringGrid1.Row);

  UpdatePreview;

  RefreshBtn.OnClick(nil);

  GeoReady := false;
  MFound := False;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;
end;

type TMyGrid=class(TCustomGrid);
 
procedure DeleteARow(Grid: TStringGrid;
  ARow: Integer);
begin
  TMyGrid(Grid).DeleteRow(ARow);
end;

procedure TForm1.DelPoint(StringGrid: TStringGrid; DRow: Integer);
var i:integer;
begin
//
  if DRow <1 then
  exit;

  for i := 0 to StringGrid.ColCount-1 do
         StringGrid.Cells[i,dRow]:='';

//  StringGrid.Options := StringGrid.Options +[goEditing];
  if StringGrid.RowCount > 2 then
     DeleteARow(StringGrid,dRow)

 // StringGrid.Options := StringGrid.Options -[goEditing];
end;

procedure TForm1.ProjectionsChange(Sender: TObject);
begin

   Case Projections.ItemIndex of
   0:
     begin
       StringGrid1.Cells[3,0] := '������� (L), ' +#176;
       StringGrid1.Cells[4,0] := '������ (B), ' +#176;
     end;
   1,2:
     begin
       StringGrid1.Cells[3,0] := '������, �';
       StringGrid1.Cells[4,0] := '�����, �';
     end;
   end;


end;

procedure TForm1.Label5Click(Sender: TObject);
begin
 Label5.Hide;
 Label5.Caption :='';
end;

procedure TForm1.UpdatePreview;
var i, _x, _y, _x2, _y2 :integer;
begin
   try
 //  Preview.Picture.Assign(Image.Picture);

    Preview.Picture.Bitmap.Width :=200;
    Preview.Picture.Bitmap.Height:=200;

    Preview.Picture.Bitmap.Canvas.Brush.Color := clBtnShadow;
    Preview.Picture.Bitmap.Canvas.FillRect(Rect(0,0,200,200));
   

    SetStretchBltMode(Preview.Picture.Bitmap.Canvas.Handle, HalfTone);
//    SetRop2(Handle,
    StretchBlt(Preview.Picture.Bitmap.Canvas.Handle,
               PreX0 div 2,PreY0 div 2, 200-PreX0 ,200-PreY0 ,
               Image.Canvas.Handle,
               0,0,Image.Picture.Bitmap.Width, Image.Picture.Bitmap.Height , SRCCopy);

        for i:=1 to StringGrid2.RowCount-1 do
          if StringGrid2.Cells[0,i]<>'' then
          try

            _x:= round((StrToInt(StringGrid2.Cells[1,i]))*(200-PreX0)/Image.Picture.Bitmap.Width+ PreX0 div 2);
            _y:= round((StrToInt(StringGrid2.Cells[2,i]))*(200-PreY0)/Image.Picture.Bitmap.Height+ PreY0 div 2);

            ImageList1.Draw(Preview.Canvas, _x-12, _y-12,5);

            if i>1 then
            begin
               _x2:= round((StrToInt(StringGrid2.Cells[1,i-1]))*(200-PreX0)/Image.Picture.Bitmap.Width+ PreX0 div 2);
               _y2:= round((StrToInt(StringGrid2.Cells[2,i-1]))*(200-PreY0)/Image.Picture.Bitmap.Height+ PreY0 div 2);
            end
              else
              begin
                 _x2:= round((StrToInt(StringGrid2.Cells[1,StringGrid2.RowCount-1]))*(200-PreX0)/Image.Picture.Bitmap.Width+ PreX0 div 2);
                 _y2:= round((StrToInt(StringGrid2.Cells[2,StringGrid2.RowCount-1]))*(200-PreY0)/Image.Picture.Bitmap.Height+ PreY0 div 2);
              end;

            Preview.Canvas.Pen.Color := clNavy;
            Preview.Canvas.MoveTo(_x,_y);
            Preview.Canvas.LineTo(_x2,_y2);

          except
          end;

        for i:=1 to StringGrid1.RowCount-1 do
          if StringGrid1.Cells[0,i]<>'' then
          try

            _x:= round((StrToInt(StringGrid1.Cells[1,i]))*(200-PreX0)/Image.Picture.Bitmap.Width+ PreX0 div 2);
            _y:= round((StrToInt(StringGrid1.Cells[2,i]))*(200-PreY0)/Image.Picture.Bitmap.Height+ PreY0 div 2);

            ImageList1.Draw(Preview.Canvas, _x-12, _y-12,2);

          except
          end;



        Preview.Repaint;
   except
   end;
end;

procedure TForm1.DrawDots;
var i,j, _x,_y :integer;
begin
     for i:=1 to StringGrid1.RowCount-1 do
          if StringGrid1.Cells[0,i]<>'' then
          try


            _x:= round(-ImgShift.X +  StrToInt(StringGrid1.Cells[1,i]));
            _y:= round(-ImgShift.Y +  StrToInt(StringGrid1.Cells[2,i]));

            j:= 1;
            if ClickMode = 2 then
              if i = StringGrid1.Row then
                  j:=0;

            Image1.Canvas.Brush.Color := clWhite;
            Image1.Canvas.Font.Color := clRed;
            ImageList1.Draw(Image1.Canvas, _x-12, _y-12,j);
            Image1.Canvas.TextOut(_x+5,_y+5, StringGrid1.Cells[0,i]);

          except
          end;

end;

procedure TForm1.DrawFrameDots;
var i,j, _x,_y, px, py, dx, dy :integer;
begin


      for i:=1 to StringGrid2.RowCount-1 do
          if StringGrid2.Cells[0,i]<>'' then
          try


            _x:= round(-ImgShift.X +  StrToInt(StringGrid2.Cells[1,i]));
            _y:= round(-ImgShift.Y +  StrToInt(StringGrid2.Cells[2,i]));


            if I > 1 then
            Begin
              px := Image1.Canvas.PenPos.X;
              py := Image1.Canvas.PenPos.Y;

            End else
              begin
                 px := round(-ImgShift.X +  StrToInt(StringGrid2.Cells[1,StringGrid2.RowCount-1]));
                 py := round(-ImgShift.Y +  StrToInt(StringGrid2.Cells[2,StringGrid2.RowCount-1]));
              end;

              dx := 2;
              dy := 2;
              try
                dx := round( 2* sin (arctan2(px-_x, py - _y)+pi/2));
                dy := round( 2* cos (arctan2(px-_x, py - _y)+pi/2));
              except
              end;

              Image1.Canvas.Pen.Color := clWhite;
              Image1.Canvas.MoveTo(px-dx,py-dy);
              Image1.Canvas.LineTo(_x-dx,_y-dy);
              Image1.Canvas.MoveTo(px+dx,py+dy);
              Image1.Canvas.LineTo(_x+dx,_y+dy);

              Image1.Canvas.Pen.Color := clNavy;
              Image1.Canvas.MoveTo(px-dx div 2,py-dy div 2);
              Image1.Canvas.LineTo(_x-dx div 2,_y-dy div 2);
              Image1.Canvas.MoveTo(px+dx div 2,py+dy div 2);
              Image1.Canvas.LineTo(_x+dx div 2,_y+dy div 2);
              Image1.Canvas.MoveTo(px,py);
              Image1.Canvas.LineTo(_x,_y);


             



            Image1.Canvas.Pen.Color := clNavy;


            j:= 4;
            if ClickMode = 3 then
              if i = StringGrid2.Row then
                  j:=3;

            Image1.Canvas.Brush.Color := clWhite;
            ImageList1.Draw(Image1.Canvas, _x-12, _y-12,j);
            Image1.Canvas.Font.Color := clNavy;
            Image1.Canvas.TextOut(_x+5,_y-15, StringGrid2.Cells[0,i]);

            Image1.Canvas.MoveTo(_x,_y);
          except
          end;
end;

procedure TForm1.N17Click(Sender: TObject);
begin
  DelPoint(StringGrid2, StringGrid2.Row);

  KillFrame.Enabled := StringGrid2.RowCount >4;

  UpdatePreview;

  RefreshBtn.OnClick(nil);
end;

procedure TForm1.N16Click(Sender: TObject);
var x, y: integer;
begin
     if StringGrid2.Cells[0,StringGrid2.Row] = '' then
     exit;

     Form2.Top := (Screen.Height - Form2.Height) div 2;
     Form2.Left := (Screen.Width - Form2.Width) div 2;

     Form2.X.MaxValue := Image.Picture.Bitmap.Width-1;
     Form2.Y.MaxValue := Image.Picture.Bitmap.Height-1;

     x := StrToInt(StringGrid2.Cells[1,StringGrid2.Row]);
     y := StrToInt(StringGrid2.Cells[2,StringGrid2.Row]) ;

     Form2.X.Value := X;
     Form2.Y.Value := Y;

     //Form2.XX.Text := (StringGrid2.Cells[3,StringGrid2.Row]);
     //Form2.YY.Text := (StringGrid2.Cells[4,StringGrid2.Row]);

     Form2.N := trunc(StrToFloat2((StringGrid2.Cells[0,StringGrid2.Row])));

     try
        Image2.Picture.Bitmap.Canvas.Brush.Color := clBtnShadow;

        Image2.Picture.Bitmap.Canvas.FillRect(Rect(0,0,100,100));
        Image2.Picture.Bitmap.Canvas.CopyRect(Rect(0,0,100,100),
                               Image.Picture.Bitmap.Canvas, Rect(X  - Zoom[ZoomI]+1,
                               Y  - Zoom[ZoomI]+1, X + Zoom[ZoomI], Y  + Zoom[ZoomI]));
        Image2.Canvas.Pen.Color := clRed;
        Image2.Canvas.MoveTo(Image2.Width div 2, Image2.Height);
        Image2.Canvas.LineTo(Image2.Width div 2, 0);
        Image2.Canvas.MoveTo(0, Image2.Height div 2);
        Image2.Canvas.LineTo(Image2.Width , Image2.Height div 2);

        Form2.Image1.Picture.Bitmap.Assign(Image2.Picture.Bitmap);
     except
     end;

     Form2.ShowModal;

end;

procedure TForm1.N15Click(Sender: TObject);
begin
 Image1.OnMouseUp(Sender,mbLeft,[],MM.X, MM.Y);
end;

procedure TForm1.N18Click(Sender: TObject);
var i, j : integer;
    s : string;
begin
 if StringGrid2.Row < 2 then
  exit;

 for i := 0 to StringGrid2.ColCount-1 do
 Begin
   s :=  StringGrid2.Cells[i,StringGrid2.Row - 1];
   StringGrid2.Cells[i,StringGrid2.Row - 1] := StringGrid2.Cells[i,StringGrid2.Row];
   StringGrid2.Cells[i,StringGrid2.Row] := s;
 End;

  StringGrid2.Row := StringGrid2.Row - 1;

  UpdatePreview;
end;

procedure TForm1.N19Click(Sender: TObject);
var i : integer;
    s: string;
begin
 if StringGrid2.Row >= StringGrid2.RowCount-1  then
  exit;

 for i := 0 to StringGrid2.ColCount-1 do
 Begin
   s :=  StringGrid2.Cells[i,StringGrid2.Row + 1];
   StringGrid2.Cells[i,StringGrid2.Row + 1] := StringGrid2.Cells[i,StringGrid2.Row];
   StringGrid2.Cells[i,StringGrid2.Row] := s;
 End;

  StringGrid2.Row := StringGrid2.Row + 1;

  UpdatePreview;
end;

procedure TForm1.KillFrameClick(Sender: TObject);
var
   Min, Max, I, J, X, Y, X1, Y1, X2, Y2, Xi, Yi : integer;
begin

  Image.Canvas.Pen.Color := clFuchsia;

  for Xi := 0 to Image.Picture.Bitmap.Width do
  Begin

    Min := -1000;
    Max := -1000;

    /// ��� ����������� (I=1) � ������������ (I=2)
    FOR I := 1 to 2 do
    BEGIN
       y := -1000;
       
       /////  1. ���� ������ � ����� �����
       for J := 1 to StringGrid2.RowCount-1 do
       Begin
          X1 := StrToInt(StringGrid2.Cells[1,J]);
          Y1 := StrToInt(StringGrid2.Cells[2,J]);
         if J < StringGrid2.RowCount-1 then
         begin
            X2 := StrToInt(StringGrid2.Cells[1,J+1]);
            Y2 := StrToInt(StringGrid2.Cells[2,J+1])
         end
           else
            begin
              X2 := StrToInt(StringGrid2.Cells[1,1]);
              Y2 := StrToInt(StringGrid2.Cells[2,1]);
            end;

         ////// 2. ����� ������� ���, ����� X ��� �� ������������

         if X2 < X1 then
         Begin
           X := X2;
           Y := Y2;
           X2 := X1;
           Y2 := Y1;
           X1 := X;
           Y1 := Y;
         End;

         Y := -1000;
         
         /// 3. ��������, ����� ������� ������� �� ����������� X
         /// ���� ��, �� ��� Y(Xi)

         if (Xi >= X1) and (Xi <= X2) then
         begin
           if X2<>X1 then
              Y := Trunc( Y1 + (Y2-Y1)*(Xi-X1)/(X2-X1) )
                else
                  Begin
                    case i of
                       1: if (Y1 < Y2) then
                            Y := Y1
                             else
                               Y := Y2;
                       2: if (Y1 > Y2) then
                            Y := Y1
                             else
                               Y := Y2;
                    end;
                  End;
         end;


         //// 4. ��������, ����������/ ����������� �� ���
         if Y <> -1000 then
         case i of
            1: if min = -1000 then
                   min := Y
                     else
                        if (Y < min) then
                            min := Y;

            2: if max = -1000 then
                   max := Y
                     else
                        if (Y > max) then
                            max := Y
         end;

       End; //// ����� ����� �����

     END; /// ����� ����� I = 1 - 2 

     ///// ����� �����

     if (Max = -1000) or (Min= -1000) then
     begin
       /// �� �����
       Image.Picture.Bitmap.Canvas.MoveTo(Xi,0);
       Image.Picture.Bitmap.Canvas.LineTo(Xi,Image.Picture.Bitmap.Height);
     end
      else
       begin
         /// �����
         Image.Picture.Bitmap.Canvas.MoveTo(Xi,0);
         Image.Picture.Bitmap.Canvas.LineTo(Xi,Min);
         Image.Picture.Bitmap.Canvas.MoveTo(Xi,Max);
         Image.Picture.Bitmap.Canvas.LineTo(Xi,Image.Picture.Bitmap.Height);
       end ;

    //   Memo1.Lines.Add(IntToStr(MinY)+' '+IntToStr(MaxY) );

  End;  ///// ����� ����� �������� ������� �����������


  ///// �� ������ ���!!!!

  for Yi := 0 to Image.Picture.Bitmap.Width do
  Begin

    Min := -1000;
    Max := -1000;

    /// ��� ����������� (I=1) � ������������ (I=2)
    FOR I := 1 to 2 do
    BEGIN
       y := -1000;
       
       /////  1. ���� ������ � ����� �����
       for J := 1 to StringGrid2.RowCount-1 do
       Begin
          X1 := StrToInt(StringGrid2.Cells[1,J]);
          Y1 := StrToInt(StringGrid2.Cells[2,J]);
         if J < StringGrid2.RowCount-1 then
         begin
            X2 := StrToInt(StringGrid2.Cells[1,J+1]);
            Y2 := StrToInt(StringGrid2.Cells[2,J+1])
         end
           else
            begin
              X2 := StrToInt(StringGrid2.Cells[1,1]);
              Y2 := StrToInt(StringGrid2.Cells[2,1]);
            end;

         ////// 2. ����� ������� ���, ����� Y ��� �� ������������

         if Y2 < Y1 then
         Begin
           X := X2;
           Y := Y2;
           X2 := X1;
           Y2 := Y1;
           X1 := X;
           Y1 := Y;
         End;

         X := -1000;
         
         /// 3. ��������, ����� ������� ������� �� ����������� Yi
         /// ���� ��, �� ��� Y(Xi)

         if (Yi >= Y1) and (Yi <= Y2) then
         begin
           if Y2<>Y1 then
              X := Trunc( X1 + (X2-X1)*(Yi-Y1)/(Y2-Y1) )
                else
                  Begin
                    case i of
                       1: if (X1 < X2) then
                            X := X1
                             else
                               X := X2;
                       2: if (X1 > X2) then
                            X := X1
                             else
                               X := X2;
                    end;
                  End;
         end;


         //// 4. ��������, ����������/ ����������� �� ���
         if X <> -1000 then
         case i of
            1: if min = -1000 then
                   min := X
                     else
                        if (X < min) then
                            min := X;

            2: if max = -1000 then
                   max := X
                     else
                        if (X > max) then
                            max := X
         end;

       End; //// ����� ����� �����

     END; /// ����� ����� I = 1 - 2 

     ///// ����� �����

     if (Max = -1000) or (Min= -1000) then
     begin
       /// �� �����
       Image.Picture.Bitmap.Canvas.MoveTo(0,Yi);
       Image.Picture.Bitmap.Canvas.LineTo(Image.Picture.Bitmap.Width, Yi);
     end
      else
       begin
         /// �����
         Image.Picture.Bitmap.Canvas.MoveTo(0,Yi);

         Image.Picture.Bitmap.Canvas.LineTo(Min, Yi);
         Image.Picture.Bitmap.Canvas.MoveTo(Max, Yi);

         Image.Picture.Bitmap.Canvas.LineTo(Image.Picture.Bitmap.Width, Yi);
       end ;

    //   Memo1.Lines.Add(IntToStr(MinY)+' '+IntToStr(MaxY) );

  End;  ///// ����� ����� �������� ������� �����������




  ReturnFrame.Enabled := True;

  UpdatePreview;
  UpdateBuff;
  RefreshBtn.OnClick(nil);
end;

procedure TForm1.ReturnFrameClick(Sender: TObject);
begin
  OpenImg(FName);
  UpdateBuff;
  RefreshBtn.OnClick(nil);
  UpdatePreview;
  ReturnFrame.Enabled := False;

  ClickMode :=3;
  BM3.Click;
end;

procedure TForm1.UpdateBuff;
begin
  Buff1.Size:=Point(Image.Picture.width,Image.Picture.Height);
  Buff1.Active:=true;
  Buff1.Canvas.Draw(0, 0, Image.Picture.Graphic);
end;

procedure TForm1.ClearDots;
var i :integer;
begin
      for i := StringGrid1.RowCount-1 downto 1 do
          DelPoint(StringGrid1, I);
end;

procedure TForm1.ClearFrameDots;
var i :integer;
begin
      for i := StringGrid2.RowCount-1 downto 1 do
          DelPoint(StringGrid2, I);

end;

procedure TForm1.N6Click(Sender: TObject);
begin
  ClearFrameDots;
  UpdatePreview;
  RefreshBtn.OnClick(nil);

  KillFrame.Enabled := false;
end;

procedure TForm1.N7Click(Sender: TObject);
begin
  ClearDots;
  UpdatePreview;
  RefreshBtn.OnClick(nil);

  CountControl.Enabled := False;
  AutoList.Enabled := False;
  SaveASDB.Enabled := False;

  GeoReady := false;
  MFound := False;
end;

procedure TForm1.BMP1Click(Sender: TObject);
var FName :String;
begin
  if SPD.Execute then
  begin
     FName := SPD.FileName;
     if Pos('.',Fname) < length(Fname)-4 then
        FName := FName + '.bmp';
      Image.Picture.SaveToFile(FName);
  end;
end;

procedure TForm1.StringGrid2DblClick(Sender: TObject);
var x, y : integer;
begin
   if StringGrid2.Cells[0,StringGrid2.Row] = '' then
     exit;

   x := StrToInt(StringGrid2.Cells[1,StringGrid2.Row]);
   y := StrToInt(StringGrid2.Cells[2,StringGrid2.Row]) ;

   ImgShift.X := x -Image1.Width  div 2;
   ImgShift.Y := y-Image1.Height  div 2 ;

   ScrollBar1.Position:=round(100*ImgShift.X/(Image.Picture.Width -Image1.Width /Scale));
   ScrollBar2.Position:=round(100*ImgShift.Y/(Image.Picture.height-Image1.Height/Scale));

   DrawMini(X,Y);


   DoMM := false;
   RefreshBtn.OnClick(nil);
end;

procedure TForm1.StringGrid1DblClick(Sender: TObject);
var x, y : integer;
begin
   if StringGrid1.Cells[0,StringGrid1.Row] = '' then
     exit;

   x := StrToInt(StringGrid1.Cells[1,StringGrid1.Row]);
   y := StrToInt(StringGrid1.Cells[2,StringGrid1.Row]) ;

   ImgShift.X := x -Image1.Width  div 2;
   ImgShift.Y := y-Image1.Height  div 2 ;

   ScrollBar1.Position:=round(100*ImgShift.X/(Image.Picture.Width -Image1.Width /Scale));
   ScrollBar2.Position:=round(100*ImgShift.Y/(Image.Picture.height-Image1.Height/Scale));

   DrawMini(X,Y);

   DoMM := false;
   RefreshBtn.OnClick(nil);
end;

procedure TForm1.DrawMini(X, Y: Integer);
begin
  if Panel3.Visible then
  try
     Image2.Picture.Bitmap.Canvas.Brush.Color := clBtnShadow;

     Image2.Picture.Bitmap.Canvas.FillRect(Rect(0,0,100,100));
     Image2.Picture.Bitmap.Canvas.CopyRect(Rect(0,0,100,100),
                               Image.Picture.Bitmap.Canvas, Rect(X- Zoom[ZoomI]+1,
                               Y  - Zoom[ZoomI]+1, X  + Zoom[ZoomI], Y + Zoom[ZoomI]));
     Image2.Canvas.Pen.Color := clRed;
     Image2.Canvas.MoveTo(Image2.Width div 2, Image2.Height);
     Image2.Canvas.LineTo(Image2.Width div 2, 0);
     Image2.Canvas.MoveTo(0, Image2.Height div 2);
     Image2.Canvas.LineTo(Image2.Width , Image2.Height div 2);

  except
  end;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
var FName : String;
    i: integer;
    S : TStringList;
begin
  if not SaveDialog1.Execute then
    exit;

   FName := SaveDialog1.FileName;

   if Pos('.',Fname) < length(Fname)-4 then
        FName := FName + '.txt';

   S := TStringList.Create;

   S.Add(DatumList[CS].Name + Tab + IntToStr(Projections.ItemIndex));
   for i := 1 to StringGrid1.RowCount-1 do
     S.Add(StringGrid1.Cells[0,i] + Tab +
        StringGrid1.Cells[1,i] + Tab +
        StringGrid1.Cells[2,i] + Tab +
        StringGrid1.Cells[3,i] + Tab +
        StringGrid1.Cells[4,i]);

  S.SaveToFile(Fname);
  S.Destroy;
end;

procedure TForm1.DatumsChange(Sender: TObject);
begin
  CS := FindDatumByCaption(Datums.Items[Datums.ItemIndex]);
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
var  S : TStringList;
     i :integer;
     Str : string;
begin
  if not OpenDialog2.Execute then
    exit;

   FName := OpenDialog2.FileName;

   S := TStringList.Create;
   S.LoadFromFile(FName);

   i := FindDatum(GetCols(S[0],Tab,0,1));
   if i= -1 then
   begin
     MessageDlg('�� ������� �� ��� �������� ������',mtError,[mbOk],0);
     exit;
   end;

   Str := DatumList[i].Caption;

   for i := 0 to Datums.Items.Count - 1 do
     if Datums.Items[i] = Str then
     begin
       Datums.ItemIndex := i;
       break;
     end;

   Projections.ItemIndex := StrToInt(GetCols(S[0],Tab,1,1));

   for i := 1 to S.Count-1 do
     AddPoint(StringGrid1, StrToInt(GetCols(S[i],Tab,1,1)) ,
              StrToInt(GetCols(S[i],Tab,2,1)) , StrToFloat(GetCols(S[i],Tab,4,1)) ,
              StrToFloat(GetCols(S[i],Tab,3,1)), trunc(StrToFloat2(GetCols(S[i],Tab,0,1))));


  S.Destroy;


  CountControl.Enabled := StringGrid1.RowCount >2;
  UpdatePreview;
  RefreshBtn.OnClick(nil);

  GeoReady := false;
  MFound := False;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;
end;

procedure TForm1.OziExplorer1Click(Sender: TObject);
begin
 OpenOzi(true);
end;

procedure TForm1.OpenOzi(LoadImage: Boolean);
var S : TStringList;
    ImgName : String;
    SK, I, x, y : Integer;
    B, L : double;
    SKN : String;
begin
  if not OpenDialog3.Execute then
    exit;

  N6Click(nil);
  N7Click(nil);

  StaticText3.Caption := '';
  StaticText4.Caption := '';

  S := TStringList.Create;
  i := 0;
 
  try
      S.LoadFromFile(OpenDialog3.FileName);

      ImgName := ExtractFilePath(OpenDialog3.FileName)+'\'+ s[2];
      if LoadImage then
      try
        ResetSets;

        OpenImg(ImgName);

        Fname := ImgName;

//        Fname := OpenDialog1.FileName;

        for i := Length(FName) DownTo 1 Do
          if FName[i]='\' then
            break;

        ImgFName := Copy(Fname,I+1,Length(Fname)-I);

        UpdateBuff;

        ClearDots;

        ClearFrameDots;

        Label5.Visible := false;
    except
      MessageDlg('������ �������� �����: '+#13+OpenDialog1.FileName,mtError,[mbOk],1);
    end;


     ////////// ������� ���������
      SK := -1;
      SKN := GetCols(s[4],',',0,1, False);

      if (SKN = 'WGS 84') or (SKN = 'WGS84') or (SKN = 'WGS-84') or (SKN = 'WGS')then
        SK := FindDatum('WGS84') ;

      if ( (Pos('Pulkovo',SKN)>0)or(Pos('SK',SKN)>0) ) and(Pos('42',SKN)>0) then
        SK := FindDatum('SK42');

      if ( (Pos('Pulkovo',SKN)>0)or(Pos('SK',SKN)>0) ) and(Pos('95',SKN)>0) then
        SK := FindDatum('SK95');

      if SK=-1 then
      begin
        S.Destroy;
        MessageDlg('����������� ������� ���������', mtError,[mbOk],0);
        exit;
      end;

      CS := SK;

      for i := 0 to Datums.Items.Count - 1 do
      if Datums.Items[i] = DatumList[CS].Caption then
      begin
          Datums.ItemIndex := i;
          break;
      end;

      i := 8;
      while not((GetCols(S[i],',',2,1, true)='')or(GetCols(S[i],',',2,1, true)=' '))
            or (i>S.Count-1) do
      try
         inc(i);
         X := round(StrToFloat(GetCols(S[i],',',2,1,true)));
         Y := round(StrToFloat(GetCols(S[i],',',3,1,true)));

         if GetCols(S[i],',',6,1,true)<>'' then
         begin

            B :=  StrToFloat(GetCols(S[i],',',6,1,true))+ StrToFloat(GetCols(S[i],',',7,1,true))/60;
            if GetCols(S[i],',',8,1,true)='S' then
                B := -B;

            L :=  StrToFloat(GetCols(S[i],',',9,1,true))+ StrToFloat(GetCols(S[i],',',10,1,true))/60;
            if GetCols(S[i],',',11,1,true)='W' then
                L := -L;

            Projections.ItemIndex := 0;

         end
           else
           begin
             L :=  StrToFloat(GetCols(S[i],',',14,1,true)); /// NORTHING
             B :=  StrToFloat(GetCols(S[i],',',15,1,true)); /// EASTING

             if (SK =  FindDatum('SK95')) or (SK=SK42) then
                Projections.ItemIndex := 1
                else
                  Projections.ItemIndex := 2;
           end;

         AddPoint(StringGrid1, X, Y, B, L, 0);
        // inc(i);
      except
      End;


  except
    MessageDlg('������ �������� �����', mtError, [mbOk],0);
  end;


  Projections.OnChange(nil);
  RefreshBtn.OnClick(nil);
  UpdatePreview;
  S.Destroy;

  CountControl.Enabled := StringGrid1.RowCount >2;

  GeoReady := false;
  MFound := False;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;

  //if ImgFName <> '' then
 // FName := ExtractFilePath(Fname) + '\' + ImgFName;

  if i-9 >= 4 then
    if MessageDlg(' ��������� ������ ���������� ��������? ', MtConfirmation, [mbYes, mbNo],0) = 6 then
      CountControl.Click;

end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  OpenOzi(false);
end;

procedure TForm1.CountControlClick(Sender: TObject);
var i, j : integer;

    PointCount, Zone, CoefCount : integer;

    ImgX, ImgY, MapX, MapY : array [0..100] of Double;
    a, b, C, D, SF, beta : array [0..2000] of Double; //// Coefficients
    aa, ab, aC, aD, aSF, abeta, RMS, Err : Double; //// Average results

    MapB, MapL, _B, _L, _h, _MapX, _MapY : Double; //// TMP Data
begin
  ///
  // if Sender<> nil then
  ShowReport := true;

  Form3.Memo1.Clear;

  //// ���� 1. �������� ������� �����, �������� ���������� � �������� ��� �������������

  Form3.Memo1.Lines.Add('�����: ');
  Form3.Memo1.Lines.Add('');
  PointCount := StringGrid1.RowCount-1;

  if PointCount > 100 then
    PointCount := 100;

  for I := 1 to PointCount do
  Begin
    ImgX[I-1] := StrToFloat(StringGrid1.Cells[1,I]);
    ImgY[I-1]  := StrToFloat(StringGrid1.Cells[2,I]);

    case Projections.ItemIndex of
      0:
      begin
         if I = 1 then
         Begin
           _L := 0;    _B := 0;                {01-06-21}
           for j := 1 to PointCount do
           begin
             _L  := _L + StrToFloat(StringGrid1.Cells[3,j])/(PointCount);
             _B  := _B + StrToFloat(StringGrid1.Cells[4,j])/(PointCount);
           end;
           if CS <> WGS then
            Geo1ForceToGeo2(_B,
                            _L,0,
                            CS, WGS, _B, _L, _h);

           GeoToUTM(WGS, _B, _L, false, MapB, MapL, zone, true);
           MainZone := zone;
         End;

         _L  := StrToFloat(StringGrid1.Cells[3,I]);
         _B  := StrToFloat(StringGrid1.Cells[4,I]);

         if CS <> WGS then
            Geo1ForceToGeo2(StrToFloat(StringGrid1.Cells[4,I]),
                            StrToFloat(StringGrid1.Cells[3,I]),0,
                            CS, WGS, _B, _L, _h);

         GeoToUTM(WGS, _B, _L, false, MapB, MapL, zone, false);

      end;
      1,2:
      begin
         MapL := StrToFloat(StringGrid1.Cells[3,I]);
         MapB  := StrToFloat(StringGrid1.Cells[4,I]);
      end;
    end;

    MapX[I-1] := MapL;
    MapY[I-1]  := - MapB;

    Form3.Memo1.Lines.Add(StringGrid1.Cells[0,I]+ '  '
                          + format('%n',[ImgX[I-1]]) + '  '
                          + format('%n',[ImgY[I-1]]) + '  '
                          + format('%n',[MapX[I-1]]) + '  '
                          + format('%n',[-MapY[I-1]]) + '  ');
  End;

  //// ���� 2. C����� ��������� ��� ������ �����
  CoefCount := 0;

  Form3.Memo1.Lines.Add('');
  Form3.Memo1.Lines.Add('------');
  Form3.Memo1.Lines.Add('');
  Form3.Memo1.Lines.Add('������ ��������: ');
  Form3.Memo1.Lines.Add('');
  For I := 0 To  PointCount -2 Do
    For J := I+1 To  PointCount -1 Do
    Begin
       //

      SF[CoefCount] := ( sqrt ( sqr(ImgX[j]-ImgX[i]) + sqr(ImgY[j]-ImgY[i]) )
                / sqrt ( sqr(MapY[j]-MapY[i]) + sqr(MapX[j]-MapX[i]) ));

      beta[CoefCount] := -(arctan3((ImgY[j]-ImgY[i]),(ImgX[j]-ImgX[i])) - arctan3((MapY[j]-MapY[i]),(MapX[j]-MapX[i])));


      if beta[CoefCount] < -pi then
         beta[CoefCount] := beta[CoefCount] + 2*pi;
      if beta[CoefCount] > pi then
         beta[CoefCount] := beta[CoefCount] - 2*pi;


      a[CoefCount] := 1/SF[CoefCount]*Cos(beta[CoefCount]);
      b[CoefCount] := 1/SF[CoefCount]*Sin(beta[CoefCount]);

      C[CoefCount] := MapX[i] - (a[CoefCount]*ImgX[i] - b[CoefCount]*ImgY[i]);
      D[CoefCount] := MapY[i] - (a[CoefCount]*ImgY[i] + b[CoefCount]*ImgX[i]);


      Form3.Memo1.Lines.Add(StringGrid1.Cells[0,I+1] + ' - ' + StringGrid1.Cells[0,J+1]);
      Form3.Memo1.Lines.Add('  �������: ' + format('%.4f',[SF[CoefCount]]));

      Form3.Memo1.Lines.Add('  ����: a1: '
                            + format('%.4f', [(arctan3((ImgY[j]-ImgY[i]),(ImgX[j]-ImgX[i])))*180/pi])
                            +'  a2: '+
                            format('%.4f',[(arctan3((MapY[j]-MapY[i]),(MapX[j]-MapX[i])))*180/pi])
                            +'   ��������: '+
                            format('%.4f',[(beta[CoefCount]*180/pi)]));

      Form3.Memo1.Lines.Add('  �����: X: ' + format('%n',[C[CoefCount]]) + '  Y: ' + format('%n',[-D[CoefCount]]));

      inc(CoefCount);
    End;


  //// ���� 2b. C����� ������� ���������

  //aa:= 0; ab :=0;
  aC := 0; aD := 0;
  aBeta := 0; aSF := 0;

  For i := 0 to CoefCount-1 Do
  Begin
     aSF := aSF + SF[I]/CoefCount;
     aBeta := aBeta + Beta[I]/CoefCount;
     aC := aC + C[I]/CoefCount;
     aD := aD + D[I]/CoefCount;
  End;

  aa := 1/aSF*Cos(abeta);
  ab := 1/aSF*Sin(abeta);

  if CDMode then
  Begin
     aC := MapX[1] - (aa*ImgX[1] - ab*ImgY[1]);
     aD := MapY[1] - (aa*ImgY[1] + ab*ImgX[1]);
  End;

  {aC := MapX[0] - (aa*ImgX[0] - ab*ImgY[0]);
  aD := MapY[0] - (aa*ImgY[0] + ab*ImgX[0]); }

  //// ���� 3. ����������

  Coeffs.a := aa;  Coeffs.b := ab;
  Coeffs.C := aC;  Coeffs.D := aD;
  GeoReady := true;

  //// ���� 4. ��� �����: ��������� ������� � �-�� ������

  GetRasterMapData;

  //// ���� 5. �����

  with Form3 do
  begin
    Memo1.Lines.Add('');
    Memo1.Lines.Add('------');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('������� ��������:');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('  �������: ' + format('%.4f',[aSF]) + ' ���� ��������: '+format('%.4f',[abeta*180/pi]));
    Memo1.Lines.Add('  �����: X: ' + format('%n',[aC])+'  Y: '+ format('%n',[-aD])+
                    '  ���������: a: ' +  format('%.4f',[aa]) + '  b: ' +  format('%.4f',[ab]) );

    Memo1.Lines.Add('');
    Memo1.Lines.Add('------');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('����� �������� �������� �����:');
    Memo1.Lines.Add('');

    RMS := 0;
    For i := 0 to PointCount-1 Do
    begin
      _mapX := aa*imgX[i] - ab*imgY[i] + aC   - mapX[i];
      _mapY := ab*imgX[i] + aa*imgY[i] + aD   - mapY[i];

      Err := Sqrt(Sqr(_mapX)+Sqr(_mapY));
      RMS := RMS + Err*Err;

      Memo1.Lines.Add(StringGrid1.Cells[0,I+1]+': ');
      Memo1.Lines.Add('   ' +  format('%n',[ Err ])+' �');

  //    Err := Err*aSF;

      _mapX :=(aa*MapX[i] + ab*MapY[i] - ab*aD - aa*aC)/(aa*aa + ab*ab);
      _mapY :=(MapY[i] - ab*_MapX - aD)/aa;

      Err := Sqrt(Sqr(ImgX[i]-_mapX)+Sqr(ImgY[i]-_mapY));

      Memo1.Lines.Add('   ' +  format('%n', [Err])+' ����.');
      Memo1.Lines.Add('');
    end;

    IsolatedControl;

    RMS := Sqrt(RMS/ (PointCount-2));
    Memo1.Lines.Add('------');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('��� ��������: '+ format('%n',[RMS*aSF])+' ����. ('+ format('%n',[RMS]) +' �)');
    Memo1.Lines.Add('');

    case trunc(RMS*aSF) of
      0..1:   Memo1.Lines.Add('����� ������: �������');
      2..4:   Memo1.Lines.Add('����� ������: ��');
      5..10:  Memo1.Lines.Add('����� ������: �����������������');
      11..25: Memo1.Lines.Add('����� ������: �����');
      else
      begin
         Memo1.Lines.Add('������� ������ ������. ��������� �����.');
         GeoReady := false;
      end;
    end;

    if ShowReport then
       Showmodal;
  end;

  //GeoReady := false;
  //MFound := False;
  SaveAsdb.Enabled := geoReady;
  AutoList.Enabled := mFound;
end;

procedure TForm1.N21Click(Sender: TObject);
begin
  if StringGrid1.Cells[0,StringGrid1.Row] = '' then
     exit;

  ClickMode :=3;

  AddPoint(StringGrid2, StrToInt(StringGrid1.Cells[1,StringGrid1.Row]),
                        StrToInt(StringGrid1.Cells[2,StringGrid1.Row]),
                        StrToFloat(StringGrid1.Cells[3,StringGrid1.Row]),
                        StrToFloat(StringGrid1.Cells[4,StringGrid1.Row]),
                        0);
  RefreshBtn.Click;                       
  UpdatePreview;

  KillFrame.Enabled := StringGrid2.RowCount >4;

  ClickMode :=2;
end;

procedure TForm1.CountTiles;
var i, j, k: integer;
    x, y : array[1..4] of Double;
    B, L, H : double;
    S : TStringList;
begin

  S := TStringList.Create;
  try
    For i := 0 to NX-1 do
      For j := 0 to NY-1 do
      Begin
        X[1] := Coeffs.a*(i*512) - Coeffs.b*(j*512) + Coeffs.C;
        Y[1] := - ( Coeffs.b*(i*512) + Coeffs.a*(j*512) + Coeffs.D);

        X[2] := Coeffs.a*((i+1)*512) - Coeffs.b*(j*512) + Coeffs.C;
        Y[2] := - ( Coeffs.b*((i+1)*512) + Coeffs.a*(j*512) + Coeffs.D);

        X[3] := Coeffs.a*((i+1)*512) - Coeffs.b*((j+1)*512) + Coeffs.C;
        Y[3] := - ( Coeffs.b*((i+1)*512) + Coeffs.a*((j+1)*512) + Coeffs.D);

        X[4] := Coeffs.a*(i*512) - Coeffs.b*((j+1)*512) + Coeffs.C;
        Y[4] := - ( Coeffs.b*(i*512) + Coeffs.a*((j+1)*512) + Coeffs.D);

        S.Add(IntToStr(i)+' '+IntToStr(j)+' ');

        for k := 1 to 4 do
        begin

           case Projections.ItemIndex of
             0 : UTMToGeo(WGS, y[k], x[k], false, B, L,
               0, (MainZone-30)*6-3, 0, MainZone*1000000+500000, 0.9996 );  {01-06-21}

             1: // G-K
               begin
                 GaussKrugerToGeo(CS, Y[k], X[k], B, L);
                 if CS<>WGS then
                   Geo1ForceToGeo2(B, L, 0, CS, WGS, B, L, H);

               end;

             2: // UTM
               begin
                 if CS = WGS then
                   UTMToGeo(WGS, y[k], x[k], false, B, L)
                    else
                    begin
                       UTMToGeo(CS, y[k], x[k], false, B, L);
                       Geo1ForceToGeo2(B, L, 0, CS, WGS, B, L, h)
                    end;
               end;
           end;

           S[S.Count-1] := S[S.Count-1] + FloatTostr(B) + ' ' +   FloatTostr(L) + ' ';

        end; /// cycle k

      End; /// cycle i, j

  except
  end;

  S.SaveToFile(Tmpdir + ImgFname + '.txt');
  S.Destroy;
end;

procedure TForm1.GetRasterMapData;
var
    S : TStringList;
    M, i,j : Integer;
    dY, X, Y, B, L, h : Double;

const    StabScales : array[1..6] of String = ('1000000','500000','200000',
                                                '100000','50000','25000');
         ScaleHeights : array[1..6] of Integer = (445000, 222000, 74000, 37000,
                                                  19000,9000);  /// ��������� ������ ����� �� ��������� �����������
begin

  dY := abs ((Coeffs.b*(Image.Picture.Bitmap.Width/2) + 0 + Coeffs.D)   -
            (Coeffs.b*(Image.Picture.Bitmap.Width/2) + Coeffs.a*Image.Picture.Bitmap.Height + Coeffs.D));
  M := -1;

  for i := 1 to 6 do
    if (dY > ScaleHeights[i] - ScaleHeights[i]*0.2)
        and (dY < ScaleHeights[i] + ScaleHeights[i]*0.25) then
    Begin
       M := I;
       Break;
    End;

  MFound := (M > -1);

  if not MFound then
    exit;

  S := TStringList.Create;
  S.Add(StabScales[M]);

  X := Coeffs.a*(Image.Picture.Bitmap.Width/2) - Coeffs.b*(Image.Picture.Bitmap.Height/2) + Coeffs.C;
  Y := - ( Coeffs.b*(Image.Picture.Bitmap.Width/2) + Coeffs.a*(Image.Picture.Bitmap.Height/2) + Coeffs.D);

  case Projections.ItemIndex of
     0 : UTMToGeo(WGS, y, x, false, B, L);

     1: // G-K
     begin
       GaussKrugerToGeo(CS, Y, X, B, L);
       if CS<>WGS then
         Geo1ForceToGeo2(B, L, 0, CS, WGS, B, L, H);

     end;

     2: // UTM
     begin
       if CS = WGS then
         UTMToGeo(WGS, y, x, false, B, L)
           else
           begin
             UTMToGeo(CS, y, x, false, B, L);
             Geo1ForceToGeo2(B, L, 0, CS, WGS, B, L, h)
           end;
     end;

  end; /// case Projections.ItemIndex

  S.Add(FloatTostr(B));
  S.Add(FloatTostr(L));

  S.SaveToFile(TmpDir+'find.txt');

  S.Destroy;
end;

procedure TForm1.OziExplorer2Click(Sender: TObject);
begin
 OpenOzi(false);
end;

procedure TForm1.N22Click(Sender: TObject);
begin
   SetCurrentDir(MyDir);
   winexec('GetMaps.exe',sw_restore);
end;

end.



