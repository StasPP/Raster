unit Channels;

{ © 1999 Gavina International Inc. Alexey Lavnikov }

interface uses Windows, Messages, SysUtils, Classes, Graphics;

type
    TRGBEntry     = packed record R, G, B, C: Byte; end;
    TRGBPalette   = array[0..255] of TRGBEntry;
    PRGBPalette   = ^TRGBPalette;
    PRects        = ^TRects;
    TRects        = array[0..1000] of TRect;
    TAligns       = (aFrame, aFrameLeft, aFrameTop, aFrameRight, aFrameBottom, aCenter, aLeft, aUp, aRight, aDown, aLeftUp, aLeftDown, aRightUp, aRightDown, aTile, aScale, aStretch);
    TChannelAlign =  aFrame..aTile;
    TImageAlign   =  aCenter..aStretch;
    TTextAlign    =  aCenter..aRightDown;
    TGlyphAlign   =  TTextAlign;

    TProChannel   = class(TPersistent)
    private
      FActive     : Boolean;
      FSize       : TPoint;
      FImage      : Pointer;
      FPixelSize  : Byte;
      procedure   SetSize(const ASize: TPoint);
      procedure   SetActive(AActive: Boolean);
      procedure   Activate; virtual; abstract;
      procedure   Deactivate; virtual; abstract;
      function    GetExtents: TRect;
    public
      function    DataSize: Integer;
      function    Seek(APosX, APosY: Integer): Pointer;
      function    CorrectSeek(APosX, APosY: Integer): Pointer;
      function    DirectSeek(APosX, APosY: Integer): Pointer;
      constructor Create(APixelSize: Byte);
      destructor  Destroy; override;
      property    Extents: TRect read GetExtents;
      property    Size: TPoint read FSize write SetSize;
      property    Active: Boolean read FActive write SetActive;
      property    PixelSize: Byte read FPixelSize;
      property    Image: Pointer read FImage;
    end;

    TChannelMode  = (cmColorChannel, cmAlphaChannel);

    TChannel      = class(TProChannel)
    private
      FPalette    : PRGBPalette;
      FMode       : TChannelMode;
      FOnChange   : TNotifyEvent;
      procedure   Activate; override;
      procedure   Deactivate; override;
    public
      constructor Create(AMode: TChannelMode);
      procedure   DefineProperties(Filer: TFiler); override;
      procedure   Assign(Source: TPersistent); override;
      procedure   LoadFromFileStream(AStream: TStream);
      procedure   LoadFromStream(AStream: TStream);
      procedure   SaveToStream(AStream: TStream);
      procedure   SaveToFile(const AFileName: String);
      procedure   LoadFromResourceID(ResID: Integer);
      procedure   LoadFromResourceName(const ResName: String);
      procedure   LoadFromFile(const AFileName: TFileName);
      property    Palette: PRGBPalette read FPalette;
      property    Mode: TChannelMode read FMode;
      property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TRGBChannel   = class;

    TRGBCanvas    = class(TCanvas)
    private
      FBitmap     : HBitmap;
    public
      RGBChannel: TRGBChannel;
      constructor Create(AChannel: HBitmap);
      destructor  Destroy; override;
    end;

    TRGBChannel   = class(TProChannel)
    private
      FHandle     : HBitmap;
      FCanvas     : TRGBCanvas;
      procedure   Activate; override;
      procedure   Deactivate; override;
      procedure   AlphaChannelFrame(const AClip, RGBArea: TRect; AColor: TColor; AAlphaChannel: TChannel);
    public
      constructor Create; virtual;
      property    Handle: HBitmap read FHandle;
      property    Canvas: TRGBCanvas read FCanvas;
    {Drawing methods}
      procedure   Luminosity(ALuminosity: Shortint);
      procedure   LuminosityRect(const AClip: TRect; ALuminosity: Shortint);
      procedure   WhiteAlphaChannel(const AClip: TRect; const APos: TPoint; AAlphaChannel: TChannel);
      procedure   ColorAlphaChannel(const AClip: TRect; const APos: TPoint; AColor: TColor; AAlphaChannel: TChannel; AAlpha: Byte);
      procedure   ColorChannel(const AClip: TRect; const APos: TPoint; AColorChannel: TChannel; AAlpha: Byte);
      procedure   AlphaColorChannel(const AClip: TRect; const APos: TPoint; AColorChannel, AAlphaChannel: TChannel; AAlpha: Byte);
      procedure   AlphaChannelAlign(const AClip, ARect: TRect; AColor: TColor; AAlphaChannel: TChannel; AAlign: TChannelAlign);

      procedure   TranspRGBChannel(const AClip: TRect; const APos: TPoint; ARGB2Channel: TRGBChannel; ATransparentColor: TColor; AAlpha: Byte);
    end;

    TClipChannel = class(TRGBChannel)
    private
      FRegions   : PRgnData;
      FRegMem    : Integer;
      FRects     : PRects;
      FRegion    : HRGN;
      FDrawMode  : Boolean;
    public
      function   Resize(ANewSize: TPoint): Boolean;
      destructor Destroy; override;
      procedure Invalidate(const ARect: TRect);
      procedure InvalidateAll;
      function  DrawBegin: Boolean;
      procedure DrawEnd;
    {Drawing methods}
      procedure Luminosity(ALuminosity: Shortint);
      procedure WhiteAlphaChannel(const APos: TPoint; AAlphaChannel: TChannel);
      procedure ColorAlphaChannel(const APos: TPoint; AColor: TColor; AAlphaChannel: TChannel; AAlpha: Byte);
      procedure ColorChannel(const APos: TPoint; AColorChannel: TChannel; AAlpha: Byte);
      procedure AlphaColorChannel(const APos: TPoint; AColorChannel, AAlphaChannel: TChannel; AAlpha: Byte);
      procedure AlphaChannelAlign(const ARect: TRect; AColor: TColor; AAlphaChannel: TChannel; AAlign: TChannelAlign);
    end;

    EChannelError = class(Exception);

function  AlignRect(const ARect: TRect; const ASize: TPoint; AAlign: TAligns): TRect;

implementation

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;




      function    AlignInteger(AInteger: Longint): Longint;
      begin
        Result := ((AInteger + 3) shr 2) shl 2;
      end;

      procedure   AlignPointer(var APointer: Pointer);
      begin
        APointer := Pointer(((Longint(APointer) + 3) shr 2) shl 2);
      end;

      procedure   LumConstant(ARGBs: Pointer; Count, ALuminosity: Integer); assembler; pascal;
      asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI, ARGBs
        MOV     EDI, ARGBs
        MOV     ECX, Count
        MOV     EAX, ALuminosity
        TEST    AL, $80
        JZ      @@Darken
      @@Lighten:         {Prepare Scaler}
        MOV     BX, AX
        SUB     BX, $80
        JZ      @@Finish
      @@LCycle:
        XOR     AX, AX
        LODSB
        MOV     DX, $00FF
        XCHG    AX, DX
        SUB     AX, DX

        MUL     BL
        SHR     AX, 7
        ADD     AL, DL
        STOSB
        LOOP    @@LCycle
        JMP     @@Finish
      @@Darken:          {Prepare Scaler}
        MOV     BX, AX
      @@DCycle:
        LODSB
        MUL     BL
        SHR     AX, 7
        STOSB
        LOOP    @@DCycle
      @@Finish:
        POP     EBX
        POP     EDI
        POP     ESI
      end;

      procedure   LumBuffer(RGBs, Lums: Pointer; Count: Integer); assembler; pascal;
      asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI, Lums
        MOV     EDI, RGBs
        MOV     ECX, Count
      @@CycleBegin:
        PUSH    ECX
        MOV     ECX, 3
        XOR     AX, AX
        LODSB
        CMP     AX, $007F
        JG      @@Lighten
        JL      @@Darken
        ADD     EDI, 3
        JMP     @@Continue
      @@Lighten:
        MOV     BX, AX
        SUB     BX, $80
      @@LCycle:
        XOR     AX, AX
        MOV     AL, [EDI]
        MOV     DX, $00FF
        XCHG    AX, DX
        SUB     AX, DX
        MUL     BL
        SHR     AX, 7
        ADD     AL, DL
        STOSB
        LOOP    @@LCycle
        JMP     @@Continue
      @@Darken:
        MOV     BX, AX
      @@DCycle:
        XOR     AX, AX
        MOV     AL, [EDI]
        MUL     BL
        SHR     AX, 7
        STOSB
        LOOP    @@DCycle
      @@Continue:
        POP     ECX
        LOOP    @@CycleBegin
        POP     EBX
        POP     EDI
        POP     ESI
      end;


{AL - Overlay Color}
{DL - Original Color}
{BL - Scaler}
{Result - AL}
      procedure   ScaleComponent; assembler;
      asm
        SUB     EAX, EDX
        PUSH    EDX
        IMUL    EBX
        POP     EDX
        SHL     EDX, 8
        ADD     EAX, EDX
        SHR     EDX, 8
        SHR     EAX, 8
      end;

      { Для ColorCannel }
      procedure   MergeColors(RGBs, Indices, Palette: Pointer; Count, Scaler: Integer); assembler; pascal;
      asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI, Indices
        MOV     EDI, RGBs
        MOV     ECX, Count
        MOV     EBX, Scaler
      @@CycleBegin:
        XOR     EAX, EAX
        LODSB
        PUSH    ESI
        MOV     ESI, Palette
        SHL     EAX, 2
        ADD     ESI, EAX
{Blue}
        XOR     EAX, EAX
        LODSB
        MOV     DL, [EDI]
        CALL    ScaleComponent
        STOSB
{Green}
        XOR     EAX, EAX
        LODSB
        MOV     DL, [EDI]
        CALL    ScaleComponent
        STOSB
{Red}
        XOR     EAX, EAX
        LODSB
        MOV     DL, [EDI]
        CALL    ScaleComponent
        STOSB
      @@Continue:
        POP     ESI
        LOOP    @@CycleBegin
        POP     EBX
        POP     EDI
        POP     ESI
      end;

{EAX - Overlay Color}
{EDX - Original Color}
{EBX - Scaler}
{ECX - Scaler 2}
{Result - EAX}

      procedure   DoubleScaleComponent; assembler;
      asm
        SUB     EAX, EDX
        PUSH    EDX
        IMUL    EBX
        IMUL    ECX
        POP     EDX
        SHL     EDX, 16
        ADD     EAX, EDX
        SHR     EDX, 16
        SHR     EAX, 16
      end;

      { Для AlphaColorCannel }
      procedure   MergeMaskColors(RGBs, Alphas, Indices, Palette: Pointer; Count, Scaler: Integer); assembler; pascal;
      asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI, Indices
        MOV     EDI, RGBs
        MOV     ECX, Count
        XOR     EDX, EDX
        XOR     EBX, EBX
      @@CycleBegin:
        PUSH    ECX
        MOV     ECX, Scaler
        XOR     EAX, EAX
        LODSB
        PUSH    ESI
        MOV     ESI, Alphas
        MOV     BL, [ESI]
        INC     ESI
        MOV     Alphas, ESI
        MOV     ESI, Palette
        SHL     EAX, 2
        ADD     ESI, EAX
        CMP     BX, $FF
        JE      @@Copy
        JNZ     @@Work
      @@Skip:
        ADD     EDI, 3
        JMP     @@Continue
      @@Copy:
        CMP     CX, $FF
        JNE     @@Work
        LODSB
        STOSB
        LODSB
        STOSB
        LODSB
        STOSB
        JMP     @@Continue
      @@Work:
{Blue}
        XOR     EAX, EAX
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
{Green}
        XOR     EAX, EAX
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
{Red}
        XOR     EAX, EAX
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
      @@Continue:
        POP     ESI
        POP     ECX
        LOOP    @@CycleBegin
        POP     EBX
        POP     EDI
        POP     ESI
      end;


{EAX - Overlay Color}
{EDX - Original Color}
{EBX - Scaler}
{ECX - Scaler 2}
{Result - EAX}

      { Для ColorAlphaCannel }
      procedure   MergeMaskOneColor(RGBs, Alphas, ColorRGB: Pointer; Count, Scaler: Integer); assembler; pascal;
      asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     EDI, RGBs
        MOV     ECX, Count
        MOV     ESI, Alphas
      @@CycleBegin:
        XOR     EAX, EAX
        MOV     EDX, EAX
        LODSB
        CMP     EAX, $000000FF
        JE      @@Copy
        JNZ     @@Work
        ADD     EDI, 3
        LOOP    @@CycleBegin
        JMP     @@End
      @@Copy:
        PUSH    ESI
        MOV     ESI, ColorRGB
        LODSB
        STOSB
        LODSB
        STOSB
        LODSB
        STOSB
        POP     ESI
        LOOP    @@CycleBegin
        JMP     @@End
      @@Work:
        MOV     EBX, EAX
        PUSH    ECX
        MOV     ECX, Scaler
        PUSH    ESI
        MOV     ESI, ColorRGB
{Blue}
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
{Green}
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
{Red}
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
        POP     ESI
        POP     ECX
        LOOP    @@CycleBegin
      @@End:
        POP     EBX
        POP     EDI
        POP     ESI
      end;

      procedure   MergeOneMaskOneColor(RGBs: Pointer; Alpha: Integer; ColorRGB: Pointer; Count, Scaler: Integer); assembler; pascal;
      asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     EDI, RGBs
        MOV     ECX, Count
      @@CycleBegin:
        MOV     EAX, Alpha
        XOR     EDX, EDX
        CMP     EAX, $000000FF
        JE      @@Copy
        JNZ     @@Work
        ADD     EDI, 3
        LOOP    @@CycleBegin
        JMP     @@End
      @@Copy:
        MOV     ESI, ColorRGB
        LODSB
        STOSB
        LODSB
        STOSB
        LODSB
        STOSB
        LOOP    @@CycleBegin
        JMP     @@End
      @@Work:
        MOV     EBX, EAX
        PUSH    ECX
        MOV     ECX, Scaler
        MOV     ESI, ColorRGB
{Blue}
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
{Green}
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
{Red}
        LODSB
        MOV      DL, [EDI]
        CALL    DoubleScaleComponent
        STOSB
        POP     ECX
        LOOP    @@CycleBegin
      @@End:
        POP     EBX
        POP     EDI
        POP     ESI
      end;

      procedure   MergeTransparentColor(RGBs, RGB2s: Pointer; TransparentColorRGB: Integer; Count, Scaler: Integer); assembler; pascal;
      asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     EBX, Scaler
        MOV     ESI, RGB2s
        MOV     EDI, RGBs
        MOV     ECX, Count
        XOR     EDX, EDX
      @@CycleBegin:
        { проверка пиксела на прозрачность }
        MOV     EAX, [ESI]
        AND     EAX, $00FFFFFF
        {PUSH    ESI
        XOR     EAX, EAX
        LODSB
        SHL     EAX, 8
        LODSB
        SHL     EAX, 8
        LODSB
        POP     ESI
        XOR     EAX, EAX{}
        CMP     EAX, TransparentColorRGB
        JNE     @@Work
      @@Skip:
        ADD     EDI, 3
        ADD     ESI, 3
        JMP     @@Continue
      @@Work:
        { покомпонентное умножение коэффициентов }
{Blue}
        XOR     AX, AX
        LODSB
        MOV     DL, [EDI]
        CALL    ScaleComponent
        STOSB
{Green}
        XOR     AX, AX
        LODSB
        MOV     DL, [EDI]
        CALL    ScaleComponent
        STOSB
{Red}
        XOR     AX, AX
        LODSB
        MOV     DL, [EDI]
        CALL    ScaleComponent
        STOSB

      @@Continue:
        LOOP    @@CycleBegin
      @@End:
        POP     EBX
        POP     EDI
        POP     ESI
      end;

{TProChannel ******************************************************************}

function    TProChannel.GetExtents: TRect;
begin
  Result := Bounds(0, 0, Size.X, Size.Y);
end;

procedure   TProChannel.SetSize(const ASize: TPoint);
var SaveActive: Boolean;
begin
  if (ASize.X <> FSize.X) or (ASize.Y <> FSize.Y) then
  begin
    SaveActive := Active;
    Active     := False;
    FSize      := ASize;
    Active     := SaveActive;
  end;
end;

procedure   TProChannel.SetActive(AActive: Boolean);
begin
  if AActive <> FActive then
  begin
    if AActive
    then Activate
    else Deactivate;
    FActive := AActive;
  end;
end;

function    TProChannel.DataSize: Integer;
begin
  Result := AlignInteger(FSize.X * FPixelSize) * FSize.Y;
end;

function    TProChannel.Seek(APosX, APosY: Integer): Pointer;
begin
  Result := Pointer(Longint(FImage) + APosX * FPixelSize + AlignInteger(FSize.X * PixelSize) * (FSize.Y - APosY));
end;

function    TProChannel.CorrectSeek(APosX, APosY: Integer): Pointer;
begin
  Result := Pointer(Longint(FImage) + APosX * FPixelSize + AlignInteger(FSize.X * PixelSize) * (FSize.Y-1 - APosY));
end;

function    TProChannel.DirectSeek(APosX, APosY: Integer): Pointer;
begin
  Result := Pointer(Longint(FImage) + APosX * FPixelSize + AlignInteger(FSize.X * PixelSize) * (APosY));
end;

constructor TProChannel.Create(APixelSize: Byte);
begin
  inherited Create;
  FPixelSize := APixelSize;
end;

destructor  TProChannel.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

{TChannel *********************************************************************}

procedure   TChannel.LoadFromStream(AStream: TStream);
var
  BC: TBitmapCoreHeader;
  BI: TBitmapInfoHeader;
  ASize: Integer;
  TempTriples: Array[0..255] of TRGBTriple;
  TempEntries: Array[0..255] of TPaletteEntry;

  procedure TripleToReverseQuad;
  var I: Integer;
  begin
    for I := 0 to 255 do
    begin
      FPalette^[I].R := TempTriples[I].rgbtRed;
      FPalette^[I].G := TempTriples[I].rgbtGreen;
      FPalette^[I].B := TempTriples[I].rgbtBlue;
      FPalette^[I].C := 0;
    end;
  end;

  procedure EntryToReverseQuad;
  var I: Integer;
  begin
    for I := 0 to 255 do
    begin
      FPalette^[I].R := TempEntries[I].peRed;
      FPalette^[I].G := TempEntries[I].peGreen;
      FPalette^[I].B := TempEntries[I].peBlue;
      FPalette^[I].C := 0;
    end;
  end;

begin
  Active := False;
  AStream.Read(ASize, SizeOf(ASize));
  if ASize = 0 then exit else
  if ASize = SizeOf(BC) then
  begin
    AStream.Read(BC.bcWidth, SizeOf(BC) - SizeOf(ASize));
    Size := Point(BC.bcWidth, BC.bcHeight);
    if BC.bcBitCount <> 8 then raise EChannelError.Create('Channels support 256-colors bitmaps only!');
    AStream.Read(TempTriples, 256 * SizeOf(TRGBTriple));
    Active := True;
    if Mode = cmColorChannel
    then TripleToReverseQuad;
  end else if ASize = SizeOf(BI) then
  begin
    AStream.Read(BI.biWidth, SizeOf(BI) - SizeOf(ASize));
    Size := Point(BI.biWidth, BI.biHeight);
    if BI.biBitCount <> 8 then raise EChannelError.Create('Channels support 256-colors bitmaps only!');
    if BI.biClrUsed = 0 then BI.biClrUsed := 256;
    AStream.Read(TempEntries, BI.biClrUsed * SizeOf(TRGBQuad));
    Active := True;
    if Mode = cmColorChannel
    then EntryToReverseQuad;
  end else raise EChannelError.Create('Invalid stream format!');
  AStream.Read(Image^, DataSize);
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure   TChannel.LoadFromResourceID(ResID: Integer);
var F: TStream;
begin
  F := TResourceStream.CreateFromID(HInstance, ResID, RT_BITMAP);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure   TChannel.LoadFromResourceName(const ResName: String);
var F: TStream;
begin
  F := TResourceStream.Create(HInstance, ResName, RT_BITMAP);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure   TChannel.LoadFromFileStream(AStream: TStream);
var BF: TBitmapFileHeader;
begin
  AStream.Read(BF, SizeOf(BF));
  LoadFromStream(AStream);
end;

procedure   TChannel.LoadFromFile(const AFileName: TFileName);
var F : TStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromFileStream(F);
  finally
    F.Free;
  end;
end;

procedure TChannel.SaveToFile(const AFileName: String);
var F : TStream;
    BF: TBitmapFileHeader;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    with BF do
    begin
      bfType := $1234;
      bfSize := $0;
      bfReserved1 := 0;
      bfReserved2 := 0;
      bfOffBits := 0;
    end;
    F.Write(BF, SizeOf(BF));
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TChannel.SaveToStream(AStream: TStream);
var
  BI: TBitmapInfoHeader;
  TempEntries: Array[0..255] of TPaletteEntry;

  procedure ReverseQuadToEntry;
  var I: Integer;
  begin
    for I := 0 to 255 do
    begin
      TempEntries[I].peRed := FPalette^[I].R;
      TempEntries[I].peGreen := FPalette^[I].G;
      TempEntries[I].peBlue := FPalette^[I].B;
      TempEntries[I].peFlags := 0;
    end;
  end;

begin
  FillChar(BI, SizeOf(BI), 0);
  if Active then
  begin
    BI.biSize := SizeOf(BI);
    BI.biWidth := Size.X;
    BI.biHeight := Size.Y;
    BI.biBitCount := 8;
    BI.biClrUsed := 256;
    AStream.Write(BI, SizeOf(BI));
    ReverseQuadToEntry;
    AStream.Write(TempEntries, SizeOf(TempEntries));
    AStream.Write(Image^, DataSize);
  end else AStream.Write(BI, SizeOf(BI.biSize));
end;

procedure   TChannel.Activate;
begin
  FImage := AllocMem(DataSize);
  FPalette := AllocMem(SizeOf(FPalette^));
end;

procedure   TChannel.Deactivate;
begin
  FreeMem(FImage, DataSize);
  FreeMem(FPalette, SizeOf(FPalette^));
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TChannel.Create(AMode: TChannelMode);
begin
  inherited Create(1);
  FMode := AMode;
end;

procedure   TChannel.Assign(Source: TPersistent);
begin
  Active := False;
  if Source is TChannel and TChannel(Source).Active then
  begin
    Size := TChannel(Source).Size;
    Active := True;
    Move(TChannel(Source).FPalette^, FPalette^, SizeOf(FPalette^));
    Move(TChannel(Source).FImage^, FImage^, DataSize);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure   TChannel.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    Ancestor: TChannel;
  begin
    Result := Active;
    if (Filer.Ancestor <> nil) and (Filer.Ancestor is TChannel) then
    begin
      Ancestor := TChannel(Filer.Ancestor);
      Result := (Active <> Ancestor.Active) or
                (Size.X <> Ancestor.Size.X) or
                (Size.Y <> Ancestor.Size.X) or
                (Mode <> Ancestor.Mode) or
                ((Mode = cmColorChannel) and not CompareMem(FPalette, Ancestor.FPalette, SizeOf(FPalette))) or
                not CompareMem(FImage, Ancestor.FImage, DataSize);
    end;
  end;

begin
  Filer.DefineBinaryProperty('Data', LoadFromStream, SaveToStream, DoWrite);
end;

{TRGBCanvas *******************************************************************}

constructor TRGBCanvas.Create(AChannel: HBitmap);
var NewHandle: HDC;
begin
  inherited Create;
  NewHandle := CreateCompatibleDC(0);
  FBitmap := SelectObject(NewHandle, AChannel);
  Handle := NewHandle;
end;

destructor  TRGBCanvas.Destroy;
var KillHandle: HDC;
begin
  KillHandle := Handle;
  SelectObject(KillHandle, FBitmap);
  Handle := 0;
  DeleteDC(KillHandle);
  inherited Destroy;
end;

{TRGBChannel ******************************************************************}

constructor TRGBChannel.Create;
begin
  inherited Create(3);
end;

procedure   TRGBChannel.Activate;
var
  BitmapInfo: TBitmapInfo;
begin
  with BitmapInfo.bmiHeader do
  begin
    biSize         := SizeOf(TBitmapInfoHeader);
    biWidth        := Size.X;
    biHeight       := Size.Y;
    biPlanes       := 1;
    biBitCount     := 24;
    biCompression  := BI_RGB;
    biSizeImage    := 0;
    biXPelsPerMeter:= 0;
    biYPelsPerMeter:= 0;
    biClrUsed      := 0;
    biClrImportant := 0;
  end;
  FHandle := CreateDIBSection(0, BitmapInfo, DIB_RGB_COLORS, FImage, 0, 0);
  if FHandle = 0 then raise EChannelError.Create('Unable to Create DIB Section!');
  FCanvas := TRGBCanvas.Create(FHandle);
  FCanvas.RGBChannel := Self;
end;

procedure   TRGBChannel.Deactivate;
begin
  FCanvas.Free;
  FCanvas := Nil;
  DeleteObject(FHandle);
  FHandle := 0;
end;

procedure   TRGBChannel.Luminosity(ALuminosity: Shortint);
begin
  if Active and (ALuminosity <> 0) then
    LumConstant(Image, Size.Y * AlignInteger(Size.X * 3), ALuminosity + 128);
end;

procedure   TRGBChannel.LuminosityRect(const AClip: TRect; ALuminosity: Shortint);
var Area : TRect;
    I,C  : Integer;
    WRGB : Pointer;
    ALum : Byte;
begin
  ALum := ALuminosity + 128;
  Area := Extents;
  if Active and IntersectRect(Area, Area, AClip) then
  begin
    WRGB := Seek(0, Area.Bottom);
    for I := Area.Top to Area.Bottom - 1 do
    begin
      AlignPointer(WRGB);
      {Skip Left Pixels}
      C := Area.Left;
      Inc(Longint(WRGB), FPixelSize * C);
      {Paint Center Pixels}
      C := Area.Right - Area.Left;
      LumConstant(WRGB, FPixelSize * C, ALum);
      Inc(Longint(WRGB), FPixelSize * C);
      {Skip Right Pixels}
      C := Size.X - Area.Right;
      Inc(Longint(WRGB), FPixelSize * C);
    end;
  end;
end;

procedure   TRGBChannel.ColorAlphaChannel(const AClip: TRect; const APos: TPoint; AColor: TColor; AAlphaChannel: TChannel; AAlpha: Byte);
var RGBArea : TRect;
    LumArea : TRect;
    RGBColor: Packed Array[0..2] of Byte;
    WRGB    : Pointer;
    WLum    : Pointer;
    I,C1,C2 : Integer;
begin
  RGBArea := Extents;
  LumArea := Bounds(APos.X, APos.Y, AAlphaChannel.Size.X, AAlphaChannel.Size.Y);
  if Active
  and (AAlphaChannel.Active) and (AAlphaChannel.Mode = cmAlphaChannel)
  and IntersectRect(RGBArea, RGBArea, AClip) then
  if IntersectRect(LumArea, RGBArea, LumArea) then
  begin
    AColor := ColorToRGB(AColor);
    RGBColor[2] := (AColor) and $FF;
    RGBColor[1] := (AColor shr 8) and $FF;
    RGBColor[0] := (AColor shr 16) and $FF;
    RGBArea := LumArea;
    OffsetRect(LumArea, -APos.X, -APos.Y);
    WRGB := Seek(0, RGBArea.Bottom);
    WLum := AAlphaChannel.Seek(0, LumArea.Bottom);
    for I := RGBArea.Top to RGBArea.Bottom - 1 do
    begin
      C1 := RGBArea.Left;
      C2 := LumArea.Left;
      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AAlphaChannel.PixelSize);

      C1 := RGBArea.Right - RGBArea.Left;
      MergeMaskOneColor(WRGB, WLum, @RGBColor, C1, AAlpha);

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C1 * AAlphaChannel.PixelSize);

      C1 := Size.X - RGBArea.Right;
      C2 := AAlphaChannel.Size.X - LumArea.Right;

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AAlphaChannel.PixelSize);

      AlignPointer(WRGB);
      AlignPointer(WLum);
    end;
  end;
end;

procedure   TRGBChannel.WhiteAlphaChannel(const AClip: TRect; const APos: TPoint; AAlphaChannel: TChannel);
var RGBArea : TRect;
    LumArea : TRect;
    WRGB    : Pointer;
    WLum    : Pointer;
    I,C1,C2 : Integer;
begin
  RGBArea := Extents;
  LumArea := Bounds(APos.X, APos.Y, AAlphaChannel.Size.X, AAlphaChannel.Size.Y);
  if Active
  and (AAlphaChannel.Active) and (AAlphaChannel.Mode = cmAlphaChannel)
  and IntersectRect(RGBArea, RGBArea, AClip) then
  if IntersectRect(LumArea, RGBArea, LumArea) then
  begin
    RGBArea := LumArea;
    OffsetRect(LumArea, -APos.X, -APos.Y);
    WRGB := Seek(0, RGBArea.Bottom);
    WLum := AAlphaChannel.Seek(0, LumArea.Bottom);
    for I := RGBArea.Top to RGBArea.Bottom - 1 do
    begin
      C1 := RGBArea.Left;
      C2 := LumArea.Left;
      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AAlphaChannel.PixelSize);

      C1 := RGBArea.Right - RGBArea.Left;
      LumBuffer(WRGB, WLum, C1);

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C1 * AAlphaChannel.PixelSize);

      C1 := Size.X - RGBArea.Right;
      C2 := AAlphaChannel.Size.X - LumArea.Right;

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AAlphaChannel.PixelSize);

      AlignPointer(WRGB);
      AlignPointer(WLum);
    end;
  end;
end;

procedure   TRGBChannel.ColorChannel(const AClip: TRect; const APos: TPoint; AColorChannel: TChannel; AAlpha: Byte);
var RGBArea : TRect;
    LumArea : TRect;
    WRGB    : Pointer;
    WLum    : Pointer;
    I,C1,C2 : Integer;
begin
  RGBArea := Extents;
  LumArea := Bounds(APos.X, APos.Y, AColorChannel.Size.X, AColorChannel.Size.Y);
  if Active
  and (AColorChannel.Active) and (AColorChannel.Mode = cmColorChannel)
  and IntersectRect(RGBArea, RGBArea, AClip)
  and IntersectRect(LumArea, RGBArea, LumArea) then
  begin
    RGBArea := LumArea;
    OffsetRect(LumArea, -APos.X, -APos.Y);
    WRGB := Seek(0, RGBArea.Bottom);
    WLum := AColorChannel.Seek(0, LumArea.Bottom);
    for I := RGBArea.Top to RGBArea.Bottom - 1 do
    begin
      C1 := RGBArea.Left;
      C2 := LumArea.Left;
      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AColorChannel.PixelSize);

      C1 := RGBArea.Right - RGBArea.Left;
      MergeColors(WRGB, WLum, AColorChannel.Palette, C1, AAlpha);

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C1 * AColorChannel.PixelSize);

      C1 := Size.X - RGBArea.Right;
      C2 := AColorChannel.Size.X - LumArea.Right;

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AColorChannel.PixelSize);

      AlignPointer(WRGB);
      AlignPointer(WLum);
    end;
  end;
end;

procedure   TRGBChannel.AlphaColorChannel(const AClip: TRect; const APos: TPoint; AColorChannel, AAlphaChannel: TChannel; AAlpha: Byte);
var RGBArea : TRect;
    LumArea : TRect;
    WRGB    : Pointer;
    WLum    : Pointer;
    WCol    : Pointer;
    I,C1,C2 : Integer;
begin
  RGBArea := Extents;
  LumArea := Bounds(APos.X, APos.Y, AColorChannel.Size.X, AColorChannel.Size.Y);
  if Active
  and (AColorChannel.Active) and (AColorChannel.Mode = cmcolorChannel)
  and (AAlphaChannel.Active) and (AAlphaChannel.Mode = cmAlphaChannel)
  and EqualRect(AColorChannel.Extents, AAlphaChannel.Extents)
  and IntersectRect(RGBArea, RGBArea, AClip)
  and IntersectRect(LumArea, RGBArea, LumArea) then
  begin
    RGBArea := LumArea;
    OffsetRect(LumArea, -APos.X, -APos.Y);
    WRGB := Seek(0, RGBArea.Bottom);
    WLum := AAlphaChannel.Seek(0, LumArea.Bottom);
    WCol := AColorChannel.Seek(0, LumArea.Bottom);
    for I := RGBArea.Top to RGBArea.Bottom - 1 do
    begin
      C1 := RGBArea.Left;
      C2 := LumArea.Left;
      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AAlphaChannel.PixelSize);
      Inc(Longint(WCol), C2 * AColorChannel.PixelSize);

      C1 := RGBArea.Right - RGBArea.Left;
      MergeMaskColors(WRGB, WLum, WCol, AColorChannel.Palette, C1, AAlpha);

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C1 * AAlphaChannel.PixelSize);
      Inc(Longint(WCol), C1 * AColorChannel.PixelSize);

      C1 := Size.X - RGBArea.Right;
      C2 := AColorChannel.Size.X - LumArea.Right;

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WLum), C2 * AAlphaChannel.PixelSize);
      Inc(Longint(WCol), C2 * AColorChannel.PixelSize);

      AlignPointer(WRGB);
      AlignPointer(WLum);
      AlignPointer(WCol);
    end;
  end;
end;

procedure   TRGBChannel.AlphaChannelFrame(const AClip, RGBArea: TRect; AColor: TColor; AAlphaChannel: TChannel);
var LumArea : TRect;
    WRGB    : Pointer;
    WLum    : Pointer;
    SLum    : Pointer;
    XC,YC,
    I,C     : Integer;

    RGBColor: Packed Array[0..2] of Byte;
begin
  if  (RGBArea.Right - RGBArea.Left >= AAlphaChannel.Size.X)
  and (RGBArea.Bottom - RGBArea.Top >= AAlphaChannel.Size.Y) then
  begin
    AColor := ColorToRGB(AColor);
    RGBColor[2] := {64;//}(AColor) and $FF;
    RGBColor[1] := {0;//}(AColor shr 8) and $FF;
    RGBColor[0] := {0;//}(AColor shr 16) and $FF;


    LumArea := AAlphaChannel.Extents;
    YC := AAlphaChannel.Size.Y shr 1;
    XC := AAlphaChannel.Size.X shr 1;
    WRGB := Seek(0, RGBArea.Bottom);
    WLum := AAlphaChannel.Image;
    for I := RGBArea.Top to RGBArea.Bottom - 1 do
    begin
      AlignPointer(Pointer(WRGB));
      AlignPointer(Pointer(WLum));
      SLum := WLum;

      Inc(Longint(WRGB), RGBArea.Left * PixelSize);

      if AColor = clNone then LumBuffer(WRGB, WLum, XC)
                         else MergeMaskOneColor(WRGB, WLum, @RGBColor, XC, 255);

      Inc(Longint(WRGB), XC * PixelSize);
      Inc(Longint(WLum), XC * AAlphaChannel.PixelSize);

      C := RGBArea.Right - RGBArea.Left - XC - XC;

      if C > 0 then
      begin
        if AColor = clNone then
        begin
          if Byte(WLum^) <> 127 then LumConstant(WRGB,  C * PixelSize, Byte(WLum^));
        end
        else
          MergeOneMaskOneColor(WRGB, Byte(WLum^), @RGBColor, C {* PixelSize{}, 255);
        Inc(Longint(WRGB), C * PixelSize);
        Inc(Longint(WLum), AAlphaChannel.PixelSize);
      end;

      if AColor = clNone then LumBuffer(WRGB, WLum, XC)
                         else MergeMaskOneColor(WRGB, WLum, @RGBColor, XC, 255);


      Inc(Longint(WRGB), (XC + Size.X - RGBArea.Right) * PixelSize);
      Inc(Longint(WLum), XC * AAlphaChannel.PixelSize);

      if (I >= RGBArea.Top + YC) and (I < RGBArea.Bottom - YC - 1) then WLum := SLum;
    end;
  end;
end;

procedure   TRGBChannel.AlphaChannelAlign(const AClip, ARect: TRect; AColor: TColor; AAlphaChannel: TChannel; AAlign: TChannelAlign);
var RGBArea : TRect;
    NewClip : TRect;
    LumArea : TRect;
    MX, MY,
    XX, YY  : Integer;
begin
  if not Active or not AAlphaChannel.Active or not (AAlphaChannel.Mode = cmAlphaChannel) then exit;
  RGBArea := AlignRect(ARect, AAlphaChannel.Size, AAlign);
  if  (IntersectRect(NewClip, Extents, AClip))
  and (IntersectRect(NewClip, NewClip, RGBArea)) then
  begin
    if AAlign < aCenter then AlphaChannelFrame(NewClip, RGBArea, AColor, AAlphaChannel){} else
    if AAlign < aTile then
    begin
      if AColor = clNone
      then WhiteAlphaChannel(NewClip, RGBArea.TopLeft, AAlphaChannel)
      else ColorAlphaChannel(NewClip, RGBArea.TopLeft, AColor, AAlphaChannel, 255);
    end else
    begin
      LumArea := AAlphaChannel.Extents;
      MX := (RGBArea.Right - RGBArea.Left + LumArea.Right - 1) div LumArea.Right;
      MY := (RGBArea.Bottom - RGBArea.Top + LumArea.Bottom - 1) div LumArea.Bottom;
      LumArea.TopLeft := RGBArea.TopLeft;
      if AColor = clNone then
      for YY := 0 to MY do
      begin
        LumArea.Left := RGBArea.Left;
        for XX := 0 to MX do
        begin
          WhiteAlphaChannel(NewClip, LumArea.TopLeft, AAlphaChannel);
          Inc(LumArea.Left, LumArea.Right);
        end;
        Inc(LumArea.Top, LumArea.Bottom);
      end else
      for YY := 0 to MY do
      begin
        LumArea.Left := RGBArea.Left;
        for XX := 0 to MX do
        begin
          ColorAlphaChannel(NewClip, LumArea.TopLeft, AColor, AAlphaChannel, 255);
          Inc(LumArea.Left, LumArea.Right);
        end;
        Inc(LumArea.Top, LumArea.Bottom);
      end;
    end;
  end;
end;

{ моя собственная процедура объединения картинок с учётом прозрачности }
procedure TRGBChannel.TranspRGBChannel(const AClip: TRect;
  const APos: TPoint; ARGB2Channel: TRGBChannel; ATransparentColor: TColor;
  AAlpha: Byte);
var RGBArea : TRect;
    RGB2Area : TRect;
    WRGB    : Pointer;
    WRGB2    : Pointer;
    I,C1,C2 : Integer;
    RGBColor: packed array[0..3] of Byte;
    ReverseColor: Integer absolute RGBColor;
begin
  RGBArea := Extents;
  RGB2Area := Bounds(APos.X, APos.Y, ARGB2Channel.Size.X, ARGB2Channel.Size.Y);
  if Active
  and (ARGB2Channel.Active)
  and IntersectRect(RGBArea, RGBArea, AClip)
  and IntersectRect(RGB2Area, RGBArea, RGB2Area) then
  begin
    RGBArea := RGB2Area;
    OffsetRect(RGB2Area, -APos.X, -APos.Y);
    WRGB := Seek(0, RGBArea.Bottom);
    WRGB2 := ARGB2Channel.Seek(0, RGB2Area.Bottom);

    //ATransparentColor := ColorToRGB(ATransparentColor);
    RGBColor[3] := 0;
    RGBColor[2] := (ATransparentColor) and $FF;
    RGBColor[1] := (ATransparentColor shr 8) and $FF;
    RGBColor[0] := (ATransparentColor shr 16) and $FF;

    for I := RGBArea.Top to RGBArea.Bottom - 1 do
    begin
      C1 := RGBArea.Left;
      C2 := RGB2Area.Left;
      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WRGB2), C2 * ARGB2Channel.PixelSize);

      C1 := RGBArea.Right - RGBArea.Left;
      MergeTransparentColor(WRGB, WRGB2, {ATransparentColor}ReverseColor, C1, AAlpha);

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WRGB2), C1 * ARGB2Channel.PixelSize);

      C1 := Size.X - RGBArea.Right;
      C2 := ARGB2Channel.Size.X - RGB2Area.Right;

      Inc(Longint(WRGB), C1 * PixelSize);
      Inc(Longint(WRGB2), C2 * ARGB2Channel.PixelSize);

      AlignPointer(WRGB);
      AlignPointer(WRGB2);
    end;
  end;
end;


{TClipChannel ******************************************************************}

function TClipChannel.Resize(ANewSize: TPoint): Boolean;
begin
  if ANewSize.X < Size.X then ANewSize.X := Size.X;
  if ANewSize.Y < Size.Y then ANewSize.Y := Size.Y;
  Result := (Size.X <> ANewSize.X) or (Size.Y <> ANewSize.Y);
  if Result then 
  begin
    Size := ANewSize;
    InvalidateAll;
  end;
end;

destructor TClipChannel.Destroy;
begin
  DrawEnd;
  inherited Destroy;
end;

procedure TClipChannel.Invalidate(const ARect: TRect);
var TR: HRGN;
begin
  if not FDrawMode then
  begin
    if FRegion = 0 then FRegion := CreateRectRgn(0,0,0,0);
    TR := CreateRectRgnIndirect(ARect);
    CombineRgn(FRegion, FRegion, TR, RGN_OR);
    DeleteObject(TR);
  end;
end;

procedure TClipChannel.InvalidateAll;
begin
  if not FDrawMode then
  begin
    if FRegion <> 0 then DeleteObject(FRegion);
    FRegion := CreateRectRgn(0, 0, 5000, 5000);
  end;
end;

function TClipChannel.DrawBegin: Boolean;
begin
  if not FDrawMode then
  begin
    SelectClipRgn(Canvas.Handle, FRegion);
    FRegMem := GetRegionData(FRegion, 0, Nil);
    if FRegMem > 0 then
    begin
      FRegions := AllocMem(FRegMem);
      FRegions^.rdh.dwSize := SizeOf(TRgnDataHeader);
      FRegions^.rdh.iType := RDH_RECTANGLES;
      FRects := @(FRegions^.buffer);
      FDrawMode := GetRegionData(FRegion, FRegMem, FRegions) <> 0;
      if not FDrawMode then FreeMem(FRegions, FRegMem);
    end;
  end;
  Result := FDrawMode;
end;

procedure TClipChannel.DrawEnd;
begin
  if FRegions <> Nil then
  begin
    FreeMem(FRegions, FRegMem);
    FRegions := Nil;
  end;
  if FRegion <> 0 then
  begin
    DeleteObject(FRegion);
    FRegion := 0;
  end;
  FDrawMode := False;
end;

procedure TClipChannel.Luminosity(ALuminosity: Shortint);
var I: Integer;
begin
  if FDrawMode then
  for I := 0 to FRegions^.rdh.nCount - 1 do
  inherited LuminosityRect(FRects^[I], ALuminosity);
end;

procedure TClipChannel.WhiteAlphaChannel(const APos: TPoint; AAlphaChannel: TChannel);
var I: Integer;
begin
  if FDrawMode then
  for I := 0 to FRegions^.rdh.nCount - 1 do
  inherited WhiteAlphaChannel(FRects^[I], APos, AAlphaChannel);
end;

procedure TClipChannel.ColorAlphaChannel(const APos: TPoint; AColor: TColor; AAlphaChannel: TChannel; AAlpha: Byte);
var I: Integer;
begin
  if FDrawMode then
  for I := 0 to FRegions^.rdh.nCount - 1 do
  inherited ColorAlphaChannel(FRects^[I], APos, AColor, AAlphaChannel, AAlpha);
end;

procedure TClipChannel.ColorChannel(const APos: TPoint; AColorChannel: TChannel; AAlpha: Byte);
var I: Integer;
begin
  if FDrawMode then
  for I := 0 to FRegions^.rdh.nCount - 1 do
  inherited ColorChannel(FRects^[I], APos, AColorChannel, AAlpha);
end;

procedure TClipChannel.AlphaColorChannel(const APos: TPoint; AColorChannel, AAlphaChannel: TChannel; AAlpha: Byte);
var I: Integer;
begin
  if FDrawMode then
  for I := 0 to FRegions^.rdh.nCount - 1 do
  inherited AlphaColorChannel(FRects^[I], APos, AColorChannel, AAlphaChannel, AAlpha);
end;

procedure TClipChannel.AlphaChannelAlign(const ARect: TRect; AColor: TColor; AAlphaChannel: TChannel; AAlign: TChannelAlign);
var I: Integer;
begin
  if FDrawMode then
  for I := 0 to FRegions^.rdh.nCount - 1 do
  inherited AlphaChannelAlign(FRects^[I], ARect, AColor, AAlphaChannel, AAlign);
end;

{Routines}

function  AlignRect(const ARect: TRect; const ASize: TPoint; AAlign: TAligns): TRect;
var D1, D2: Double;
begin
  Result := ARect;
  if AAlign < aTile then
  begin
    case AAlign of
     aFrame..aFrameBottom: begin
                            case AAlign of
                            aFrameTop    : Result.Bottom := Result.Top + ASize.Y;
                            aFrameBottom : Result.Top    := Result.Bottom - ASize.Y;
                            aFrameLeft   : Result.Right  := Result.Left + ASize.X;
                            aFrameRight  : Result.Left   := Result.Right - ASize.X;
                            end;
                            exit;
                          end;
     aRight,
     aRightDown,
     aRightUp:  Result.Left :=  ARect.Right - ASize.X;
     aUp,
     aCenter,
     aDown:     Result.Left := (ARect.Right + ARect.Left - ASize.X) div 2;
     else       Result.Left := ARect.Left;
    end;
    case AAlign of
     aDown,
     aRightDown,
     aLeftDown: Result.Top  :=  ARect.Bottom - ASize.Y;
     aLeft,
     aRight,
     aCenter:   Result.Top  := (ARect.Bottom + ARect.Top - ASize.Y) div 2;
     else       Result.Top  := ARect.Top;
    end;
    Result.BottomRight := Point(Result.Left + ASize.X, Result.Top + ASize.Y);
  end else
  if AAlign = aScale then
  begin
    D1 := (ARect.Right - ARect.Left) / ASize.X;
    D2 := (ARect.Bottom - ARect.Top) / ASize.Y;
    if D1 > D2
    then begin
           Result := Bounds(ARect.Left, ARect.Top, Round(D2 * ASize.X), ARect.Bottom - ARect.Top);
           OffsetRect(Result, (ARect.Right - ARect.Left - Result.Right + Result.Left) div 2, 0);
         end
    else begin
           Result := Bounds(ARect.Left, ARect.Top, ARect.Right - ARect.Left, Round(D1 * ASize.Y));
           OffsetRect(Result, 0, (ARect.Bottom - ARect.Top - Result.Bottom + Result.Top) div 2);
         end;
  end;
end;

end.
