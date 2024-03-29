unit GeoString;

///// ������ �. 2016 

interface

function DegToDMS(Deg : double; mode : shortint) :string ; overload;
function DegToDMS(Deg : double; isLatitude:Boolean; mode : shortint; HideLitera: boolean) :string ; overload;
function DegToDMS(Deg : double; isLatitude:Boolean; mode : shortint; HideLitera: boolean; psz: integer) :string ; overload;
function StrToFloat2(s:String):Double;
function StrToLatLon(s:string; IsLatitude:Boolean)  :Double ;

implementation

uses SysUtils;

function DegToDMS(Deg : double; mode : shortint) :string ;
var D,M: Integer;
    s,Mm  :Double;
    sD, sM, sS, SMm : String;
    mZ : String;    /// MinusZero
begin
  if mode<2 then
  Begin
    Result := format('%.9f',[Deg]);
    if mode=1 then
      Result := Result + #176;
  End
  else
   Begin
      Mz :='';

      D := trunc(Deg);

      if (Deg<0) and (D=0) then
        Mz := '-';

      if Deg < 0 then
         Deg := -Deg;

      Mm := frac(Deg)*60;
      M := trunc(Mm);
      s := (frac(Deg)*60-M)*60;

      if round(s*1000) >= 60000 then
      begin
        s := s - 60;
        if s < 0 then
          s := 0;
        inc(M);
      end;
      if M >=60 then
      begin
        M := M - 60;
        if M < 0 then
          M := 0;
        inc(D);
      end;

     sD  := inttostr(trunc(D));
     sM  := inttostr(trunc(M));
     sMm := format('%.6f',[Mm]);
     sS  := format('%.4f',[s]);

     if M < 10 then
     begin
       sM := '0' + sM;
       sMm := '0' + sMm;
     end;

     if s < 10 then
       sS := '0' + sS;

     case mode of
       2: Result:= Mz + sD + ' ' + sMm;
       3: Result:= Mz + sD + #176 + ' ' + sMm + #39;
       4: Result:= Mz + sD + ' '+ sM +' '+Ss;
       5: Result:= Mz + sD + #176+' '+ sM + #39 + ' ' + Ss + #34;
      end;

   End;
end;

function DegToDMS(Deg : double; isLatitude:Boolean; mode : shortint; HideLitera: boolean) :string ;
var D,M : Integer;
    s, Mm :Double;
    Axis: String;
    sD, sM, sS, SMm : String;
begin

  if Deg < 0 then
  begin
   if isLatitude then
       Axis:='S '
      else
        Axis:='W ';

   if not HideLitera then
       Deg := -Deg;
  end
    else
      if isLatitude then
        Axis:='N '
      else
        Axis:='E ';

  if HideLitera then
     Axis:='';

  if mode < 2 then
  Begin
     Result := Axis + format('%.9f',[Deg]);
     if mode=1 then
       Result := Result + #176;
  End
  else
  Begin
    D := trunc(Deg);

    if Deg < 0 then
      if HideLitera then
      Begin
        Deg := -Deg;

        if D=0 then
          Axis :='-';
      End;

    Mm := frac(Deg)*60;
    M := trunc(Mm);
    //    M := trunc(frac(Deg)*60);
    s := (frac(Deg)*60-M)*60;
    if round(S*1000) >= 60000 then
    begin
      S := S - 60;
      if S < 0 then
        S := 0;
      inc(M);
    end;
    if M >=60 then
    begin
      M := M - 60;
      if M < 0 then
         M := 0;
      inc(D);
    end;

    sD  := inttostr(trunc(D));
    sM  := inttostr(trunc(M));
    sMm := format('%.6f',[Mm]);
    sS  := format('%.4f',[s]);

    if M < 10 then
    begin
     sM := '0' + sM;
     sMm := '0' + sMm;
    end;

    if s < 10 then
     sS := '0' + sS;

    case mode of
       2: Result:= Axis+ sD + ' ' + sMm;
       3: Result:= Axis+ sD + #176 + ' ' + sMm + #39;
       4: Result:= Axis+ sD + ' '+ sM +' '+Ss;
       5: Result:= Axis+ sD + #176+' '+ sM + #39 + ' ' + Ss + #34;
    end;

  end;
end;

function DegToDMS(Deg : double; isLatitude:Boolean; mode : shortint; HideLitera: boolean; psz: integer) :string ;   //// NEW 25.06

  function MyFormat(D:Double):string;
  begin
    case psz of
      0: result := IntToStr(round(D));
      1: result := format('%.1f',[D]);
      2: result := format('%.2f',[D]);
      3: result := format('%.3f',[D]);
      4: result := format('%.4f',[D]);
      5: result := format('%.5f',[D]);
      6: result := format('%.6f',[D]);
      7: result := format('%.7f',[D]);
      8: result := format('%.8f',[D]);
      9: result := format('%.9f',[D]);
      10: result := format('%.10f',[D]);
      11: result := format('%.11f',[D]);
      12: result := format('%.12f',[D]);
      13: result := format('%.13f',[D]);
      14: result := format('%.14f',[D]);
      15: result := format('%.15f',[D]);
      16: result := format('%.16f',[D]);
    end;

  end;

var D,M : Integer;
    s, Mm :Double;
    Axis: String;
    sD, sM, sS, SMm : String;
begin

  if psz <= -1 then
  begin
    result := DegToDMS(Deg, isLatitude, mode, HideLitera);
    exit
  end;

  if Deg < 0 then
  begin
   if isLatitude then
       Axis:='S '
      else
        Axis:='W ';

   if not HideLitera then
       Deg := -Deg;
  end
    else
      if isLatitude then
        Axis:='N '
      else
        Axis:='E ';

  if HideLitera then
     Axis:='';

  if mode < 2 then
  Begin
     Result := Axis + Myformat(Deg);
     if mode=1 then
       Result := Result + #176;
  End
  else
  Begin
    D := trunc(Deg);

    if Deg < 0 then
      if HideLitera then
      Begin
        Deg := -Deg;

        if D=0 then
          Axis :='-';
      End;

    Mm := frac(Deg)*60;
    M := trunc(Mm);
    //    M := trunc(frac(Deg)*60);
    s := (frac(Deg)*60-M)*60;
    if round(S*1000) >= 60000 then
    begin
      S := S - 60;
      if S < 0 then
        S := 0;
      inc(M);
    end;
    if M >=60 then
    begin
      M := M - 60;
      if M < 0 then
         M := 0;
      inc(D);
    end;

    sD  := inttostr(trunc(D));
    sM  := inttostr(trunc(M));


    sMm := Myformat(Mm);
    sS  := Myformat(s);

    if M < 10 then
    begin
     sM := '0' + sM;
     sMm := '0' + sMm;
    end;

    if s < 10 then
     sS := '0' + sS;

    case mode of
       2: Result:= Axis+ sD + ' ' + sMm;
       3: Result:= Axis+ sD + #176 + ' ' + sMm + #39;
       4: Result:= Axis+ sD + ' '+ sM +' '+Ss;
       5: Result:= Axis+ sD + #176+' '+ sM + #39 + ' ' + Ss + #34;
    end;

  end;
end;


function StrToFloat2(s:String):Double;

 function IsDigit(ch:char):boolean;
 begin
    IsDigit := false;
    if ch in ['0'..'9',DecimalSeparator] then
      IsDigit := true;
 end;

var i:integer;
begin

  for i:=length(s) Downto 1 do
    if (s[i]= ',') or  (s[i] = '.') then
        s[i] := DecimalSeparator;

  for i:=length(s) Downto 1 do
    if (not IsDigit(s[i]))
        and (not (s[i]='-'))
        and (not (s[i]='+'))
        and (not (AnsiUpperCase(s[i])='E'))
        and (not (AnsiUpperCase(s[i])='D')) then
          Delete(s,i,1);

  try
    StrToFloat2 := StrToFloat(s);
  except
    StrToFloat2 := 0;
  end;
end;


function StrToLatLon(s : string; IsLatitude: boolean) :Double ;

var    i, MaxCol, StartI, Sign :integer;
var South, West: boolean;

  function DeleteDegMinSecSeparators(s:string):string;
  var j:integer;
  begin
     Result := s;
     if result <> '' then
      for j:= length(Result)+1 DownTo 1 do
        Case Ord(Result[j]) of
            34, 39, 176 : Result[j] := ' ';
            Ord('N'),Ord('S'),Ord('W'),Ord('E'):
               if j=1 then
                 if Result[j+1]<>' ' then
                   Insert(' ', Result, j+1);
        end;
  end;


  function GetHemisphere(s:string; var South, West : boolean): boolean;
  begin
        Result :=false;
        if (s = 'N') or (s = 'E') then
        Begin
          Result := true;
        end
         else
           if  (s = 'S') then
           Begin
                Result := true;
                South := true;
           End
             else
              if (s = 'W') then
              Begin
                Result := true;
                West := true;
              End;
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

  function GetCols(str, sep: string; ColN, ColCount:integer): string;
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

begin

  South := false;
  West := false;

  Sign := 1;

  s := DeleteDegMinSecSeparators(s);

  MaxCol:= GetColN(S,' ',length(s));

  if MaxCol >= 1 then
  Begin

     StartI :=0;
       for i := 0 to MaxCol do
         if GetHemiSphere(GetCols(S,' ',i,1),South, West) then
         begin
           if i = 0 then
             StartI := 1;

           if ((isLatitude)and(South))or ((isLatitude=false)and(West)) then
              Sign :=-1;

           break;
         end;



     if  (StrToFloat2(GetCols(S,' ', StartI , 1)) <0) then    ///// - .. .. ..
     Begin
       Sign :=-1;

       Result:= ( StrToFloat2(GetCols(S,' ', StartI , 1))
             + Sign*StrToFloat2(GetCols(S,' ', StartI +1, 1))/60
             + Sign*StrToFloat2(GetCols(S,' ', StartI +2, 1))/3600 );


     End
      else
        if  (Pos('-',(GetCols(S,' ', StartI , 1))) > 0) then       ///// - 0 .. ..
        Begin
          Sign :=-1;

           Result:= Sign*( StrToFloat2(GetCols(S,' ', StartI , 1))
             + StrToFloat2(GetCols(S,' ', StartI +1, 1))/60
             + StrToFloat2(GetCols(S,' ', StartI +2, 1))/3600 );
     End
      else
         Result:= Sign*( StrToFloat2(GetCols(S,' ', StartI , 1))
             + StrToFloat2(GetCols(S,' ', StartI +1, 1))/60
             + StrToFloat2(GetCols(S,' ', StartI +2, 1))/3600 );

  End
   else
     Begin
       Result:= StrToFloat2(s);
     End;

  {if not IsLatitude then
    if West then
    begin
      Result := - Result;
    end;

    if IsLatitude then
    if South then
    begin
      Result := - Result;
    end;}
end;

end.
 