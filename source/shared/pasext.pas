unit PasExt;

{$warn 5023 off : no warning about unused units}
interface

uses
  {$if defined(darwin)}
    MacOSAll, { CocoaAll, CocoaUtils, }
  {$endif}
  Classes, SysUtils;

const
  {$if defined(windows)}
  PlatformID = 'WIN';
  {$elseif defined(darwin)}
  PlatformID = 'OSX';
  {$elseif defined(linux)}
  PlatformID = 'LNX';
  {$elseif defined(unix)}
  PlatformID = 'UNX';
  {$else}
  PlatformID = 'UNK';
  {$endif}

const
  SPACE       = #$20;
  TAB         = #$09;
  CR          = #$0d;
  LF          = #$0a;
  UNDERSCORE  = #$5f;
  CRLF        = #$0d#$0a;

{$I country.inc}

var
  UserLanguage  : String;      { User's Language }
  UserHomePath  : String;      { User's Home directory }
  AppDataPath   : String;      { Location for program data files }
  AppCfgPath    : String;      { Location of application config file }
  AppCfgFile    : String;      { Application Config File }
  UpdateServer  : String;      { Application Update Server URL }

type
  TFileList = array of string;
  TForEachFileFunc = function (FileName : String) : integer of object;

procedure InitPasExt(Identifier : String);

function VerifiedPath (Parent, SubDir : String) : string;

function PopDelim(var AStr : String; ADelim: String = SPACE): String; overload;
function FieldStr(AStr : String; AField : integer = 0; ADelim : String = SPACE) : String; overload;
function FieldStr(AStr : String; AField, ACount : integer; ADelim : String = SPACE) : String; overload;

function SubStr(AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
function SubStr(AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;
function Excise(var AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
function Excise(var AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;

function Implode(AStr: String; ADelim : String = SPACE) : String; overload;
procedure Explode(AStr : String; var AStrs : TStringList; ADelim : String; ATrim : boolean = false); overload;
procedure Explode(AStr : String; var AStrs : TStringList; ATrim : boolean = false); overload;

function Lookup(AStr : String; AStrs : TStringList; MatchCase : boolean = false) : LongInt; overload;
function LookupValue(AStr : String; AStrs : TStringList; Default : String; MatchCase : boolean = false) : String; overload;
function LookupValue(AStr : String; AStrs : TStringList; MatchCase : boolean = false) : String; overload;

function HasTrailing(ASubStr, AStr : String; CaseSpecific : boolean = true) : boolean; overload;
function ExcludeTrailing(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;
function IncludeTrailing(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;

function HasLeading(ASubStr, AStr : String; CaseSpecific : boolean = true) : boolean; overload;
function ExcludeLeading(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;
function IncludeLeading(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;

function WhenTrue(AState : boolean; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(AState : boolean; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(AState : boolean; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function WhenTrue(AStr : String; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(AStr : String; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(AStr : String; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function WhenTrue(AInt : Integer; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(AInt : Integer; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(AInt : Integer; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function WhenTrue(APtr : Pointer; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(APtr : Pointer; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(APtr : Pointer; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function ZeroPad(AValue : LongInt; AWidth: integer) : String; overload;
function LeftPad(AStr : String; AWidth: integer; ASubStr : String = SPACE) : String; overload;
function RightPad(AStr : String; AWidth: integer; ASubStr : String = SPACE) : String; overload;
function CenterPad(AStr : String; AWidth: integer; ASubStr : String = SPACE) : String; overload;

function SimpleCheckSum(const AStr : String) : word; overload;

function ForEachFile(AProc: TForEachFileFunc; APath : String; ARecurse : boolean = True) : integer; overload;
function FileList(APathSpec : String) : TFileList;

implementation

procedure InitPasExt(Identifier : String);
var
  Executable : String;
  {$if defined(darwin)}
    lbuf :  StringPtr;
  {$else}
    ltmp : String;
  {$endif}
begin
  UserLanguage := '';
  UserHomePath := IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('HOME'));
  Executable  := ExtractFileName(Paramstr(0));
  SetLength(Executable, Length(Executable) - Length(ExtractFileExt(ParamStr(0))));
  Executable := Lowercase(Executable);
  AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + Executable);
  AppCfgPath   := AppDataPath;
  AppCfgFile   := AppCfgPath + 'settings.xml';
  {$if defined(windows)}
    AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + Executable);
    AppCfgPath   := AppDataPath;
    AppCfgFile   := AppCfgPath + 'settings.xml';
  {$elseif defined(darwin)}
    lbuf := New(StringPtr);
    if CFStringGetPascalString(CFLocaleGetIdentifier(CFLocaleCopyCurrent),
      lbuf, Sizeof(lbuf^), 0) then UserLanguage := String(lbuf^);
    Dispose(lbuf);
    AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + '.' + Executable);
    if Identifier = '' then begin
      AppCfgPath := AppDataPath;
    end else begin
      AppCfgPath :=
        IncludeTrailingPathDelimiter(
          IncludeTrailingPathDelimiter(
            IncludeTrailingPathDelimiter(UserHomePath + 'Library') +
            'Application Support') +
          Identifier);
    end;
    AppCfgFile := AppCfgPath + 'settings.xml';
  {$elseif defined(linux) or defined(unix)}
    ltmp := FieldStr(GetEnvironmentVariable('LANG'), 0, '.');
    if ltmp <> '' then UserLanguage := ltmp;
    AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + '.' + Executable);
    AppCfgPath := AppDataPath;
    AppCfgFile := AppCfgPath + 'settings.xml';
  {$else}
    ltmp := FieldStr(GetEnvironmentVariable('LANG'), 0, '.');
    if ltmp <> '' then UserLanguage := ltmp;
    AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + Executable);
    AppCfgPath   := AppDataPath;
    AppCfgFile   := AppCfgPath + 'settings.xml';
  {$ifend}

  if not DirectoryExists(AppCfgPath) then begin
     if not CreateDir(AppCfgPath) then begin
       AppCfgPath := '';
       AppCfgFile := '';
     end;
  end;
  if not DirectoryExists(AppDataPath) then begin
      if not CreateDir(AppDataPath) then begin
        AppDataPath := '';
      end
  end;
end;

function VerifiedPath(Parent, SubDir: String): string;
begin
  Result := '';
  if Parent = '' then exit;
  Parent := IncludeTrailingPathDelimiter(Parent);
  if not DirectoryExists(Parent + SubDir) then
     if not CreateDir(Parent + SubDir) then exit;
  Result := IncludeTrailingPathDelimiter(Parent + SubDir);
end;

function PopDelim(var AStr : String; ADelim: String = SPACE): String;
var
  P : integer;
begin
  P := Pos(ADelim, AStr);
  if P <= 0 then P := Length(AStr) + 1;
  Result := Copy(AStr, 1, P - 1);
  Delete(AStr, 1, P - 1 + Length(ADelim));
end;

function FieldStr(AStr: String; AField: integer; ADelim: String): String;
begin
  Result := '';
  if AField >= 0 then
    repeat
        Result := PopDelim(AStr, ADelim);
        Dec(AField);
    until (AField < 0) or ((Result = '') and (AStr = ''));
end;

function FieldStr(AStr: String; AField, ACount: integer; ADelim: String
  ): String;
begin
  Result := '';
  if (AField >= 0) and (ACount <> 0) then
    repeat
        if AField > 0 then begin
          PopDelim(AStr, ADelim);
          Dec(AField);
        end else begin
          if Result <> '' then Result := Result + ADelim;
          Result := Result + PopDelim(AStr, ADelim);
          if ACount > 0 then Dec(ACount);
        end;
    until (ACount = 0) or  (AStr = '');
end;

function SubStrExcise(var AStr : String; AFrom : String; ATo : String;  MatchCase, Remove : boolean) : String; overload;
var
  S : String;
  P, E, EL : integer;
begin
  if not MatchCase then begin
     AFrom := UpperCase(AFrom);
     ATo := UpperCase(ATo);
     S := UpperCase(AStr);
  end else
      S := AStr;
  P := Pos(AFrom, S);
  if P > 1 then begin
    EL := Length(ATo);
    if EL = 0 then begin
      E := 0;
      EL := 1;
    end else
       E := Pos(ATo, S, P + Length(AFrom));
    if E < 1 then E := Length(S) + 1;
    Result := Copy(AStr, P + Length(AFrom), E - P - Length(AFrom));
    if Remove then
      Delete(AStr, P, E - P + Length(ATo));
  end else
    Result := '';
end;

function SubStr(AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, ATo, MatchCase, False);
end;

function SubStr(AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, '', MatchCase, False);
end;

function Excise(var AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, ATo, MatchCase, True);
end;

function Excise(var AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, '', MatchCase, True);
end;

function Implode(AStr: String; ADelim : String = SPACE) : String; overload;
begin
  Result :=
    StringReplace(
      StringReplace(
        StringReplace(AStr, CRLF, ADelim, [rfReplaceAll]),
      LF, ADelim, [rfReplaceAll]),
    CR, ADelim, [rfReplaceAll]);
end;

procedure Explode(AStr : String; var AStrs : TStringList; ADelim : String;
  ATrim : boolean = false); overload;
var
  S, K : String;
begin
  While Length(AStr) > 0 do begin
    S := PopDelim(AStr, ADelim);
    if ATrim then begin
      S := Trim(S);
      if Pos('=', S) > 0 then begin
        K := Trim(PopDelim(S, '='));
        S := Trim(S);
        AStrs.Add(K + '=' + S);
      end else if Length(S) > 0 then
        AStrs.Add(S);
    end else
      AStrs.Add(S);
  end;
end;

procedure Explode(AStr: String; var AStrs: TStringList; ATrim: boolean = False);
begin
  Explode(AStr, AStrs, SPACE, ATrim);
end;

function Lookup(AStr: String; AStrs: TStringList; MatchCase: boolean): LongInt;
var
  I : LongInt;
begin
  AStr := AStr + '=';
  Result := -1;
  if not MatchCase then begin
    AStr := UpperCase(AStr);
    for I := 0 to AStrs.Count - 1 do begin
      if Uppercase(Copy(AStrs[I], 0, Length(AStr))) = AStr then begin
         Result := I;
         Break;
      end;
    end;
  end else begin
    for I := 0 to AStrs.Count - 1 do begin
      if Copy(AStrs[I], 0, Length(AStr)) = AStr then begin
         Result := I;
         Break;
      end;
    end;
  end;
end;

function LookupValue(AStr: String; AStrs: TStringList; Default: String;
  MatchCase: boolean = false): String;
var
  I : LongInt;
  S : String;
begin
  I := Lookup(AStr, AStrs, MatchCase);
  if I = -1 then
     Result := Default
  else begin
    S := AStrs[I];
    PopDelim(S, '=');
    if Copy(S, 1, 1) = Copy(S, Length(S), 1) then begin
      if Copy(S, 1, 1) = '"' then begin
        Delete(S, 1, 1);
        Delete(S, Length(S), 1);
      end;
    end;
    Result := S;
  end;
end;

function LookupValue(AStr: String; AStrs: TStringList; MatchCase: boolean = false
  ): String;
begin
  Result := LookupValue(AStr, AStrs, '', MatchCase);
end;

function HasTrailing(ASubStr, AStr: String; CaseSpecific: boolean): boolean;
begin
  if CaseSpecific then
    Result := Copy(AStr, Length(AStr) - Length(ASubStr) + 1) = ASubStr
  else
    Result := Uppercase(Copy(AStr, Length(AStr) - Length(ASubStr) + 1)) = Uppercase(ASubStr);
end;

function ExcludeTrailing(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasTrailing(ASubStr, AStr, CaseSpecific) then
    Result := Copy(AStr, 1, Length(AStr) - Length(ASubStr))
  else
    Result := AStr;
end;

function IncludeTrailing(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasTrailing(ASubStr, AStr, CaseSpecific) then
    Result := AStr
  else
    Result := AStr + ASubStr;
end;

function HasLeading(ASubStr, AStr: String; CaseSpecific: boolean): boolean;
begin
  if CaseSpecific then
    Result := Copy(AStr, 1, Length(ASubStr)) = ASubStr
  else
    Result := Uppercase(Copy(AStr, 1, Length(ASubStr))) = Uppercase(ASubStr);
end;

function ExcludeLeading(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasLeading(ASubStr, AStr, CaseSpecific) then
    Result := Copy(AStr, Length(ASubStr) + 1)
  else
    Result := AStr;
end;

function IncludeLeading(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasLeading(ASubStr, AStr, CaseSpecific) then
    Result := AStr
  else
    Result := ASubStr + AStr;
end;

function WhenTrue(AState: boolean; ATrue: String; AFalse: String): String;
begin
  if AState then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AState: boolean; ATrue: integer; AFalse: integer): integer;
begin
  if AState then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AState: boolean; ATrue: pointer; AFalse: pointer): pointer;
begin
  if AState then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AStr: String; ATrue: String; AFalse: String): String;
begin
  if AStr <> '' then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AStr: String; ATrue: integer; AFalse: integer): integer;
begin
  if AStr <> '' then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AStr: String; ATrue: pointer; AFalse: pointer): pointer;
begin
  if AStr <> '' then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AInt: Integer; ATrue: String; AFalse: String): String;
begin
  if AInt <> 0 then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AInt: Integer; ATrue: integer; AFalse: integer): integer;
begin
  if AInt <> 0 then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AInt: Integer; ATrue: pointer; AFalse: pointer): pointer;
begin
  if AInt <> 0 then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(APtr: Pointer; ATrue: String; AFalse: String): String;
begin
  if Assigned(APtr) then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(APtr: Pointer; ATrue: integer; AFalse: integer): integer;
begin
  if Assigned(APtr) then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(APtr: Pointer; ATrue: pointer; AFalse: pointer): pointer;
begin
  if Assigned(APtr) then
    Result := ATrue
  else
    Result := AFalse;
end;

function ZeroPad(AValue : LongInt; AWidth: integer): String;
begin
  Result := LeftPad(IntToStr(AValue), AWidth, '0');
end;

function LeftPad(AStr: String; AWidth: integer; ASubStr: String): String;
begin
  if ASubStr = '' then ASubStr := SPACE;
  While (Length(AStr) < AWidth) do begin
    AStr := ASubStr + AStr;
  end;
  Result := AStr;
end;

function RightPad(AStr: String; AWidth: integer; ASubStr: String): String;
begin
  if ASubStr = '' then ASubStr := SPACE;
  While (Length(AStr) < AWidth) do begin
    AStr := AStr + ASubStr;
  end;
  Result := AStr;
end;

function CenterPad(AStr: String; AWidth: integer; ASubStr: String): String;
begin
  if ASubStr = '' then ASubStr := SPACE;
  While (Length(AStr) < AWidth) do begin
    AStr := AStr + ASubStr;
    if (Length(AStr) < AWidth) then
      AStr := ASubStr + AStr;
  end;
  Result := AStr;
end;



function SimpleCheckSum(const AStr: String): word;
var
  Sum: word;
  I : integer;
begin
  Sum:= 0;
  for I := 1 to Length(AStr) do
    Sum:=word((Sum shr 1) or ((Sum and 1) shl 15)) + Ord(AStr[I]);
  Result:=Sum;
end;

function ForEachFile(AProc: TForEachFileFunc; APath : String; ARecurse : boolean) : integer;
var
  PathFlag : boolean;
  AnyFile : boolean;

  function ForEachFiles(const ASubPath : String) : integer;
  var
    R : integer;
    Search : TSearchRec;
  begin
    if ARecurse then begin
      R := FindFirst(IncludeTrailingPathDelimiter(APath + ASubPath) + '*', faAnyFile, Search);
      while (R = 0) do begin
        if (Search.Attr and faDirectory = faDirectory) then begin
          if (Search.Name <> '.') and (Search.Name <> '..') then
            R := ForEachFiles(ASubPath + WhenTrue(ASubPath <> '', DirectorySeparator) + Search.Name);
        end;
        if R = 0 then
          R := FindNext(Search);
      end;
      FindClose(Search);
    end;

    if (R = 0) or (R = -1) then begin
      R := FindFirst(IncludeTrailingPathDelimiter(APath + ASubPath) + '*', faAnyFile, Search);
      while (R = 0) do begin
        if (Search.Attr and faDirectory <> faDirectory) then begin
           AnyFile := True;
           R := AProc(WhenTrue(PathFlag, '', DirectorySeparator) +
             ASubPath + WhenTrue(ASubPath <> '', DirectorySeparator) + Search.Name);
        end;
        if R = 0 then
          R := FindNext(Search);
      end;
      FindClose(Search);
    end;

    if (R = -1) then R := 0;
    Result := R;
  end;

begin
  AnyFile := True;
  PathFlag := APath = IncludeTrailingPathDelimiter(APath);
  if not PathFlag then
    APath := IncludeTrailingPathDelimiter(APath);
  Result := ForEachFiles('');
  if (Result = 0) and (Not AnyFile) then
    Result := -1;
end;

function FileList(APathSpec : String) : TFileList;
var
  R, C : integer;
  Search : TSearchRec;
begin
  C := 0;
  SetLength(Result, 0);
  R := FindFirst(APathSpec, faAnyFile, Search);
  while (R = 0) do begin
    if (Search.Attr and faDirectory <> faDirectory) then begin
      if C = Length(Result) then
        SetLength(Result, Length(Result) + 16);
      Result[C] := Search.Name;
      Inc(C);
    end;
    if R = 0 then
      R := FindNext(Search);
  end;
  FindClose(Search);
  SetLength(Result, C);
end;

initialization
  UpdateServer := 'https://up.lod.bz/';
  InitPasExt('');
end.
