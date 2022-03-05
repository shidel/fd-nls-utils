unit PasExt;

{$warn 5023 off : no warning about unused units}
interface

uses
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

var
  UserHomePath  : String;      { User's Home directory }
  AppDataPath   : String;      { Location for program data files }
  AppCfgPath    : String;      { Location of application config file }
  AppCfgFile    : String;      { Application Config File }
  UpdateServer  : String;      { Application Update Server URL }

type
  TUpdateInterval = (auiNever, auiMonthy, auiWeekly, auiDaily, auiFrequent);

procedure InitPasExt(Identifier : String);

function VerifiedPath (Parent, SubDir : String) : string;

function PopDelim(var AStr : String; ADelim: String = SPACE): String; overload;

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

implementation

procedure InitPasExt(Identifier : String);
var
  Executable : String;
begin
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
  AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + '.' + Executable);
  AppCfgPath := AppDataPath;
  AppCfgFile := AppCfgPath + 'settings.xml';
  {$else}
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


initialization
  UpdateServer := 'https://up.lod.bz/';
  InitPasExt('');
end.
