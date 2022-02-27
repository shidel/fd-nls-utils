unit PasExt;

{$warn 5023 off : no warning about unused units}
interface

var
  UserHomePath  : String;      { User's Home directory }
  AppDataPath   : String;      { Location for program data files }
  AppCfgPath    : String;      { Location of application config file }
  AppCfgFile    : String;      { Application Config File }

procedure InitPasExt(Identifier : String);

function VerifiedPath (Parent, SubDir : String) : string;

implementation

uses
  Classes, SysUtils;

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

initialization
  InitPasExt('');
end.
