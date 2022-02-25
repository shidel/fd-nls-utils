unit AppConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, XMLConf, Forms;

type

  { TAppConfig }

  TAppConfig = class(TComponent)
  private
    function GetSettings: TXMLConfig;
    procedure SetSettings(AValue: TXMLConfig);
  protected
  public
    property Settings : TXMLConfig read GetSettings write SetSettings;
  published
  end;

var
  UserHomePath : String;      { User's Home directory }
  AppXMLPath   : String;      { Location of application config file }
  AppDataPath  : String;      { Location for program data files }
  AppXMLConfig : TXMLConfig;  { program settings }

procedure InitAppConfig(Identifier : String);

procedure SaveState(const Form : TForm); overload;

procedure Register;

implementation

procedure SaveState(const Form : TForm); overload;
begin

end;

procedure Register;
begin
  {$I appconfig_icon.lrs}
  RegisterComponents('AppKit',[TAppConfig]);
end;

{ TAppConfig }

function TAppConfig.GetSettings: TXMLConfig;
begin
  Result := AppXMLConfig;
end;

procedure TAppConfig.SetSettings(AValue: TXMLConfig);
begin
  if AppXMLConfig=AValue then Exit;
  AppXMLConfig:=AValue;
end;

procedure InitAppConfig(Identifier : String);
var
  S : String;
begin
  {$if defined(windows)}
    UserHomePath := '';
    AppXMLPath := ExtractFilePath(ExpandFileName(Paramstr(0)));
    AppDataPath := AppXMLPath;
  {$elseif defined(darwin)}
    UserHomePath := IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('HOME'));
    AppXMLPath := LowerCase(ExtractFileName(ParamStr(0)));
    SetLength(AppXMLPath, Length(AppXMLPath) - Length(ExtractFileExt(ParamStr(0))));
    AppDataPath := IncludeTrailingpathDelimiter(UserHomePath + '.' + AppXMLPath);
    AppXMLPath :=
      IncludeTrailingPathDelimiter(
        IncludeTrailingPathDelimiter(
          IncludeTrailingPathDelimiter(UserHomePath + 'Library') +
          'Application Support') +
        Identifier);
  {$elseif defined(linux) or defined(unix)}
    UserHomePath := IncludeTrailingpathDelimiter(SysUtils.GetEnvironmentVariable('HOME'));
    AppXMLPath := LowerCase(ExtractFileName(ParamStr(0)));
    SetLength(AppXMLPath, Length(AppXMLPath) - Length(ExtractFileExt(ParamStr(0))));
    AppXMLPath := IncludeTrailingpathDelimiter(UserHomePath + '.' + AppXMLPath);
    AppDataPath := AppXMLPath;
  {$else}
    UserHomePath := '';
    AppXMLPath := ExtractFilePath(ExpandFileName(Paramstr(0)));
    AppDataPath := AppXMLPath;
  {$ifend}

  try
     AppXMLConfig := TXMLConfig.Create(nil);
     if not DirectoryExists(AppXMLPath) then begin
        if not CreateDir(AppXMLPath) then begin
          FreeAndNil(AppXMLConfig);
          AppXMLPath := '';
        end
     end;
     if Assigned(AppXMLConfig) then begin
       AppXMLConfig.Filename:= AppXMLPath + 'settings.xml';
       AppXMLConfig.OpenKey('general/storage');
       S := AppXMLConfig.GetValue('path', '');
       if S = '' then
         AppXMLConfig.SetValue('path', AppDataPath)
       else
         AppDataPath := S;
       AppXMLConfig.CloseKey;
     end;
     if not DirectoryExists(AppDataPath) then begin
         if not CreateDir(AppDataPath) then begin
           AppDataPath := '';
         end
     end;
  except
     AppXMLConfig := nil;
  end;
end;

initialization

  AppXMLConfig := nil;

finalization

  if Assigned(AppXMLConfig) then begin
     AppXMLConfig.Flush;
     AppXMLConfig.CloseKey;
     FreeAndNil(AppXMLConfig);
  end;

end.
