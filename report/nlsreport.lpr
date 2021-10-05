{
    BSD 3-Clause License
    Copyright (c) 2021, Jerome Shidel
    All rights reserved.
}

program nlsreport;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, IniFiles, StrUtils;

{$I version.inc}
const
  CfgExt = '.CFG';
  VerboseOpts : array of string = ('Quite', 'Normal', 'Verbose');
  {  NoYesOpts : array of string = ('No', 'Yes', 'True'); }
  ProjectTypes : array of string = ('Detect', 'Exclude', 'Standard');

type
  TProject = record
    Name : String;
    Kind : integer;
  end;
  TProjects = array of TProject;

  { TNLSReport }

  TNLSReport = class(TCustomApplication)
  private
  protected
      CfgFile  : TIniFile;
      Template : string;
      Verbose  : integer;
      Projects : TProjects;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure SettingsDefault; virtual;
    procedure SettingsLoad; virtual;
    procedure SettingsSave; virtual;
    procedure ProcessMain; virtual;
    procedure ProcessProject; virtual;
    procedure ProcessNLSFiles(APath : String); virtual;
    procedure ProcessHelpFiles(APath : String); virtual;
  end;

{ TNLSReport }

procedure TNLSReport.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  ProcessMain;

  // stop program loop
  Terminate;
end;

constructor TNLSReport.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  SetLength(Projects, 0);
  CfgFile := TIniFile.Create(LowerCase(ChangeFileExt(ExeName, CfgExt)));
  SettingsDefault;
  SettingsLoad;

end;

destructor TNLSReport.Destroy;
begin
  { SettingsSave; }
  CfgFile.Free;
  inherited Destroy;
end;

procedure TNLSReport.WriteHelp;
begin
  WriteLn(APP_PRODUCTNAME, ' version ', APP_VERSION);
  WriteLn;
  writeLn('Usage: ', ExtractFileName(ExeName), ' [options]');
  WriteLn;
  WriteLn(#9,'-h, --help',    #9, 'Display this help text');
  WriteLn;
{
  WriteLn(#9,'-v, --verbose', #9, 'Display more information');
  WriteLn(#9,'-q, --quite',   #9, 'Display less information');
  WriteLn;
}
end;

procedure TNLSReport.SettingsDefault;
begin
  Template:='report.template';
end;

procedure TNLSReport.SettingsLoad;
begin
  Template := CfgFile.ReadString('*', 'Template', Template);
  Verbose := IndexText(Trim(CfgFile.ReadString('*', 'Verbose', '')), VerboseOpts) - 1;
  if Verbose < -1 then Verbose := 0;
end;

procedure TNLSReport.SettingsSave;
begin
  CfgFile.WriteString('*', 'Template', Template);
  CfgFile.WriteString('*', 'Verbose', VerboseOpts[Verbose + 1]);
end;

procedure TNLSReport.ProcessMain;
var
  Files : TSearchRec;
  Res, I : LongInt;
  Dirs : TStringList;
  X : integer;
begin
  Dirs := TStringList.Create;
  Dirs.Sorted:=True;
  Res := FindFirst('*', faAnyFile, Files);
  while Res = 0 do begin
      if (Files.Attr and faDirectory = faDirectory)
      and (Files.Name[1] <> '.') then
        Dirs.Add(Files.Name);
    Res := FindNext(Files);
  end;
  FindClose(Files);
  for I := 0 to Dirs.Count - 1 do begin
      if CfgFile.ValueExists(Dirs.Strings[I], 'Type') then
        X := IndexStr(Trim(CfgFile.ReadString(Dirs[I], 'Type', '')), ProjectTypes)
      else
        X := 0;
      if (X < 0) then begin
        X := IndexText(Trim(CfgFile.ReadString(Dirs[I], 'Type', '')), ProjectTypes);
        if X < 0 then X := 0;
        CfgFile.WriteString(Dirs.Strings[I], 'Type', ProjectTypes[X]);
      end;
      if X = 1 then Continue;
      SetLength(Projects, Length(Projects) + 1);
      Projects[High(Projects)].Name := Dirs[I];
      Projects[High(Projects)].Kind := X;
      case X of
         0,2 : ProcessProject;
      end;
  end;
  Dirs.Free;
end;

procedure TNLSReport.ProcessProject;
var
  Files : TSearchRec;
  Res, I : LongInt;
  Dirs : TStringList;
  CP : String;
begin
  CP := IncludeTrailingPathDelimiter(Projects[High(Projects)].Name);
  if Verbose > 0 then
     WriteLn('Project: ', Projects[High(Projects)].Name);
  Dirs := TStringList.Create;
  Dirs.Sorted:=True;
  Res := FindFirst(CP + '*', faAnyFile, Files);
  while Res = 0 do begin
      if (Files.Attr and faDirectory = faDirectory)
      and (Files.Name[1] <> '.') then
        Dirs.Add(Files.Name);
    Res := FindNext(Files);
  end;
  FindClose(Files);
  for I := 0 to Dirs.Count - 1 do begin
      case Uppercase(Dirs[I]) of
         'NLS' : ProcessNLSFiles(CP + Dirs[I]);
         'HELP' : ProcessHelpFiles(CP + Dirs[I]);
      end;
  end;
  Dirs.Free;

end;

procedure TNLSReport.ProcessNLSFiles(APath : String);
begin
     WriteLn(APath);
end;

procedure TNLSReport.ProcessHelpFiles(APath : String);
begin
  WriteLn(APath);
end;

var
  Application: TNLSReport;

{$R *.res}

begin
  Application:=TNLSReport.Create(nil);
  Application.Title:=APP_PRODUCTNAME;
  Application.Run;
  Application.Free;
end.

