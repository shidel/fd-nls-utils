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
  NoYesOpts : array of string = ('No', 'Yes', 'True');

type
  { TNLSReport }

  TNLSReport = class(TCustomApplication)
  protected
      CfgFile  : TIniFile;
      Template : string;
      Verbose  : integer;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure SettingsDefault; virtual;
    procedure SettingsLoad; virtual;
    procedure SettingsSave; virtual;
    procedure ProcessMain; virtual;
    procedure ProcessProject( Project : String ); virtual;
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
  WriteLn(#9,'-v, --verbose', #9, 'Display more information');
  WriteLn(#9,'-q, --quite',   #9, 'Display less information');
  WriteLn;
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
  S : String;
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
      if CfgFile.ValueExists(Dirs.Strings[I], 'Exclude') then
        X := IndexStr(Trim(CfgFile.ReadString(Dirs.Strings[I], 'Exclude', '')), NoYesOpts)
      else
        X := 0;
      if (X < 0) or (X > 1) then begin
        X := IndexText(Trim(CfgFile.ReadString(Dirs.Strings[I], 'Exclude', '')), NoYesOpts);
        if X > 0 then
          CfgFile.WriteString(Dirs.Strings[I], 'Exclude', 'Yes')
        else
          CfgFile.WriteString(Dirs.Strings[I], 'Exclude', 'No');
      end;
      if X > 0 then Continue;
      ProcessProject(Dirs.Strings[I]);
  end;
  Dirs.Free;
end;

procedure TNLSReport.ProcessProject(Project: String);
begin
  if Verbose >= 0 then
     WriteLn('Project: ', Project);
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

