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
  PProject = ^TProject;
  TProject = record
    Parent : PProject;
    Name : String;
    Kind : integer;
    Level : integer;
    Subs : array of PProject;
  end;

  { TNLSReport }

  TNLSReport = class(TCustomApplication)
  private
  protected
      CfgFile  : TIniFile;
      Template : string;
      Verbose  : integer;
      Projects : PProject;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure SettingsDefault; virtual;
    procedure SettingsLoad; virtual;
    procedure SettingsSave; virtual;
    procedure ProcessMain; virtual;
    function ProcessSub(AParent : PProject; ASubPath : String) : PProject; virtual;
    function ProjectPath(AProject : PProject) : String; virtual;
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
begin
  Projects := ProcessSub(nil, '');
end;

function TNLSReport.ProcessSub(AParent: PProject; ASubPath: String): PProject;
var
  Project : PProject;
  Files : TSearchRec;
  Res, I : LongInt;
  Dirs : TStringList;
  X : integer;
  SubP : String;
begin
  Project := New(PProject);
  Result := Project;
  with Project^ do begin
    Parent := AParent;
    Name := ASubPath;
    Kind := 0;
    SetLength(Subs, 0);
  end;
  WriteLn(ProjectPath(Project) + '*');
  Dirs := TStringList.Create;
  Dirs.Sorted:=True;
  Res := FindFirst(ProjectPath(Project) + '*', faAnyFile, Files);
  while Res = 0 do begin
      if (Files.Attr and faDirectory = faDirectory)
      and (Files.Name[1] <> '.') then
        Dirs.Add(Files.Name);
    Res := FindNext(Files);
  end;
  FindClose(Files);
  for I := 0 to Dirs.Count - 1 do begin
      SubP := IncludeTrailingPathDelimiter(ProjectPath(Project) + Dirs[I]);
      if CfgFile.ValueExists(SubP, 'Type') then
        X := IndexStr(Trim(CfgFile.ReadString(SubP, 'Type', '')), ProjectTypes)
      else
        X := 0;
      if (X < 0) then begin
        X := IndexText(Trim(CfgFile.ReadString(SubP, 'Type', '')), ProjectTypes);
        if X < 0 then X := 0;
        CfgFile.WriteString(SubP, 'Type', ProjectTypes[X]);
      end;
      if X = 1 then Continue;
      SetLength(Project^.Subs, Length(Project^.Subs) + 1);
      if (not Assigned(AParent)) or (not Assigned(AParent^.Parent)) then
            Project^.Subs[High(Project^.Subs)] := ProcessSub(Project, Dirs[I]);
  end;
  Dirs.Free;

end;

function TNLSReport.ProjectPath(AProject: PProject): String;
var
  S : String;
begin
  S := '';
  while Assigned(AProject) do begin
    if AProject^.Name <> '' then
      S := IncludeTrailingPathDelimiter(AProject^.Name) + S;
    AProject := AProject^.Parent;
  end;
  Result := S;
end;

{

procedure TNLSReport.ProcessSub(APath : String; var Project: TProject);
begin
  Inc(ScanLevel);
  Dec(ScanLevel);
end;
}

var
  Application: TNLSReport;

{$R *.res}

begin
  Application:=TNLSReport.Create(nil);
  Application.Title:=APP_PRODUCTNAME;
  Application.Run;
  Application.Free;
end.

