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
  Classes, SysUtils, CustApp
  { you can add units after this };

{$I version.inc}

type

  { TNLSReport }

  TNLSReport = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
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

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TNLSReport.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TNLSReport.Destroy;
begin
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

