program FDNLS;

{$I defines.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, AppConfig, uMain, uSettings;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  InitAppConfig(APP_IDENTIFIER);
  Application.Title:='FD-NLS';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

