program fdnls;

{$I defines.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, PasExt, uMain, uLog, uAppNLS, uPickFlag;

{$R *.res}

{$I version.inc}

begin
  // InitPasExt(APP_IDENTIFIER);
  RequireDerivedFormResource:=True;
  Application.Title:='FD-NLS';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TmForm, mForm);
  Application.CreateForm(TfLog, fLog);
  Application.CreateForm(TfPickFlag, fPickFlag);
  Application.Run;
end.

