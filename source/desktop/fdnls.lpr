program fdnls;
{$WARN 5028 off : Local $1 "$2" is not used}

{$I defines.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, PasExt, uMain, uLog, uAppCfg, uAppNLS, uPickFlag, uEditCodePage, uPkgListEdit,
  uPkgDetails, uPkgPreview;

{$R *.res}

{$I version.inc}

begin
  // InitPasExt(APP_IDENTIFIER);
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Title:='FD-NLS';
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfLog, fLog);
  Application.CreateForm(TfPickFlag, fPickFlag);
  Application.CreateForm(TfEditCodePage, fEditCodePage);
  Application.Run;
end.

