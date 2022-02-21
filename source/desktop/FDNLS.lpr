program FDNLS;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uSettings
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='FD-NLS';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

