unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls, XMLConf;

type

  { TfMain }

  TfMain = class(TForm)
    Label1: TLabel;
    XMLCfg: TXMLConfig;
    XMLProps: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
var
   Displays : String;
   I : integer;
   T : TMonitor;
begin
   // Create a unique ID for monitor count and resolutions
   Displays := IntToHex(Screen.MonitorCount, 2) +
     IntToHex(Screen.PrimaryMonitor.MonitorNum, 2) +
     IntToHex(Screen.PrimaryMonitor.PixelsPerInch, 4);
   for I := 0 to Screen.MonitorCount - 1 do
     Displays := Displays +
     IntToHex(Screen.Monitors[I].Width,4) +
     IntToHex(Screen.Monitors[I].Height,4);
   // Assign the config application files
   XMLProps.FileName := AppCfgFile;
   XMLProps.RootNodePath := 'DISPLAYS/UID_' + Displays + '/STATE';
   XMLCfg.Filename:=AppCfgPath + 'userdata.xml';
end;

end.

