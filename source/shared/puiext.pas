unit PUIExt;

{$warn 5023 off : no warning about unused units}
interface

uses
  Classes, SysUtils, Graphics, Controls, Forms;

const
  clErrorText : TColor = clRed;

function DisplayUID : string;
function DisplayNamePath(AComponent : TComponent) : String;

implementation

function DisplayUID : string;
var
   Displays : String;
   I : integer;
begin
     // Create a unique ID for monitor count and resolutions
   Displays := IntToHex(Screen.MonitorCount, 2) +
     IntToHex(Screen.PrimaryMonitor.MonitorNum, 2) +
     IntToHex(Screen.PrimaryMonitor.PixelsPerInch, 4);
   for I := 0 to Screen.MonitorCount - 1 do
     Displays := Displays +
     IntToHex(Screen.Monitors[I].Width,4) +
     IntToHex(Screen.Monitors[I].Height,4);
   Result := Displays;
end;

function DisplayNamePath(AComponent : TComponent): String;
begin
  Result := 'DISPLAYS/UID_' + DisplayUID + '/' + AComponent.GetNamePath + '/STATE';
end;

initialization
end.
