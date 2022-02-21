unit uSettings;

{$I defines.inc}

interface

uses
  Classes, SysUtils;

{$I version.inc}

var
  SettingsPath : string;

implementation

initialization
    {$ifdef MacOS}
        SettingsPath := GetEnvironmentVariable('HOME') + '/Library/Application Support/' + APP_Identifier;
    {$else}
           Compile Error!
    {$endif}
end.

