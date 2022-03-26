unit uAppCfg;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, ClassExt, PUIExt, XMLConf, Controls, { StdCtrls, }
  ExtCtrls, Forms;

var
  Settings,
  Properties : TXMLConfig;

function GetPropertyState(Control : TControl; KeyName : String; Value : integer) : integer; overload;
procedure SetPropertyState(Control : TControl; KeyName : String; Value : integer); overload;

function GetPropertyState(PathName : String; KeyName : String; Value : integer) : integer; overload;
procedure SetPropertyState(PathName : String; KeyName : String; Value : integer); overload;

procedure GetPropertyState(Form : TForm); overload;
procedure SetPropertyState(Form : TForm); overload;

function GetSetting(Key : String; Default : boolean = false) : boolean; overload;
function GetSetting(Key : String; Default : integer = 0) : Integer; overload;
function GetSetting(Key : String; Default : String = '') : String; overload;
procedure SetSetting(Key : String; Value : boolean); overload;
procedure SetSetting(Key : String; Value : integer); overload;
procedure SetSetting(Key : String; Value : String); overload;

implementation

function GetPropertyState(Control : TControl; KeyName : String; Value : integer) : integer; overload;
var
  DNP : String;
begin
  DNP := DisplayNamePath(Control);
  Result := GetValueXML(Properties, DNP + KeyName, Value);
end;

procedure SetPropertyState(Control: TControl; KeyName: String; Value: integer);
var
  DNP : String;
begin
  DNP := DisplayNamePath(Control);
  SetValueXML(Properties, DNP + KeyName, Value);
end;

function GetPropertyState(PathName: String; KeyName: String; Value: integer
  ): integer;
var
  DNP : String;
begin
  DNP := DisplayNamePath(PathName);
  Result := GetValueXML(Properties, DNP + KeyName, Value);
end;

procedure SetPropertyState(PathName: String; KeyName: String; Value: integer);
var
  DNP : String;
begin
  DNP := DisplayNamePath(PathName);
  SetValueXML(Properties, DNP + KeyName, Value);
end;

procedure GetPropertyState(Form: TForm);
var
  DNP : String;
begin
  DNP := DisplayNamePath(Form);
  Form.Top := GetValueXML(Properties, DNP + 'TOP', Form.Top);
  Form.Left := GetValueXML(Properties, DNP + 'LEFT', Form.Left);
  Form.Width := GetValueXML(Properties, DNP + 'WIDTH', Form.Width);
  Form.Height := GetValueXML(Properties, DNP + 'HEIGHT', Form.Height);
  Form.WindowState := GetValueXML(Properties, DNP + 'STATE', Form.WindowState);
  Form.Position := GetValueXML(Properties, DNP + 'POSITION', Form.Position);
end;

procedure SetPropertyState(Form: TForm);
var
  DNP : String;
begin
  DNP := DisplayNamePath(Form);
  SetValueXML(Properties, DNP + 'POSITION', Form.Position);
  SetValueXML(Properties, DNP + 'STATE', Form.WindowState);
  SetValueXML(Properties, DNP + 'TOP', Form.Top);
  SetValueXML(Properties, DNP + 'LEFT', Form.Left);
  SetValueXML(Properties, DNP + 'WIDTH', Form.Width);
  SetValueXML(Properties, DNP + 'HEIGHT', Form.Height);
end;

function GetSetting(Key: String; Default: boolean): boolean;
begin
  Result := GetValueXML(Settings, Key, Default);
end;

function GetSetting(Key: String; Default: integer): Integer;
begin
  Result := GetValueXML(Settings, Key, Default);

end;

function GetSetting(Key: String; Default: String): String;
begin
  Result := GetValueXML(Settings, Key, Default);
end;

procedure SetSetting(Key: String; Value: boolean);
begin
  SetValueXML(Settings, Key, Value);
end;

procedure SetSetting(Key: String; Value: integer);
begin
  SetValueXML(Settings, Key, Value);
end;

procedure SetSetting(Key: String; Value: String);
begin
  SetValueXML(Settings, Key, Value);
end;


initialization

  Settings := TXMLConfig.Create(nil);
  Properties := TXMLConfig.Create(nil);

  Settings.Filename:= AppCfgPath + 'userdata.xml';
  Properties.Filename:= AppCfgPath + 'display.xml';

finalization

if Assigned(Properties) then begin
  Properties.Flush;
  FreeAndNil(Properties);
end;

if Assigned(Settings) then begin
  Settings.Flush;
  FreeAndNil(Settings);
end;

end.

