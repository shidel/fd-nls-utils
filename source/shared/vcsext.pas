unit VCSExt;

{$warn 5023 off : no warning about unused units}
interface

uses
  Classes, SysUtils;

function VCSAdd(AFileName : String) : boolean;
function VCSDelete(AFileName : String) : boolean;

implementation

procedure InitVCSExt;
begin
end;

function VCSAdd(AFileName : String) : boolean;
begin
	Result := True;
end;

function VCSDelete(AFileName : String) : boolean;
begin
	Result := True;
end;

initialization
  InitVCSExt;
end.
