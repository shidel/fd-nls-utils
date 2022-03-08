unit VCSExt;

{$warn 5023 off : no warning about unused units}
interface

uses
  Classes, SysUtils;

function VCSAddFile(AFileName : String) : boolean;
function VCSDeleteFile(AFileName : String) : boolean;

implementation

procedure InitVCSExt;
begin
end;

function VCSAddFile(AFileName : String) : boolean;
begin
	Result := True;
end;

function VCSDeleteFile(AFileName : String) : boolean;
begin
	Result := True;
end;

initialization
  InitVCSExt;
end.
