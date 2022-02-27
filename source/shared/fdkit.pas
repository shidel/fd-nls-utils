{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FDKit;

{$warn 5023 off : no warning about unused units}
interface

uses
  Classes, SysUtils, PasExt;

type

  { TFDNLS }

  TFDNLS = class(TComponent)
  private
    function GetDataPath: string;
    function GetLanguagesPath: string;
    function GetProjectsPath: string;
    function GetPath: string;
    procedure SetPath(AValue: string);
  protected
  public
    property Path : string read GetPath write SetPath;
    property DataPath : string read GetDataPath;
    property LanguagesPath : string read GetLanguagesPath;
    property ProjectsPath : string read GetProjectsPath;
  published
  end;


implementation

const
   RepositoryPath : String = '';

{ TFDNLS }

function TFDNLS.GetDataPath: string;
begin
  Result := VerifiedPath(RepositoryPath, 'fd-nls');
end;

function TFDNLS.GetLanguagesPath: string;
begin
  Result := VerifiedPath(DataPath, 'languages');
end;

function TFDNLS.GetProjectsPath: string;
begin
  Result := VerifiedPath(DataPath, 'projects');
end;

function TFDNLS.GetPath: string;
begin
  Result := RepositoryPath;
end;

procedure TFDNLS.SetPath(AValue: string);
begin
  if AValue <> '' then
     AValue := IncludeTrailingPathDelimiter(AValue);
  if RepositoryPath=AValue then Exit;
  RepositoryPath:=AValue;
end;

end.
