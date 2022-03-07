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
  TFDNLS = class;

  { TFDLanguages }

  TFDLanguages = class(TPersistent)
  private
    FCount: integer;
    FOwner: TFDNLS;
    function GetCaptions(Index : integer): String;
    procedure SetCaptions(Index : integer; AValue: String);
    procedure SetCount(AValue: integer);
    procedure SetOwner(AValue: TFDNLS);
  protected
    property Owner : TFDNLS read FOwner write SetOwner;
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    property Count : integer read FCount write SetCount;
    property Captions[Index : integer] : String read GetCaptions write SetCaptions;
  published
  end;

  TFDNLS = class(TPersistent)
  private
    FLanguages: TFDLanguages;
    function GetDataPath: string;
    function GetLanguagesPath: string;
    function GetProjectsPath: string;
    function GetPath: string;
    procedure SetLanguages(AValue: TFDLanguages);
    procedure SetPath(AValue: string);
  protected
    property DataPath : string read GetDataPath;
    property LanguagesPath : string read GetLanguagesPath;
    property ProjectsPath : string read GetProjectsPath;
  public
    constructor Create;
    destructor Destroy; override;
    property Path : string read GetPath write SetPath;
    property Languages : TFDLanguages read FLanguages write SetLanguages;
  published
  end;


implementation

const
   RepositoryPath : String = '';

{ TFDLanguages }

function TFDLanguages.GetCaptions(Index : integer): String;
begin
  Result := 'Language ' + IntToStr(Index);
end;

procedure TFDLanguages.SetCaptions(Index : integer; AValue: String);
begin

end;

procedure TFDLanguages.SetCount(AValue: integer);
begin
  if FCount=AValue then Exit;
  FCount:=AValue;
end;

procedure TFDLanguages.SetOwner(AValue: TFDNLS);
begin
  if FOwner=AValue then Exit;
  FOwner:=AValue;
end;

constructor TFDLanguages.Create(AOwner : TFDNLS);
begin
  inherited Create;
  FOwner := AOwner;
  FCount := 5;
end;

destructor TFDLanguages.Destroy;
begin
  inherited Destroy;
end;

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

procedure TFDNLS.SetLanguages(AValue: TFDLanguages);
begin
  if FLanguages=AValue then Exit;
  FLanguages:=AValue;
end;

procedure TFDNLS.SetPath(AValue: string);
begin
  if AValue <> '' then
     AValue := IncludeTrailingPathDelimiter(AValue);
  if RepositoryPath=AValue then Exit;
  RepositoryPath:=AValue;
end;

constructor TFDNLS.Create;
begin
  inherited Create;
  FLanguages := TFDLanguages.Create(Self);
end;

destructor TFDNLS.Destroy;
begin
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

end.
