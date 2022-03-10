{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FDKit;

{$warn 5023 off : no warning about unused units}
interface

uses
  Classes, SysUtils, PasExt, XMLConf, VCSExt;

type
  TLangDataItem = record
    Caption : String;
    Identifier : String;
    Lang : String;
    CodePage : integer;
    Graphic : String;
  end;

  { TFDNLS }
  TFDNLS = class;

  { TFDLanguages }

  TFDLanguages = class(TPersistent)
  private
    FXML : TXMLConfig;
    FFiles : TStringList;
    FData : array of TLangDataItem;
    FOwner: TFDNLS;
    function GetCaption(Index : integer): String;
    function GetCodePage(Index : integer): integer;
    function GetCount: integer;
    function GetFileName(Index : integer): String;
    function GetGraphic(Index : integer): String;
    function GetIdentifier(Index : integer): String;
    function GetLang(Index : integer): String;
    procedure SetCaption(Index : integer; AValue: String);
    procedure SetCodePage(Index : integer; AValue: integer);
    procedure SetFileName(Index : integer; AValue: String);
    procedure SetGraphic(Index : integer; AValue: String);
    procedure SetIdentifier(Index : integer; AValue: String);
    procedure SetLang(Index : integer; AValue: String);
    procedure SetOwner(AValue: TFDNLS);
  protected
    property Owner : TFDNLS read FOwner write SetOwner;
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    procedure Refresh;
    property Count : integer read GetCount;
    property Filename[Index : integer] : String read GetFileName write SetFileName;
    property Caption[Index : integer] : String read GetCaption write SetCaption;
    property Identifier[Index : integer] : String read GetIdentifier write SetIdentifier;
    property Lang[Index : integer] : String read GetLang write SetLang;
    property CodePage[Index : integer] : integer read GetCodePage write SetCodePage;
    property Graphic[Index : integer] : String read GetGraphic write SetGraphic;
    function NewLanguage : integer;
    procedure Delete(Index : integer);
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
    procedure Refresh;
  published
  end;


implementation

const
   RepositoryPath : String = '';

{ TFDLanguages }

function TFDLanguages.GetCaption(Index : integer): String;
begin
  Result := FData[Index].Caption;
end;

function TFDLanguages.GetCodePage(Index : integer): integer;
begin
  Result := FData[Index].Codepage;
end;

function TFDLanguages.GetCount: integer;
begin
  Result := FFiles.Count;
end;

function TFDLanguages.GetFileName(Index : integer): String;
begin
  Result := FFiles[Index];
end;

function TFDLanguages.GetGraphic(Index : integer): String;
begin
  Result := FData[Index].Graphic;
end;

function TFDLanguages.GetIdentifier(Index : integer): String;
begin
  Result := FData[Index].Identifier;
end;

function TFDLanguages.GetLang(Index : integer): String;
begin
  Result := FData[Index].Lang;
end;

procedure TFDLanguages.SetCaption(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Trim(AValue);
  if AValue = FData[Index].Caption then exit;
  T := Uppercase(AValue);
  for I := 0 to Length(FData) - 1 do
    if (I <> Index) and (T = Uppercase(FData[I].Caption)) then exit;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/CAPTION', AValue);
  FXML.Flush;
  FData[Index].Caption := AValue;
end;

procedure TFDLanguages.SetCodePage(Index : integer; AValue: integer);
begin
  if AValue = FData[Index].CodePage then exit;
  // OK to have a duplicate codepage
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/CODEPAGE', AValue);
  FXML.Flush;
  FData[Index].Codepage := AValue;
end;

procedure TFDLanguages.SetFileName(Index : integer; AValue: String);
begin
  AValue := Trim(Lowercase(AValue));
  if FileExists(FOwner.LanguagesPath + AValue) then Exit;
  if FileExists(FOwner.LanguagesPath + FFiles[Index]) then begin
    if not RenameFile(FOwner.LanguagesPath + FFiles[Index], FOwner.LanguagesPath + AValue) then exit;
  end;
  FFiles[Index] := AValue;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
end;

procedure TFDLanguages.SetGraphic(Index : integer; AValue: String);
begin
  AValue := Trim(LowerCase(AValue));
  if AValue = FData[Index].Graphic then exit;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/GRAPHIC', AValue);
  FXML.Flush;
  FData[Index].Graphic := AValue;
end;

procedure TFDLanguages.SetIdentifier(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Trim(AValue);
  if AValue = FData[Index].Identifier then exit;
  if AValue <> '' then begin
    T := Uppercase(AValue);
    for I := 0 to Length(FData) - 1 do
        if (I <> Index) and (T = Uppercase(FData[I].Identifier)) then exit;
  end;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/IDENTIFIER', AValue);
  FXML.Flush;
  FData[Index].Identifier := AValue;
end;

procedure TFDLanguages.SetLang(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Uppercase(Trim(AValue));
  if AValue = FData[Index].Lang then exit;
  if AValue <> '' then begin
    for I := 0 to Length(FData) - 1 do
        if AValue = FData[I].Lang then exit;
  end;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/LANG', AValue);
  FXML.Flush;
  FData[Index].Lang := AValue;
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
  FXML := TXMLConfig.Create(nil);
  FFiles := TStringList.Create;
  Refresh;
end;

destructor TFDLanguages.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FXML);
  inherited Destroy;
end;

procedure TFDLanguages.Refresh;
var
  I, J : integer;
begin
  FileList(FFiles, FOwner.LanguagesPath + '*.xml');
  FFiles.Sort;
  SetLength(FData, Count);
  for I := 0 to Count - 1 do begin
    FXML.Filename:=FOwner.LanguagesPath + FFiles[I];
    FData[I].Caption := FXML.GetValue('LANGUAGE/CAPTION', FFiles[I]);
    FData[I].Identifier := FXML.GetValue('LANGUAGE/IDENTIFIER', '');
    FData[I].Lang := FXML.GetValue('LANGUAGE/LANG', '');
    FData[I].Graphic := FXML.GetValue('LANGUAGE/GRAPHIC', '');
    try
        FData[I].Codepage := StrToInt(FXML.GetValue('LANGUAGE/CODEPAGE', ''));
    except
      FData[I].Codepage := -1;
    end;
{    for J := 0 to 255 do begin
      FData[I].UnicodeMap[J] := IntsToStr(
        FXML.GetValue('TRANSLATE/ASCII_' + IntToStr(J) + '/UNICODE',
          WhenTrue(J <= 127, Char(J), '')
        ));
      FData[I].UTFMap[J] := IntsToStr(
        FXML.GetValue('TRANSLATE/ASCII_' + IntToStr(J) + '/UTF8',
          WhenTrue(J <= 127, Char(J), '')
        ));
    end;}
  end;
end;

function TFDLanguages.NewLanguage: integer;
var
  N : String;
  X, I : integer;
begin
  Result := -1;
  X := 0;
  N := 'newlang.xml';
  While X < 100 do begin
    if not FileExists(FOwner.LanguagesPath + N) then break;
    Inc(X);
    N := 'newlang-'+IntToStr(X)+'.xml';
  end;
  if X = 100 then exit;
  FXML.Filename:=FOwner.LanguagesPath + N;
  FXML.SetValue('LANGUAGE/CODEPAGE', -1);
  VCSAddFile(FOwner.LanguagesPath + N);
  I := FFiles.Add(N);
  if I <> Length(FData) then
     raise Exception.Create('internal sorting error');
  SetLength(FData, I + 1);
  Result := I;
  with FData[I] do begin
    Identifier := '';
    Lang := '';
    Caption := '';
    Graphic := '';
  end;
end;

procedure TFDLanguages.Delete(Index: integer);
begin
  VCSDeleteFile(FOwner.LanguagesPath + FFiles[Index]);
  DeleteFile(FOwner.LanguagesPath + FFiles[Index]);
  Refresh;
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

procedure TFDNLS.Refresh;
begin
  Languages.Refresh;
end;

end.
