unit FDKit;

{$warn 5023 off : no warning about unused units}
interface

{$DEFINE UseLog}
uses
  Classes, SysUtils, Contnrs, PasExt,
  {$IFDEF UseLog}
    uLog,
  {$ENDIF}
  XMLConf, XMLExt, VCSExt;

type
  TLanguageData = class(TObject)
    Identifier : String;
    Caption : String;
    Language : String;
    CodePage : integer;
    Graphic : String;
  end;

  TCodePageData = class(TObject)
    Identifier : String;
  end;

  { TFDNLS }
  TFDNLS = class;

  { TFDLanguages }

  TFDLanguages = class(TXMLGroup)
  private
    FOwner: TFDNLS;
    function GetCaption(Index : integer): String;
    function GetCodePage(Index : integer): integer;
    function GetData(Index : integer): TLanguageData;
    function GetGraphic(Index : integer): String;
    function GetIdentifier(Index : integer): String;
    function GetLanguage(Index : integer): String;
    procedure SetCaption(Index : integer; AValue: String);
    procedure SetCodePage(Index : integer; AValue: integer);
    procedure SetGraphic(Index : integer; AValue: String);
    procedure SetIdentifier(Index : integer; AValue: String);
    procedure SetLanguage(Index : integer; AValue: String);
  protected
    property Owner : TFDNLS read FOwner;
    function GroupPath : String; override;
    function LoadData : TObject; override;
    property Data[Index : integer] : TLanguageData read GetData;
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    property Caption[Index : integer] : String read GetCaption write SetCaption;
    property Identifier[Index : integer] : String read GetIdentifier write SetIdentifier;
    property Language[Index : integer] : String read GetLanguage write SetLanguage;
    property CodePage[Index : integer] : integer read GetCodePage write SetCodePage;
    property Graphic[Index : integer] : String read GetGraphic write SetGraphic;
    function IndexOfIdentifier(AValue : String) : integer;
    function IndexOfLanguage(AValue : String) : integer;
  published
  end;

  { TFDCodePages }

  TFDCodePages = class(TXMLGroup)
  private
    FOwner: TFDNLS;
    function GetData(Index : integer): TCodePageData;
    function GetIdentifier(Index : integer): String;
    procedure SetIdentifier(Index : integer; AValue: String);
  protected
    property Owner : TFDNLS read FOwner;
    function GroupPath : String; override;
    function LoadData : TObject; override;
    property Data[Index : integer] : TCodePageData read GetData;
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    property Identifier[Index : integer] : String read GetIdentifier write SetIdentifier;
    function IndexOfIdentifier(AValue : String) : integer;
    function IndexOfCodePage(AValue : Integer) : integer;
  published
  end;

  { TFDNLS }
  TFDNLS = class(TPersistent)
  private
    FCodePages: TFDCodePages;
    FLanguages: TFDLanguages;
    function GetCodePagePath: string;
    function GetDataPath: string;
    function GetLanguagesPath: string;
    function GetProjectsPath: string;
    function GetPath: string;
    procedure SetPath(AValue: string);
  protected
    property DataPath : string read GetDataPath;
    property LanguagesPath : string read GetLanguagesPath;
    property CodePagePath : string read GetCodePagePath;
    property ProjectsPath : string read GetProjectsPath;
  public
    constructor Create;
    destructor Destroy; override;
    property Path : string read GetPath write SetPath;
    property Languages : TFDLanguages read FLanguages;
    property CodePages : TFDCodePages read FCodePages;
    procedure Reload;
  published
  end;

implementation

const
   RepositoryPath : String = '';

{ TFDCodePages }

function TFDCodePages.GetData(Index : integer): TCodePageData;
begin
  Result := TCodePageData(FData[Index]);
end;

function TFDCodePages.GetIdentifier(Index : integer): String;
begin
  Result := Data[Index].Identifier;
end;

procedure TFDCodePages.SetIdentifier(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Trim(AValue);
  if AValue = Data[Index].Identifier then exit;
  if AValue <> '' then begin
    T := Uppercase(AValue);
    for I := 0 to FData.Count - 1 do
        if (I <> Index) and (T = Uppercase(Data[I].Identifier)) then exit;
  end;
  SetValue(Index, 'IDENTIFIER', AValue);
  Data[Index].Identifier := AValue;
end;

function TFDCodePages.GroupPath: String;
begin
  Result := FOwner.GetCodePagePath;
end;

function TFDCodePages.LoadData: TObject;
var
  O : TLanguageData;
begin
  O := TLanguageData.Create;
  try
    O.Identifier := FXML.GetValue(FGroupID + '/IDENTIFIER', '');
  except
    FreeAndNil(O);
  end;
  Result := O;
end;

constructor TFDCodePages.Create(AOwner: TFDNLS);
begin
  inherited Create;
  FOwner := AOwner;
  FGroupID := 'CODEPAGE';
  Reload;
end;

destructor TFDCodePages.Destroy;
begin
  inherited Destroy;
end;

function TFDCodePages.IndexOfIdentifier(AValue: String): integer;
var
  I : integer;
begin
  Result := -1;
  AValue := Trim(UpperCase(AValue));
  if AValue <> '' then
    for I := 0 to FData.Count - 1 do
      if (AValue = Uppercase(Data[I].Identifier)) then begin
         Result := I;
         Break;
      end;
end;

function TFDCodePages.IndexOfCodePage(AValue: Integer): integer;
begin
  Result := IndexOfIdentifier(IntToStr(AValue));
end;

{ TFDLanguages }

function TFDLanguages.GetCaption(Index : integer): String;
begin
  Result := Data[Index].Caption;
end;

function TFDLanguages.GetCodePage(Index : integer): integer;
begin
  Result := Data[Index].Codepage;
end;

function TFDLanguages.GetData(Index : integer): TLanguageData;
begin
  Result := TLanguageData(FData[Index]);
end;

function TFDLanguages.GetGraphic(Index : integer): String;
begin
  Result := Data[Index].Graphic;
end;

function TFDLanguages.GetIdentifier(Index : integer): String;
begin
  Result := Data[Index].Identifier;
end;

function TFDLanguages.GetLanguage(Index : integer): String;
begin
  Result := Data[Index].Language;
end;

procedure TFDLanguages.SetCaption(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Trim(AValue);
  if AValue = Data[Index].Caption then exit;
  T := Uppercase(AValue);
  for I := 0 to FData.Count - 1 do
    if (I <> Index) and (T = Uppercase(TLanguageData(FData[I]).Caption)) then exit;
  SetValue(Index, 'CAPTION', AValue);
  Data[Index].Caption := AValue;
end;

procedure TFDLanguages.SetCodePage(Index : integer; AValue: integer);
begin
  SetValue(Index, 'CODEPAGE', AValue);
  Data[Index].Codepage := AValue;
end;

procedure TFDLanguages.SetGraphic(Index : integer; AValue: String);
begin
  AValue := Trim(LowerCase(AValue));
  SetValue(Index, 'GRAPHIC', AValue);
  Data[Index].Graphic := AValue;
end;

procedure TFDLanguages.SetIdentifier(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Trim(AValue);
  if AValue = Data[Index].Identifier then exit;
  if AValue <> '' then begin
    T := Uppercase(AValue);
    for I := 0 to FData.Count - 1 do
        if (I <> Index) and (T = Uppercase(Data[I].Identifier)) then exit;
  end;
  SetValue(Index, 'IDENTIFIER', AValue);
  Data[Index].Identifier := AValue;
end;

procedure TFDLanguages.SetLanguage(Index : integer; AValue: String);
begin
  AValue := Uppercase(Trim(AValue));
  SetValue(Index, 'LANG', AValue);
  Data[Index].Language := AValue;
end;

function TFDLanguages.GroupPath: String;
begin
  Result := FOwner.GetLanguagesPath;
end;

function TFDLanguages.LoadData: TObject;
var
  O : TLanguageData;
begin
  O := TLanguageData.Create;
  try
    O.Identifier := FXML.GetValue(FGroupID + '/IDENTIFIER', '');
    O.Caption := FXML.GetValue(FGroupID + '/CAPTION', '');
    O.Language := FXML.GetValue(FGroupID + '/LANG', '');
    O.CodePage := StrToInt(FXML.GetValue(FGroupID + '/CODEPAGE', '-1'));
  except
    FreeAndNil(O);
  end;
  Result := O;
end;

constructor TFDLanguages.Create(AOwner : TFDNLS);
begin
  inherited Create;
  FOwner := AOwner;
  FGroupID := 'LANGUAGE';
  Reload;
end;

destructor TFDLanguages.Destroy;
begin
  inherited Destroy;
end;

function TFDLanguages.IndexOfIdentifier(AValue: String): integer;
var
  I : integer;
begin
  Result := -1;
  AValue := Trim(UpperCase(AValue));
  if AValue <> '' then
    for I := 0 to FData.Count - 1 do
      if (AValue = Uppercase(Data[I].Identifier)) then begin
         Result := I;
         Break;
      end;
end;

function TFDLanguages.IndexOfLanguage(AValue: String): integer;
var
  I : integer;
begin
  Result := -1;
  AValue := Trim(UpperCase(AValue));
  if AValue <> '' then
    for I := 0 to FData.Count - 1 do
      if (AValue = Uppercase(Data[I].Language)) then begin
         Result := I;
         Break;
      end;
end;

{ TFDNLS }

function TFDNLS.GetDataPath: string;
begin
  Result := VerifiedPath(RepositoryPath, 'fd-nls');
end;

function TFDNLS.GetCodePagePath: string;
begin
  Result := VerifiedPath(DataPath, 'codepages');
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

constructor TFDNLS.Create;
begin
  inherited Create;
  FLanguages := TFDLanguages.Create(Self);
  FCodePages := TFDCodepages.Create(Self);
end;

destructor TFDNLS.Destroy;
begin
  FreeAndNil(FCodePages);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

procedure TFDNLS.Reload;
begin
  Languages.Reload;
  CodePages.Reload;
end;

end.
