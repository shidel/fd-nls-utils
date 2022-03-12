unit FDKit;

{$warn 5023 off : no warning about unused units}
interface

{$DEFINE UseLog}
uses
  Classes, SysUtils, Contnrs, Graphics, Controls, ExtCtrls, PasExt,
  {$IFDEF UseLog}
    uLog,
  {$ENDIF}
  XMLConf, ClassExt, VCSExt;

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
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    property Data[Index : integer] : TLanguageData read GetData;
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
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    property Data[Index : integer] : TCodePageData read GetData;
    property Identifier[Index : integer] : String read GetIdentifier write SetIdentifier;
    function IndexOfIdentifier(AValue : String) : integer;
    function IndexOfCodePage(AValue : Integer) : integer;
  published
  end;

  { TFDFontFiles }

  TFDFontFiles = class (TFileGroup)
  private
    FOwner: TFDNLS;
  protected
    property Owner : TFDNLS read FOwner;
    function GroupPath : String; override;
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    function  AsBitmap(Index : integer; Foreground : TColor = clBlack;
      Background : TColor = clWhite) : TBitmap;
    procedure ToImage(Index: integer; var Image : TImage;
      Foreground : TColor = clBlack; Background : TColor = clWhite);
    procedure ToImageList(Index: integer; var Images : TImageList;
      Foreground : TColor = clBlack; Background : TColor = clWhite);
  published
  end;

  { TFDNLS }
  TFDNLS = class(TPersistent)
  private
    FCodePages: TFDCodePages;
    FFonts: TFDFontFiles;
    FLanguages: TFDLanguages;
    function GetCodePagePath: string;
    function GetDataPath: string;
    function GetFontsPath: string;
    function GetLanguagesPath: string;
    function GetProjectsPath: string;
    function GetPath: string;
    procedure SetPath(AValue: string);
  protected
    property DataPath : string read GetDataPath;
    property LanguagesPath : string read GetLanguagesPath;
    property CodePagePath : string read GetCodePagePath;
    property FontsPath : string read GetFontsPath;
    property ProjectsPath : string read GetProjectsPath;
  public
    constructor Create;
    destructor Destroy; override;
    property Path : string read GetPath write SetPath;
    property Languages : TFDLanguages read FLanguages;
    property CodePages : TFDCodePages read FCodePages;
    property Fonts : TFDFontFiles read FFonts;
    procedure Reload;
  published
  end;

implementation

const
   RepositoryPath : String = '';

{ TFDFontFiles }

function TFDFontFiles.GroupPath: String;
begin
  Result := FOwner.GetFontsPath;
end;

constructor TFDFontFiles.Create(AOwner : TFDNLS);
begin
  inherited Create;
  FOwner := AOwner;
  GroupID := 'FNT';
end;

destructor TFDFontFiles.Destroy;
begin
  inherited Destroy;
end;

function TFDFontFiles.AsBitmap(Index: integer; Foreground: TColor;
  Background: TColor): TBitmap;
const
  Columns : integer = 32;
var
  Rows : integer;
  BPC : word;
  F : TByteArray;
  X, Y, XX, YY, V : integer;
begin
  try
    Result := TBitMap.Create;
    F := Data[Index].FileData;
    BPC := Length(F) div 256;
    Rows := 256 div Columns;
    if Rows * Columns <> 256 then Inc(Rows);
    Result.SetSize(Columns * 8, Rows * BPC);
    Result.Canvas.Brush.Color:= Background;
    Result.Canvas.FillRect(0,0,Result.Width, Result.Height);
    for Y := 0 to Rows - 1 do
      for X := 0 to Columns - 1 do
        if Y * Columns + X < 256 then
          for YY := 0 to BPC - 1 do begin
            V := F[(Y * Columns + X) * BPC + YY];
            for XX := 0 to 7 do
              if ((V shr (7 - XX)) and 1 = 1) then
                Result.Canvas.Pixels[X * 8 + XX, Y * BPC + YY] := Foreground;
          end;
  except
    FreeAndNil(Result);
  end;
end;

procedure TFDFontFiles.ToImage(Index: integer; var Image: TImage;
  Foreground: TColor; Background: TColor);
var
  I : TBitmap;
begin
  try
    I := AsBitmap(Index,Foreground, Background);
    Image.Picture.Assign(I);
  finally
    FreeAndNil(I);
  end;
end;

procedure TFDFontFiles.ToImageList(Index: integer; var Images: TImageList;
  Foreground: TColor; Background: TColor);
var
  BPC : word;
  F : TByteArray;
  C, X, Y, V : integer;
  B, N : TBitmap;
begin
  F := Data[Index].FileData;
  BPC := Length(F) div 256;
  for C := 0 to 255 do
    try
      B := TBitmap.Create;
      B.SetSize(8, BPC);
      B.Canvas.Brush.Color:= Background;
      B.Canvas.FillRect(0,0,B.Width, B.Height);
      for Y := 0 to BPC - 1 do begin
        V := F[C * BPC + Y];
        for X := 0 to 7 do
          if ((V shr (7 - X)) and 1 = 1) then
            B.Canvas.Pixels[X, Y] := Foreground;
      end;
      if (Images.Width <> 8) or (Images.Height <> BPC) then begin
         try
           N := ScaleBitmap(B, Images.Width, Images.Height);
           Images.AddMasked(N, Background);
           FreeAndNil(N);
         except
           FreeAndNil(N);
           raise
         end;
      end else
        Images.AddMasked(B, Background);
      FreeAndNil(B);
    except
      FreeAndNil(B);
      raise
    end;
end;

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
    O.Identifier := GetValueXML(FXML, FGroupID + '/IDENTIFIER', '');
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
  if GroupPath <> '' then Reload;
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
    O.Identifier := GetValueXML(FXML, FGroupID + '/IDENTIFIER', '');
    O.Caption := GetValueXML(FXML, FGroupID + '/CAPTION', '');
    O.Language := GetValueXML(FXML, FGroupID + '/LANG', '');
    O.CodePage := StrToInt(GetValueXML(FXML, FGroupID + '/CODEPAGE', '-1'));
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
  if GroupPath <> '' then Reload;
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

function TFDNLS.GetFontsPath: string;
begin
  Result := VerifiedPath(DataPath, 'fonts');
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
  Reload;
end;

constructor TFDNLS.Create;
begin
  inherited Create;
  FLanguages := TFDLanguages.Create(Self);
  FCodePages := TFDCodepages.Create(Self);
  FFonts     := TFDFontFiles.Create(Self);
end;

destructor TFDNLS.Destroy;
begin
  FreeAndNil(FFonts);
  FreeAndNil(FCodePages);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

procedure TFDNLS.Reload;
begin
  Languages.Reload;
  CodePages.Reload;
  Fonts.Reload;
end;

end.
