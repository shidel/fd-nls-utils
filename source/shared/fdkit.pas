unit FDKit;

{$warn 5023 off : no warning about unused units}
interface

{$DEFINE UseLog}
uses
  Classes, SysUtils, Contnrs, Graphics, Controls, ExtCtrls, Grids, XMLConf,
  {$IFDEF UseLog}
    uLog,
  {$ENDIF}
  PasExt, ClassExt, VCSExt, Icons;

type
  TPackageState = (psGood, psNew, psWarning, psInvalid, psError);

  TLanguageData = class(TObject)
    Identifier : String;
    Caption : String;
    Language : String;
    CodePage : integer;
    Graphic : String;
  end;

  { TCodePageData }

  TCodePageData = class(TObject)
  private
    FUTF8Dict,
    FHTMLDict : TDictionary;
  public
    Identifier : String;
    UTF8       : array [0..255] of UTF8String;
    HTML       : array [0..255] of String;
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    function    DOStoUTF8( S : String ) : String;
    function    UTF8toDOS( S : String ) : String;
    function    DostoHTML( S : String ) : String;
    function    HTMLtoDOS( S : String ) : String;
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
    procedure Reload; override;
    property Data[Index : integer] : TCodePageData read GetData; default;
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
      Foreground : TColor = clBlack; Background : TColor = clWhite); overload;
    procedure ToImage(Index: integer; Ascii : integer; var Image : TImage;
      Foreground : TColor = clBlack; Background : TColor = clWhite); overload;
    procedure ToImageList(Index: integer; var Images : TImageList;
      Foreground : TColor = clBlack; Background : TColor = clWhite); overload;
  published
  end;

  { TFDPackageLists }

  TFDPackageLists = class (TFileGroup)
  private
    FFields: TStringList;
    FDetails : TStringList;
    FOwner: TFDNLS;
    FMasterCSV : TStringGrid;
    FMasterCP : integer;
    FFileCSV : array of TStringGrid;
    FCPIndex : array of integer;
    FModified : array of boolean;
    function GetLangDetails(LangIndex : integer; Index : integer): TStringList;
    function GetFields: TStringList;
    function GetFileCSV(Index : integer): TStringGrid;
    function GetMasterCSV: TStringGrid;
    function GetMasterDetails(Index : integer): TStringList;
    function GetModified: boolean;
    function GetPackageCount: integer;
    function GetPackageID(Index : integer): String;
    function GetPackageStatus(LangIndex : integer; Index : integer
      ): TPackageState;
    procedure SetModified(AValue: boolean);
  protected
    property Owner : TFDNLS read FOwner;
    function GroupPath : String; override;
    function IncludeFile(AFileName : String) : boolean; override;
    function IndexOfLanguage(AValue : String) : integer;
    procedure PurgeCSVData;
    procedure MakeCodePage(var CSV : TStringGrid; Codepage : integer);
    procedure MakeUTF8(var CSV : TStringGrid; Codepage : integer);
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    procedure Reload; override;
    property Modified : boolean read GetModified write SetModified;
    property Fields : TStringList read GetFields;
    property PackageCount : integer read GetPackageCount;
    property PackageID[Index : integer] : String read GetPackageID;
    property MasterDetails[Index : integer] : TStringList read GetMasterDetails;
    property LangDetails[LangIndex : integer; Index : integer] : TStringList
      read GetLangDetails;
    property StatusDetails[LangIndex : integer; Index : integer] : TPackageState
      read GetPackageStatus;
    function Language(Index : integer) : String; overload;
    function Language(ALanguage : String) : integer; overload;
    function CreateLanguage(ALanguage : String) : integer;
    procedure SetLangDetails(LangIndex : integer; Index : integer; AValue: TStringList);
    procedure SaveChanges;
    // Probably move to Private after Development
    property MasterCSV : TStringGrid read GetMasterCSV;
    property FileCSV[Index : integer] : TStringGrid read GetFileCSV;
  published
  end;

  { TFDNLS }
  TFDNLS = class(TPersistent)
  private
    FAutoCreate: boolean;
    FCodePages: TFDCodePages;
    FFonts: TFDFontFiles;
    FLanguages: TFDLanguages;
    FPackageLists: TFDPackageLists;
    function GetCodePagePath: string;
    function GetDataPath: string;
    function GetFontsPath: string;
    function GetLanguagesPath: string;
    function GetPackageListPath: String;
    function GetProjectsPath: string;
    function GetPath: string;
    procedure SetAutoCreate(AValue: boolean);
    procedure SetPath(AValue: string);
  protected
    property DataPath : string read GetDataPath;
    property LanguagesPath : string read GetLanguagesPath;
    property CodePagePath : string read GetCodePagePath;
    property FontsPath : string read GetFontsPath;
    property ProjectsPath : string read GetProjectsPath;
    property PackageListPath : String read GetPackageListPath;
  public
    constructor Create;
    destructor Destroy; override;
    property Path : string read GetPath write SetPath;
    property Languages : TFDLanguages read FLanguages;
    property CodePages : TFDCodePages read FCodePages;
    property Fonts : TFDFontFiles read FFonts;
    property PackageLists : TFDPackageLists read FPackageLists;
    property AutoCreate : boolean read FAutoCreate write SetAutoCreate;
    procedure Reload;
    function FindLanguage(ALanguage : String) : integer;
    function FindCodepage(ALanguage : String) : integer;
    function FindFont(ALanguage : String) : integer;
    function FindFlag(ALanguage : String) : integer;
  published

  end;

var
  FDNLS : TFDNLS;

const
   MasterCSVFile     : String = 'master.csv';
   MasterCSVLanguage : String = 'en_US';
   DefaultCSVFields  : array of string = (
    'title',
    'description',
    'summary',
    'keywords',
    'platforms',
    'copying-policy'
   );

   RequiredCSVFields  : array of string = (
    'title',
    'description',
    'copying-policy'
   );

implementation

const
   RepositoryPath    : String = '';

{ TCodePageData }

constructor TCodePageData.Create;
begin
  inherited Create;
end;

destructor TCodePageData.Destroy;
begin
  FreeAndNil(FUTF8Dict);
  FreeAndNil(FHTMLDict);
  inherited Destroy;
end;

procedure TCodePageData.Clear;
begin
  FreeAndNil(FUTF8Dict);
  FreeAndNil(FHTMLDict);
end;

function TCodePageData.DOStoUTF8(S: String): String;
var
  I : integer;
begin
  Result := '';
  for I := 1 to Length(S) do begin
    if S[I] in [CR, LF, TAB, SPACE] then
      Result := Result + S[I]
    else
      Result := Result + UTF8[Ord(S[I])];
  end;
end;

function TCodePageData.UTF8toDOS(S: String): String;
var
  I : integer;
begin
  if not Assigned(FUTF8Dict) then begin
    FUTF8Dict := TDictionary.Create;
    try
      for I := 0 to 255 do
        FUTF8Dict.Add(UTF8[I], Char(I));
    except
      FreeANdNil(FUTF8Dict);
    end;
  end;
  if Assigned(FUTF8Dict) then
    Result := FUTF8Dict.Rewrite(S)
  else
    Result := S;
end;

function TCodePageData.DostoHTML(S: String): String;
var
  I : integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] = CR then
    else if S[I] = LF then
      Result := Result + '<br>'
    else
      Result := Result + HTML[Ord(S[I])];
end;

function TCodePageData.HTMLtoDOS(S: String): String;
var
  I : integer;
begin
  if not Assigned(FHTMLDict) then begin
    FHTMLDict := TDictionary.Create;
    try
      for I := 0 to 255 do
        FHTMLDict.Add(HTML[I], Char(I));
    except
      FreeANdNil(FHTMLDict);
    end;
  end;
  if Assigned(FHTMLDict) then
    Result := FHTMLDict.Rewrite(S)
  else
    Result := S;
end;

{ TFDPackageLists }

function TFDPackageLists.GetMasterCSV: TStringGrid;
begin
  if not Assigned(FMasterCSV) then begin
    try
      FMasterCSV := TStringGrid.Create(nil);
      FMasterCSV.LoadFromCSVFile(GroupPath + MasterCSVFile);
      MakeUTF8(FMasterCSV, FDNLS.FindCodepage(MasterCSVLanguage));
    except
      Log(Self, 'exception opening ' + MasterCSVFile);
      FreeAndNil(FMasterCSV);
    end;
  end;
  Result := FMasterCSV;
end;

function TFDPackageLists.GetMasterDetails(Index : integer): TStringList;
var
  I, J : integer;
  H, D : String;
begin
  FDetails.Clear;
  Log(Self, 'get master details ' + IntToStr(Index));
  for I := 0 to FFields.Count - 1 do begin
    H := Lowercase(trim(FFields[I]));
    D := '';
    for J := 0 to MasterCSV.ColCount - 1 do begin
      if MasterCSV.Cells[J,0] = H then begin
        D := MasterCSV.Cells[J, Index + 1];
        Break;
      end;
    end;
    FDetails.Add(D);
  end;
  Result := FDetails;
end;

function TFDPackageLists.GetModified: boolean;
var
  I : integer;
begin
  Result := False;
  for I := 0 to Length(FModified) -1 do
    Result := Result or FModified[I];
end;

function TFDPackageLists.GetPackageCount: integer;
begin
  Result := MasterCSV.RowCount - 1;
end;

function TFDPackageLists.GetPackageID(Index : integer): String;
begin
  Result := MasterCSV.Cells[0, Index + 1];
end;

function TFDPackageLists.GetPackageStatus(LangIndex : integer; Index : integer
  ): TPackageState;
var
  I, J : integer;
  H, D : String;
begin
  // Log(Self, 'get language status ' + IntTostr(LangIndex) + '/' + IntToStr(Index));
  Result := psNew;
  D := lowercase(FMasterCSV.Cells[0, Index + 1]);
  if (LangIndex <> -1) then begin
    Index := -1;
    for I := 1 to FileCSV[LangIndex].RowCount - 1 do
      if D = lowercase(FFileCSV[LangIndex].Cells[0,I]) then begin
        Index := I;
        Break;
      end;
    if Index > 0 then begin
      Result := psGood;
      for I := 0 to FFields.Count - 1 do begin
        H := Lowercase(trim(FFields[I]));
        for J := 0 to FFileCSV[LangIndex].ColCount - 1 do begin
          if FFileCSV[LangIndex].Cells[J,0] = H then begin
            if Trim(FFileCSV[LangIndex].Cells[J, Index]) = '' then
              Result := psWarning;
            Break;
          end;
        end;
      end;
      for I := 0 to Length(RequiredCSVFields) - 1 do begin
        H := RequiredCSVFields[I];
        for J := 0 to FFileCSV[LangIndex].ColCount - 1 do begin
          if FFileCSV[LangIndex].Cells[J,0] = H then begin
            if Trim(FFileCSV[LangIndex].Cells[J, Index]) = '' then
              Result := psInvalid;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFDPackageLists.SetLangDetails(LangIndex : integer; Index : integer;
  AValue: TStringList);
var
  I, J : integer;
  H, D : String;
begin
  Log(Self, 'set Language details ' + IntTostr(LangIndex) + '/' + IntToStr(Index));
  D := lowercase(FMasterCSV.Cells[0, Index + 1]);
  if LangIndex <> -1 then begin
    Index := -1;
    for I := 1 to FileCSV[LangIndex].RowCount - 1 do
      if D = lowercase(FFileCSV[LangIndex].Cells[0,I]) then begin
        Index := I;
        Break;
      end;
    if Index = -1 then begin
       FFileCSV[LangIndex].InsertColRow(False, FFileCSV[LangIndex].RowCount);
       Index := FFileCSV[LangIndex].RowCount - 1;
       FFileCSV[LangIndex].Cells[0,Index]:=D;
       for J := 0 to FFileCSV[LangIndex].ColCount - 1 do begin
           if FFileCSV[LangIndex].Cells[J, 0] = 'sha' then
             FFileCSV[LangIndex].Cells[J,Index]:=ZeroPad(0,64);
       end;
    end;
    Log(Self, 'Index of "' + D + '" is ' + IntToStr(Index));
    if Index > 0 then
      for I := 0 to FFields.Count - 1 do begin
        H := Lowercase(trim(FFields[I]));
        for J := 0 to FFileCSV[LangIndex].ColCount - 1 do begin
          if FFileCSV[LangIndex].Cells[J,0] = H then begin
            if FFileCSV[LangIndex].Cells[J, Index]<>AValue[I] then begin
              FModified[LangIndex] := True;
              FFileCSV[LangIndex].Cells[J, Index]:=AValue[I];
            end;
            Break;
          end;
        end;
      end;
  end;
end;

procedure TFDPackageLists.SaveChanges;
var
  I : integer;
  G : TStringGrid;
  LId : String;
  CP : integer;
begin
  for I := 0 to FFiles.Count - 1 do begin
    if not FModified[I] then Continue;
    Log(Self, '*** save file ' + FFiles[I]);
    FFileCSV[I].SaveToCSVFile(GroupPath + FFiles[I] + '.UTF-8');
    VCSAdd(GroupPath + FFiles[I] + '.UTF-8');
    G := TStringGrid.Create(nil);
    try
      G.Assign(FFileCSV[I]);
      LId := Language(I);
      CP := FDNLS.FindCodepage(LId);
      MakeCodePage(G, CP);
      G.SaveToCSVFile(GroupPath + FFiles[I]);
      VCSAdd(GroupPath + FFiles[I]);
    finally
      FreeAndNil(G);
    end;
    FModified[I] := False;
  end;
end;

procedure TFDPackageLists.SetModified(AValue: boolean);
var
  I : integer;
begin
  if Modified=AValue then Exit;
  for I := 0 to Length(FModified) -1 do
      FModified[I]:=AValue;
end;

function TFDPackageLists.GetFileCSV(Index : integer): TStringGrid;
var
  LId : String;
begin
  try
    if not Assigned(FFileCSV[Index]) then begin
      try
        FFileCSV[Index] := TStringGrid.Create(nil);
        FFileCSV[Index].LoadFromCSVFile(GroupPath + FFiles[Index]);
        LId := Language(Index);
        FCPIndex[Index] := FDNLS.FindCodepage(LId);
        Log(Self, 'CVS load, ' + FFiles[Index] + ', ' + LId +
          ':' + IntToStr(FCPIndex[Index]));
        MakeUTF8(FFileCSV[Index], FCPIndex[Index] );
      except
        Log(Self, 'exception opening ' + FFiles[Index]);
        FreeAndNil(FFileCSV[Index - 1]);
      end;
    end;
    Result := FFileCSV[Index];
  except
    Result := nil;
  end;
end;

function TFDPackageLists.GetFields: TStringList;
begin
  Result := FFields;
end;

function TFDPackageLists.GetLangDetails(LangIndex : integer; Index : integer): TStringList;
var
  I, J : integer;
  H, D : String;
begin
  FDetails.Clear;
  Log(Self, 'get Language details ' + IntTostr(LangIndex) + '/' + IntToStr(Index));
  D := lowercase(FMasterCSV.Cells[0, Index + 1]);
  if LangIndex <> -1 then begin
    Index := -1;

    for I := 1 to FileCSV[LangIndex].RowCount - 1 do
      if D = lowercase(FFileCSV[LangIndex].Cells[0,I]) then begin
        Index := I;
        Break;
      end;
    Log(Self, 'Index of "' + D + '" is ' + IntToStr(Index));
    if Index > 0 then
      for I := 0 to FFields.Count - 1 do begin
        H := Lowercase(trim(FFields[I]));
        D := '';
        for J := 0 to FFileCSV[LangIndex].ColCount - 1 do begin
          if FFileCSV[LangIndex].Cells[J,0] = H then begin
            D := FFileCSV[LangIndex].Cells[J, Index];
            Break;
          end;
        end;
        FDetails.Add(D);
      end;
  end;
  D := '';
  while FDetails.Count < FFields.Count do begin
    FDetails.Add(D);
  end;
  Result := FDetails;
end;

function TFDPackageLists.GroupPath: String;
begin
  Result := FOwner.PackageListPath;
end;

function TFDPackageLists.IncludeFile(AFileName: String): boolean;
begin
  Result:=inherited IncludeFile(AFileName) and
    (Uppercase(ExtractFilename(AFileName)) = 'LISTING.CSV') and
    (not (Pos('UTF-8', Uppercase(AFilename)) > 0));
end;

constructor TFDPackageLists.Create(AOwner: TFDNLS);
var
  I : integer;
begin
 inherited Create;
 FOwner := AOwner;
 FMasterCP := -1;
 GroupID := 'CSV';
 Recursive := True;
 FFields := TStringList.Create;
 FDetails := TStringList.Create;
 for I := 0 to length(DefaultCSVFields) - 1 do
   FFields.Add(DefaultCSVFields[I]);
end;

destructor TFDPackageLists.Destroy;
begin
  SaveChanges;
  PurgeCSVData;
  FreeAndNil(FFields);
  FreeAndNil(FDetails);
  inherited Destroy;
end;

procedure TFDPackageLists.Reload;
var
  I : integer;
begin
  PurgeCSVData;
  inherited Reload;
  SetLength(FFileCSV,FFiles.Count);
  SetLength(FCPIndex,FFiles.Count);
  SetLength(FModified,FFiles.Count);
  for I := 0 to Length(FFileCSV) - 1 do begin
    FFileCSV[I] := nil;
    FCPIndex[I] := -1;
    FModified[I] := False;
  end;
end;

function TFDPackageLists.Language(Index: integer): String;
begin
  Result := FieldStr(FFiles[Index], 0, DirectorySeparator);
  Log(Self, 'Language for file ' + FFiles[Index] + ' is ' + Result);
end;

function TFDPackageLists.Language(ALanguage: String): integer;
begin
  Result := IndexofLanguage(ALanguage);
  Log(Self, 'Index of ' + ALanguage + ' is ' + IntToStr(Result));
end;

function TFDPackageLists.CreateLanguage(ALanguage: String): integer;
var
  H, D : String;
begin
  Result := -1;
  H := 'id,' + Implode(DefaultCSVFields, ',') + ',sha';
  D := IncludeTrailingPathDelimiter(GroupPath + lowercase(ALanguage));
  if not FileExists(D + 'listing.csv') then begin
    if not DirectoryExists(D) then
       if not CreateDir(D) then exit;
    if SaveToFile(D + 'listing.csv', H) = 0 then begin
      Result := FileAdd(D + 'listing.csv');
    end;
  end;
  Log(Self, 'Create new language ' + ALanguage + ' index ' + IntToStr(Result));

end;

function TFDPackageLists.IndexOfLanguage(AValue: String): integer;
var
  I : integer;
begin
  Result := -1;
  AValue := Trim(UpperCase(AValue));
  if AValue <> '' then
    for I := 0 to Count - 1 do
      if (AValue = Uppercase(FieldStr(FileName[I], 0, DirectorySeparator))) then begin
         Result := I;
         Break;
      end;
end;

procedure TFDPackageLists.PurgeCSVData;
var
  I : integer;
begin
  FreeAndNil(FMasterCSV);
  for I := 0 to Length(FFileCSV) - 1 do begin
    FreeAndNil(FFileCSV[I]);
    FCPIndex[I] := -1;
  end;
end;

procedure TFDPackageLists.MakeCodePage(var CSV: TStringGrid; Codepage: integer);
var
  I , J : integer;
begin
  CSV.SortColRow(True, 0);
  for I := 0 to CSV.ColCount - 1 do
    CSV.Cells[I,0] := Trim(Lowercase(CSV.Cells[I,0]));
  if CodePage >= 0 then begin
    Log(self, 'Convert CSV UTF8 to codepage ' + FDNLS.CodePages.Identifier[Codepage]);
    for J := 0 to CSV.RowCount - 1 do
      for I := 0 to CSV.ColCount - 1 do
        CSV.Cells[I,J] :=
          FDNLS.CodePages[Codepage].UTF8toDOS(CSV.Cells[I,J]);
  end;
end;

procedure TFDPackageLists.MakeUTF8(var CSV: TStringGrid; Codepage: integer);
var
  I , J : integer;
begin
  CSV.SortColRow(True, 0);
  for I := 0 to CSV.ColCount - 1 do
    CSV.Cells[I,0] := Trim(Lowercase(CSV.Cells[I,0]));
  if CodePage >= 0 then begin
    Log(self, 'Convert CSV codepage ' + FDNLS.CodePages.Identifier[Codepage] + ' to UTF8');
    for J := 0 to CSV.RowCount - 1 do
      for I := 0 to CSV.ColCount - 1 do
        CSV.Cells[I,J] :=
          FDNLS.CodePages[Codepage].DOStoUTF8(CSV.Cells[I,J]);
  end;
end;

{ TFDFontFiles }

function TFDFontFiles.GroupPath: String;
begin
  Result := FOwner.GetFontsPath;
end;

constructor TFDFontFiles.Create(AOwner : TFDNLS);
begin
  inherited Create;
  FOwner := AOwner;
  GroupID := 'fnt';
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
  F : TArrayOfBytes;
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

procedure TFDFontFiles.ToImage(Index: integer; Ascii: integer;
  var Image: TImage; Foreground: TColor; Background: TColor);
var
  BPC : word;
  F : TArrayOfBytes;
  X, Y, V : integer;
  B : TBitmap;
begin
  F := Data[Index].FileData;
  BPC := Length(F) div 256;
  try
    B := TBitmap.Create;
    B.SetSize(8, BPC);
    B.Canvas.Brush.Color:= Background;
    B.Canvas.FillRect(0,0,B.Width, B.Height);
    for Y := 0 to BPC - 1 do begin
      V := F[Ascii * BPC + Y];
      for X := 0 to 7 do
        if ((V shr (7 - X)) and 1 = 1) then
          B.Canvas.Pixels[X, Y] := Foreground;
    end;
    Image.Picture.Assign(B);
    FreeAndNil(B);
  except
    FreeAndNil(B);
    raise
  end;
end;

procedure TFDFontFiles.ToImageList(Index: integer; var Images: TImageList;
  Foreground: TColor; Background: TColor);
var
  BPC : word;
  F : TArrayOfBytes;
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
  O : TCodePageData;
  I : Integer;
  N : String;
  U : UTF8String;
  H : String;
begin
  O := TCodePageData.Create;
  try
    N := ExtractFileName(FXML.FileName);
    SetLength(N, Length(N) - Length(ExtractFileExt(N)));
    O.Identifier := GetValueXML(FXML, FGroupID + '/IDENTIFIER', '');
    if (O.Identifier <> N) and (O.Identifier <> '') then
       raise exception.Create('invalid codepage format');
    for I := 0 to 255 do begin
      U := AnsiToUTF8(Char(I));
      H:= Char(I);
      O.UTF8[I]    := IntsToStr(GetValueXML(FXML, FGroupID + '/ASCII_' + IntToStr(I) + '/UTF8', StrToInts(U)));
      O.HTML[I]    := IntsToStr(GetValueXML(FXML, FGroupID + '/ASCII_' + IntToStr(I) + '/HTML', StrToInts(H)));
    end;
    FXML.Flush;
  except
    FreeAndNil(O);
    raise
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

procedure TFDCodePages.Reload;
var
  O : TCodePageData;
  I, X : integer;
begin
  // For some reason, adding the dictionaries to TCodePageData started
  // causing an exception when trying to free the codepage data. For now,
  // just going to ignore it and wrap it to discard the exception.
  try
    X := FData.Count;
    Log(self, 'CP Free Indexes ' + IntToStr(X));
    FData.OwnsObjects:=False;
    for I := 0 to FData.Count - 1 do begin
      try
        O := TCodepageData(FData[I]);
        FreeAndNil(O);
        FData[I] := nil;
      except
        Log(self, 'Exception while destroying CP index ' + IntToStr(I));
      end;
    end;
    Log(self, 'Freed ' + IntToStr(X - FData.Count) + ' CP indexes');
    FData.Clear;
    Log(self, 'CP Cleared, ' + IntToStr(FData.Count) + ' remain');
  finally
    FData.OwnsObjects:=True;
  end;
  inherited Reload;
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
    if AValue[3] = '-' then AValue[3] := '_';
    if (Length(AValue) <> 2) and (Length(AValue) <> 5) then exit;
    if (Length(AValue) = 5) and (AValue[3] <> '_') then exit;
    AValue := Copy(AValue,1,3) + UpperCase(Copy(AValue, 4,2));
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
    O.Graphic:= GetValueXML(FXML, FGroupID + '/GRAPHIC', '');
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

function TFDNLS.GetPackageListPath: String;
begin
  Result := VerifiedPath(RepositoryPath, 'packages');
end;

function TFDNLS.GetProjectsPath: string;
begin
  Result := VerifiedPath(DataPath, 'projects');
end;

function TFDNLS.GetPath: string;
begin
  Result := RepositoryPath;
end;

procedure TFDNLS.SetAutoCreate(AValue: boolean);
begin
  if FAutoCreate=AValue then Exit;
  FAutoCreate:=AValue;
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
  FPackageLists := TFDPackageLists.Create(Self);
  if not Assigned(FDNLS) then FDNLS := Self;
end;

destructor TFDNLS.Destroy;
begin
  if FDNLS = Self then FDNLS := nil;
  FreeAndNil(FPackageLists);
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
  PackageLists.Reload;
end;

function TFDNLS.FindLanguage(ALanguage: String): integer;
begin
  ALanguage := Trim(Uppercase(ALanguage));
  Result := Languages.IndexOfIdentifier(ALanguage);
  if Result < 0 then
    Result := Languages.IndexOfLanguage(ALanguage);
end;

function TFDNLS.FindCodepage(ALanguage: String): integer;
begin
  Result := FindLanguage(ALanguage);
  if Result <> -1 then
    Result := Codepages.IndexOfIdentifier(IntToStr(Languages.CodePage[Result]));
end;

function TFDNLS.FindFont(ALanguage: String): integer;
begin
  Result := FindCodepage(ALanguage);
  if Result <> -1 then
    Result := Fonts.IndexOfFile(Codepages.Identifier[Result] + '.fnt');

end;

function TFDNLS.FindFlag(ALanguage: String): integer;
var
  I : integer;
  G, S : String;
begin
  Result := -1;
  I := FindLanguage(ALanguage);
  if I = -1 then exit;
  if Languages.Graphic[I] <> '' then begin
    G := IconPrefix + Languages.Graphic[I];
    for I := 0 to Length(IconFlags) - 1 do
      if G = IconFlags[I] then begin
        Result := I;
        Break;
      end;
  end else begin
    Result := 0;
    S := Uppercase(Languages.Identifier[I]);
    for I := 0 to Length(LanguageCodes) - 1 do
      if Uppercase(FieldStr(LanguageCodes[I],0,',')) = S then begin
        G := FieldStr(LanguageCodes[I],4,',');
        break;
      end;
    for I := 0 to Length(CountryData) - 1 do
      if G = FieldStr(CountryData[I], 1, ',') then begin
        Result := I;
        Break;
      end;
  end;
end;

initialization
  FDNLS := nil;
end.
