{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FDKit;

{$warn 5023 off : no warning about unused units}
interface

{$DEFINE UseLog}
uses
  Classes, SysUtils, Contnrs, PasExt,
  {$IFDEF UseLog}
    uLog,
  {$ENDIF}
  XMLConf, VCSExt;

type
  TLanguageData = class(TObject)
    Caption : String;
    Identifier : String;
    Lang : String;
    CodePage : integer;
    Graphic : String;
  end;

  { TFDNLS }
  TFDNLS = class;

  { TXMLGroup }

  TXMLGroup = class(TPersistent)
  private
    FGroupID: String;
    FXML : TXMLConfig;
    FFiles : TStringList;
    FData : TObjectList;
    function GetCount: integer;
    function GetFileName(Index : integer): String;
    procedure SetFileName(Index : integer; AValue: String);
    procedure SetGroupID(AValue: String);
  protected
    property XML : TXMLConfig read FXML;
    function GroupPath : String; virtual; abstract;
    function LoadData : TObject; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    property GroupID : String read FGroupID write SetGroupID;
    property Count : integer read GetCount;
    property Filename[Index : integer] : String read GetFileName write SetFileName;
    procedure Reload;
  published

  end;

  { TFDLanguages }

  TFDLanguages = class(TXMLGroup)
  private
    FOwner: TFDNLS;
    function GetCaption(Index : integer): String;
    function GetCodePage(Index : integer): integer;
    function GetGraphic(Index : integer): String;
    function GetIdentifier(Index : integer): String;
    function GetLang(Index : integer): String;
    procedure SetCaption(Index : integer; AValue: String);
    procedure SetCodePage(Index : integer; AValue: integer);
    procedure SetGraphic(Index : integer; AValue: String);
    procedure SetIdentifier(Index : integer; AValue: String);
    procedure SetLang(Index : integer; AValue: String);
  protected
    property Owner : TFDNLS read FOwner;
    function GroupPath : String; override;
    function LoadData : TObject; override;
  public
    constructor Create(AOwner : TFDNLS);
    destructor Destroy; override;
    procedure Refresh;
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

{ TXMLGroup }

function TXMLGroup.GetCount: integer;
begin
  FFiles.Count;
end;

function TXMLGroup.GetFileName(Index : integer): String;
begin
  Result := FFiles[Index];
end;

procedure TXMLGroup.SetFileName(Index : integer; AValue: String);
begin
  AValue := Trim(Lowercase(AValue));
  if FileExists(GroupPath + AValue) then exit;
  if FileExists(GroupPath + FFiles[Index]) then begin
    if not RenameFile(GroupPath + FFiles[Index], GroupPath + AValue) then exit;
  end;
  FFiles[Index] := AValue;
  FXML.Filename:=GroupPath + FFiles[Index];
end;

procedure TXMLGroup.SetGroupID(AValue: String);
begin
  if FGroupID=AValue then Exit;
  FGroupID:=AValue;
end;

constructor TXMLGroup.Create;
begin
  inherited Create;
  FGroupID := '';
  FXML := TXMLConfig.Create(nil);
  FFiles := TStringList.Create;
  FData := TObjectList.Create(true); // Free removed objects
end;

destructor TXMLGroup.Destroy;
begin
  FreeAndNil(FData);
  FreeAndNil(FFiles);
  FreeAndNil(FXML);
  inherited Destroy;
end;

procedure TXMLGroup.Reload;
var
  I, X : Integer;
  D : TObject;
begin
  {$IFDEF UseLog}
    Log(nil,'XML_GROUP, file list ' + GroupPath );
  {$ENDIF}
  FileList(FFiles, GroupPath + '*.xml');
  FData.Clear;
  FFiles.Sort;
  I := 0;
  while I < FFiles.Count do begin
    try
      FXML.Filename:=GroupPath + FFiles[I];
      if (GroupID <> '') and (FXML.GetValue('XMLGROUP/CLASS/ID', '') <> GroupID) then begin
        {$IFDEF UseLog}
          Log(nil,'XML_GROUP, ' + GroupID + ' verification failed for ' + FFiles[I]);
        {$ENDIF}
         FFiles.Delete(I)
      end
      else begin
        D := LoadData;
        if not Assigned(D) then
          FFiles.Delete(I)
        else begin
          X := FData.Add(D);
          if I <> X then begin
            {$IFDEF UseLog}
              Log(nil,'XML_GROUP, maligned indexes ' + IntToStr(I) + ':' + IntToStr(X) + ' with ' + FFiles[I]);
            {$ENDIF}
            raise exception.Create('maligned list index management error');
          end;
          inc(I);
        end;
      end;
    except
      {$IFDEF UseLog}
        Log(nil,'XML_GROUP, ERROR ' + FFiles[I] + ' raised exception');
      {$ENDIF}
      FXML.FileName := '';
      FFiles.Delete(I)
    end;
  end;
end;

{ TFDLanguages }

function TFDLanguages.GetCaption(Index : integer): String;
begin
  Result := TLanguageData(FData[Index]).Caption;
end;

function TFDLanguages.GetCodePage(Index : integer): integer;
begin
  Result := TLanguageData(FData[Index]).Codepage;
end;

function TFDLanguages.GetGraphic(Index : integer): String;
begin
  Result := TLanguageData(FData[Index]).Graphic;
end;

function TFDLanguages.GetIdentifier(Index : integer): String;
begin
  Result := TLanguageData(FData[Index]).Identifier;
end;

function TFDLanguages.GetLang(Index : integer): String;
begin
  Result := TLanguageData(FData[Index]).Lang;
end;

procedure TFDLanguages.SetCaption(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Trim(AValue);
  if AValue = TLanguageData(FData[Index]).Caption then exit;
  T := Uppercase(AValue);
  for I := 0 to FData.Count - 1 do
    if (I <> Index) and (T = Uppercase(TLanguageData(FData[I]).Caption)) then exit;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/CAPTION', AValue);
  FXML.Flush;
  TLanguageData(FData[Index]).Caption := AValue;
end;

procedure TFDLanguages.SetCodePage(Index : integer; AValue: integer);
begin
  if AValue = TLanguageData(FData[Index]).CodePage then exit;
  // OK to have a duplicate codepage
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/CODEPAGE', AValue);
  FXML.Flush;
  TLanguageData(FData[Index]).Codepage := AValue;
end;

procedure TFDLanguages.SetGraphic(Index : integer; AValue: String);
begin
  AValue := Trim(LowerCase(AValue));
  if AValue = TLanguageData(FData[Index]).Graphic then exit;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/GRAPHIC', AValue);
  FXML.Flush;
  TLanguageData(FData[Index]).Graphic := AValue;
end;

procedure TFDLanguages.SetIdentifier(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Trim(AValue);
  if AValue = TLanguageData(FData[Index]).Identifier then exit;
  if AValue <> '' then begin
    T := Uppercase(AValue);
    for I := 0 to FData.Count - 1 do
        if (I <> Index) and (T = Uppercase(TLanguageData(FData[I]).Identifier)) then exit;
  end;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/IDENTIFIER', AValue);
  FXML.Flush;
  TLanguageData(FData[Index]).Identifier := AValue;
end;

procedure TFDLanguages.SetLang(Index : integer; AValue: String);
var
  I : integer;
  T : String;
begin
  AValue := Uppercase(Trim(AValue));
  if AValue = TLanguageData(FData[Index]).Lang then exit;
  if AValue <> '' then begin
    for I := 0 to FData.Count - 1 do
        if AValue = TLanguageData(FData[I]).Lang then exit;
  end;
  FXML.Filename:=FOwner.LanguagesPath + FFiles[Index];
  FXML.SetValue('LANGUAGE/LANG', AValue);
  FXML.Flush;
  TLanguageData(FData[Index]).Lang := AValue;
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
    O.Caption := FXML.GetValue('LANGUAGE/CAPTION', ExtractFilename(FXML.Filename));
    O.Identifier := FXML.GetValue('LANGUAGE/IDENTIFIER', '');
    O.Lang := FXML.GetValue('LANGUAGE/LANG', '');
    O.CodePage := StrToInt(FXML.GetValue('LANGUAGE/CODEPAGE', '-1'));
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
  Refresh;
end;

destructor TFDLanguages.Destroy;
begin
  inherited Destroy;
end;

procedure TFDLanguages.Refresh;

begin
  Reload;
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
{  I := FFiles.Add(N);
  if I <> Length(FData) then
     raise Exception.Create('internal sorting error');
  FXML.Filename:=FOwner.LanguagesPath + N;
  FXML.SetValue('LANGUAGE/CODEPAGE', -1);
  FXML.Flush;
  VCSAddFile(FOwner.LanguagesPath + N);
  SetLength(FData, I + 1);
  Result := I;
  with FData[I] do begin
    Identifier := '';
    Lang := '';
    Caption := '';
    Graphic := '';
  end;            }
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
