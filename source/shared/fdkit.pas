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
    Language : String;
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
    function Add : integer;
    procedure Delete(Index : integer);
    procedure SetValue(Index : integer; KeyName : String; Value : String); overload;
    procedure SetValue(Index : integer; KeyName : String; Value : Integer); overload;
  published

  end;

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
    procedure Reload;
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
      if (GroupID <> '') and (FXML.GetValue('XMLGROUP/ID', '') <> GroupID) then begin
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
      FFiles.Delete(I);
    end;
  end;
end;

function TXMLGroup.Add: integer;
var
  N : String;
  X, I : integer;
  D : TObject;
begin
  Result := -1;
  X := 0;
  N := 'new-item.xml';
  While X < 100 do begin
    if not FileExists(GroupPath + N) then break;
    Inc(X);
    N := 'new-item'+IntToStr(X)+'.xml';
  end;
  if X = 100 then exit;
  FXML.Filename:=GroupPath + N;
  FXML.SetValue('XMLGROUP/ID', GroupID);
  FXML.SetValue('LANGUAGE/CODEPAGE', -1);
  FXML.Flush;
  VCSAddFile(GroupPath + N);
  X := -1;
  I := FFiles.Add(N);
  D := LoadData;
  if Assigned(D) then
    X := FData.Add(D);
  if I <> X then begin
     raise Exception.Create('internal sorting error');
  end else begin
    Result := I;
  end;
end;

procedure TXMLGroup.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= Count) then exit;
  VCSDeleteFile(GroupPath + FFiles[Index]);
  DeleteFile(GroupPath + FFiles[Index]);
  FFiles.Delete(Index);
  FData.Delete(Index);
end;

procedure TXMLGroup.SetValue(Index : integer; KeyName: String; Value: String);
begin
  if FXML.GetValue(GroupID + '/' + KeyName, '') = Value then exit;
  FXML.Filename:=GroupPath + FFiles[Index];
  FXML.SetValue(GroupID + '/' + KeyName, Value);
  FXML.Flush;
end;

procedure TXMLGroup.SetValue(Index : integer; KeyName: String; Value: Integer);
begin
  SetValue(Index, KeyName, IntToStr(Value));
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
var
  I : integer;
  T : String;
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
    O.Caption := FXML.GetValue('LANGUAGE/CAPTION', '');
    O.Identifier := FXML.GetValue('LANGUAGE/IDENTIFIER', '');
    O.Language := FXML.GetValue('LANGUAGE/LANG', '');
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
  Reload;
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

procedure TFDNLS.Reload;
begin
  Languages.Reload;
end;

end.
