unit ClassExt;

{$WARN 5023 off : no warning about unused units}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}

interface

{$DEFINE UseLog}
uses
  Classes, SysUtils, Contnrs, PasExt,
  {$IFDEF UseLog}
    uLog,
  {$ENDIF}
  XMLConf, VCSExt;

type

  { TFileObject }

  TFileObject = class(TObject)
    FileData : TByteArray;
    procedure WriteFile(FileName : String); virtual;
    procedure ReadFile(FileName : String); virtual;
  end;

  { TFileGroup }

  TFileGroup = class(TPersistent)
  private
    FLoadAll: boolean;
    function GetFileObject(Index : integer): TFileObject;
    procedure SetLoadAll(AValue: boolean);
    procedure SetFileObject(Index : integer; AValue: TFileObject);
  protected
    FGroupID, FExt: String;
    FFiles : TStringList;
    FData : TObjectList;
    function GetCount: integer;
    function GetFileName(Index : integer): String;
    procedure SetFileName(Index : integer; AValue: String);
    procedure SetGroupID(AValue: String);
    function GroupPath : String; virtual; abstract;
    function LoadFile(Index : integer) : TFileObject; virtual;
    procedure SaveFile(Index : integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property GroupID : String read FGroupID write SetGroupID;
    property Count : integer read GetCount;
    property LoadAll : boolean read FLoadAll write SetLoadAll;
    property Filename[Index : integer] : String read GetFileName write SetFileName;
    property Data[Index : integer] : TFileObject read GetFileObject write SetFileObject;
    function IndexOfFile(AValue : String) : integer;
    procedure Reload; virtual;
    function Add : integer;
    procedure Delete(Index : integer);
  published
  end;

  { TXMLGroup }

  // TXMLGroup was needed/created before TFileGroup. At some point, it will
  // probably be reworked into a decendant of TFileGroup.

  TXMLGroup = class(TPersistent)
  private
  protected
    FGroupID: String;
    FXML : TXMLConfig;
    FFiles : TStringList;
    FData : TObjectList;
    function GetCount: integer;
    function GetFileName(Index : integer): String;
    procedure SetFileName(Index : integer; AValue: String);
    procedure SetGroupID(AValue: String);

    property XML : TXMLConfig read FXML;
    function GroupPath : String; virtual; abstract;
    function LoadData : TObject; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    property GroupID : String read FGroupID write SetGroupID;
    property Count : integer read GetCount;
    property Filename[Index : integer] : String read GetFileName write SetFileName;
    function IndexOfFile(AValue : String) : integer;
    procedure Reload;
    function Add : integer;
    procedure Delete(Index : integer);
    procedure SetValue(Index : integer; KeyName : String; Value : String); overload;
    procedure SetValue(Index : integer; KeyName : String; Value : Integer); overload;
  published
  end;

function GetValueXML(XML : TXMLConfig; Key : String; Default : boolean = false) : boolean; overload;
function GetValueXML(XML : TXMLConfig; Key : String; Default : integer = 0) : Integer; overload;
function GetValueXML(XML : TXMLConfig; Key : String; Default : String = '') : String; overload;
procedure SetValueXML(XML : TXMLConfig; Key : String; Value : boolean); overload;
procedure SetValueXML(XML : TXMLConfig; Key : String; Value : integer); overload;
procedure SetValueXML(XML : TXMLConfig; Key : String; Value : String); overload;

implementation

function GetValueXML(XML: TXMLConfig; Key: String; Default: boolean): boolean;
var
  S : String;
begin
  S := Uppercase(Trim(GetValueXML(XML, Key, '')));
  if S = '' then
    Result := Default
  else if (S = 'FALSE') or (S = 'NO') or (S = '0') or (S = 'OFF')
  or (S = 'DISABLE') or (S = 'DISABLED') then
    Result := False
  else
    Result := True;
end;

function GetValueXML(XML: TXMLConfig; Key: String; Default: integer): Integer;
begin
  Result := XML.GetValue(Key, Default);
end;

function GetValueXML(XML: TXMLConfig; Key: String; Default: String): String;
begin
  Result := XML.GetValue(Key, Default);
end;

procedure SetValueXML(XML: TXMLConfig; Key: String; Value: boolean);
begin
  XML.SetValue(Key, WhenTrue(Value, 'true', 'false'));
end;

procedure SetValueXML(XML: TXMLConfig; Key: String; Value: integer);
begin
  XML.SetValue(Key, Value);
end;

procedure SetValueXML(XML: TXMLConfig; Key: String; Value: String);
begin
  XML.SetValue(Key, Value);
end;

{ TFileObject }

procedure TFileObject.WriteFile(FileName: String);
begin

end;

procedure TFileObject.ReadFile(FileName: String);
begin

end;

{ TFileGroup }

procedure TFileGroup.SetLoadAll(AValue: boolean);
begin
  if FLoadAll=AValue then Exit;
  FLoadAll:=AValue;
end;

function TFileGroup.GetFileObject(Index : integer): TFileObject;
begin
  try
    if not Assigned(FData[Index]) then
      FData[Index] := LoadFile(Index);
    Result := TFileObject(FData[Index]);
  except
    Result := nil
  end;
end;

procedure TFileGroup.SetFileObject(Index : integer; AValue: TFileObject);
var
  O : TObject;
begin
  O := FData[Index];
  if Assigned(O) then
    FreeAndNil(O);
  FData[Index] := AValue;
  SaveFile(Index);
end;

function TFileGroup.GetCount: integer;
begin
  Result := FFiles.Count;
end;

function TFileGroup.GetFileName(Index: integer): String;
begin
  Result := FFiles[Index];
end;

procedure TFileGroup.SetFileName(Index: integer; AValue: String);
begin
  AValue := Trim(Lowercase(AValue));
  if FileExists(GroupPath + AValue) then exit;
  if FileExists(GroupPath + FFiles[Index]) then begin
    if not RenameFile(GroupPath + FFiles[Index], GroupPath + AValue) then exit;
    VCSDelete(GroupPath + FFiles[Index]);
    VCSAdd(GroupPath + AValue);
  end;
  FFiles[Index] := AValue;
end;

procedure TFileGroup.SetGroupID(AValue: String);
begin
  AValue := Trim(AValue);
  if FGroupID=AValue then Exit;
  FGroupID:=AValue;
  FExt := AValue;
  if FExt <> '' then FExt := '.' + FExt;
end;

constructor TFileGroup.Create;
begin
  inherited Create;
  FLoadAll := False;
  FGroupID := '';
  FExt := '';
  FFiles := TStringList.Create;
  FData := TObjectList.Create(true); // Free removed objects
end;

destructor TFileGroup.Destroy;
begin
  FreeAndNil(FData);
  FreeAndNil(FFiles);
  inherited Destroy;
end;

function TFileGroup.IndexOfFile(AValue: String): integer;
var
  I : integer;
begin
  Result := FFiles.IndexOfName(AValue);
  if Result < 0 then begin
    AValue := Trim(UpperCase(AValue));
    if AValue <> '' then
      for I := 0 to FFiles.Count - 1 do
        if (AValue = Uppercase(FFiles[I])) then begin
           Result := I;
           Break;
        end;
  end;
end;

procedure TFileGroup.Reload;
var
  I, X : Integer;
  D : TFileObject;
begin
  {$IFDEF UseLog}
    Log(nil,'FILE_GROUP, file list ' + GroupPath );
  {$ENDIF}
  FileList(FFiles, GroupPath + '*' + FExt);
  FData.Clear;
  FFiles.Sort;
  I := 0;
  while I < FFiles.Count do begin
    try
      if LoadAll then
        D := LoadFile(I)
      else
        D := nil;
      X := FData.Add(D);
      if I <> X then begin
        {$IFDEF UseLog}
          Log(nil,'FILE_GROUP, maligned indexes ' + IntToStr(I) + ':' + IntToStr(X) + ' with ' + FFiles[I]);
        {$ENDIF}
        raise exception.Create('maligned list index management error');
      end;
      inc(I);
    except
      {$IFDEF UseLog}
        Log(nil,'FILE_GROUP, ERROR ' + FFiles[I] + ' raised exception');
      {$ENDIF}
      FFiles.Delete(I);
    end;
  end;
end;

function TFileGroup.Add: integer;
var
  N : String;
  X, I : integer;
begin
  Result := -1;
  X := 0;
  N := 'new-file'   + FExt;
  While X < 100 do begin
    if not FileExists(GroupPath + N) then break;
    Inc(X);
    N := 'new-file'+IntToStr(X) + FExt;
  end;
  if X = 100 then exit;
  I := FFiles.Add(N);
  X := FData.Add(nil);
  if I <> X then begin
     raise Exception.Create('internal list index error');
  end else begin
    Result := I;
    try
      SaveFile(I);
      VCSAdd(GroupPath + N);
    except
      FFiles.Delete(I);
      FData.Delete(I);
      Result := -1;
    end;
  end;
end;

procedure TFileGroup.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= Count) then exit;
  VCSDelete(GroupPath + FFiles[Index]);
  DeleteFile(GroupPath + FFiles[Index]);
  FFiles.Delete(Index);
  FData.Delete(Index);
end;

function TFileGroup.LoadFile(Index : integer): TFileObject;
var
  O : TFileObject;
begin
  O := TFileObject(FData[Index]);
  if Assigned(O) then FreeAndNil(O);
  FData[Index] := nil;
  O := TFileObject.Create;
  try
    O.ReadFile(GroupPath + FFiles[Index]);
    FData[Index] := O;
    Result := O;
  except
    FreeAndNil(O);
    Result := nil;
  end;
end;

procedure TFileGroup.SaveFile(Index: integer);
begin
  if Assigned(FData[Index]) then begin
    TFileObject(FData[Index]).WriteFile(GroupPath + FFiles[Index]);
  end;
end;

{ TXMLGroup }

function TXMLGroup.GetCount: integer;
begin
  Result := FFiles.Count;
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
    VCSDelete(GroupPath + FFiles[Index]);
    VCSAdd(GroupPath + AValue);
  end;
  FFiles[Index] := AValue;
  FXML.Filename:=GroupPath + FFiles[Index];
end;

procedure TXMLGroup.SetGroupID(AValue: String);
begin
  AValue := Trim(AValue);
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

function TXMLGroup.IndexOfFile(AValue: String): integer;
var
  I : integer;
begin
  Result := FFiles.IndexOfName(AValue);
  if Result < 0 then begin
    AValue := Trim(UpperCase(AValue));
    if AValue <> '' then
      for I := 0 to FFiles.Count - 1 do
        if (AValue = Uppercase(FFiles[I])) then begin
           Result := I;
           Break;
        end;
  end;
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
      if (GroupID <> '') and (GetValueXML(FXML, 'XMLGROUP/ID', '') <> GroupID) then begin
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
  VCSAdd(GroupPath + N);
  X := -1;
  I := FFiles.Add(N);
  D := LoadData;
  if Assigned(D) then
    X := FData.Add(D);
  if I <> X then begin
     raise Exception.Create('internal list index error');
  end else begin
    Result := I;
  end;
end;

procedure TXMLGroup.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= Count) then exit;
  VCSDelete(GroupPath + FFiles[Index]);
  DeleteFile(GroupPath + FFiles[Index]);
  FFiles.Delete(Index);
  FData.Delete(Index);
end;

procedure TXMLGroup.SetValue(Index : integer; KeyName: String; Value: String);
begin
  if GetValueXML(FXML, GroupID + '/' + KeyName, '') = Value then exit;
  FXML.Filename:=GroupPath + FFiles[Index];
  SetValueXML(FXML, GroupID + '/' + KeyName, Value);
  FXML.Flush;
end;

procedure TXMLGroup.SetValue(Index : integer; KeyName: String; Value: Integer);
begin
  SetValue(Index, KeyName, IntToStr(Value));
end;

end.
