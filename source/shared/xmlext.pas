unit XMLExt;

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

  { TXMLGroup }

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
    procedure Reload;
    function Add : integer;
    procedure Delete(Index : integer);
    procedure SetValue(Index : integer; KeyName : String; Value : String); overload;
    procedure SetValue(Index : integer; KeyName : String; Value : Integer); overload;
  published
  end;

implementation

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

end.
