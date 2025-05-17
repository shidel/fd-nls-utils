// Copyright 2025, Jerome Shidel
// BSD 3-Clause License

unit CSVExt;

interface

uses
  Classes, SysUtils;

type
  TCSVMerge = set of (mfOverwrite,mfMustExist,mfColumnLock);

  // Sure, this could be done with a two dimensional array. But overall, this
  // will be easier to interact with as a "thing".

  { TCSV_Column }

  TCSV_Column = class
  private
    FHeader: String;
    FRows : TStringList;
    function GetCell(Row: Integer): String;
    procedure SetCell(Row: Integer; AValue: String);
    procedure SetHeader(AValue: String);
    procedure CheckIndex(Row : integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Header : String read FHeader write SetHeader;
    property Cell[Row: Integer] : String read GetCell write SetCell;
  published

  end;

  { TCSV }

  TCSV = class
  private
    FDelimiter: String;
    FColumns: array of TCSV_Column;
    FFilename: String;
    FLineEnding: String;
    FMalformed: boolean;
    function GetCell(Column, Row: Integer): String;
    function GetColumnCount: integer;
    function GetHeader(Column: Integer): String;
    function GetRowCount: integer;
    procedure SetCell(Column, Row: Integer; AValue: String);
    procedure SetFilename(AValue: String);
    procedure SetHeader(Column: Integer; AValue: String);
    procedure SetDelimiter(AValue: String);
    procedure CheckIndex(Column:Integer);
    procedure SetLineEnding(AValue: String);
    procedure SetMalformed(AValue: boolean);
  protected
    function EncodeCSV(AValue : String) : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    function AsString : String;
    function SaveAsFile(FileName:String):integer; overload;
    function SaveToFile:integer; overload;
    function LoadFromFile(FileName:String): integer; overload;
    function LoadFromFile:integer; overload;
    property Malformed : boolean read FMalformed write SetMalformed;
    property Filename : String read FFilename write SetFilename;
    property Delimiter : String read FDelimiter write SetDelimiter;
    property LineEnding : String read FLineEnding write SetLineEnding;
    function AddColumn(AHeader:String):Integer; virtual;
    function AddRow : Integer; virtual;
    property ColumnCount : integer read GetColumnCount;
    property RowCount : integer read GetRowCount;
    property Header[Column: Integer] : String read GetHeader write SetHeader;
    property Cell[Column, Row: Integer] : String read GetCell write SetCell;
    function FindColumn(ASearch : String; CaseSpecific : boolean = false) : integer;
    function FindRow(ASearch : String; AColumn : integer = 0; CaseSpecific : boolean = false) : integer;
    procedure Sort(AColumn : integer = 0; Ascending : boolean = true);
    function Duplicate : TCSV;
    function MergeFrom(var CSV : TCSV; Flags : TCSVMerge = []; CtrlColumn : integer = 0) : boolean;
    procedure InsertColumn(AHeader : String; APos : integer);
    procedure DeleteColumn(AColumn : integer);
  published
  end;

implementation

uses PasExt;

{ TCSV }

procedure TCSV.SetDelimiter(AValue: String);
begin
  if FDelimiter=AValue then Exit;
  FDelimiter:=AValue;
end;

procedure TCSV.CheckIndex(Column: Integer);
begin
  if (Column<0) or (Column>High(FColumns)) then
     raise Exception.Create('column index ' + IntToStr(Column)
     + ' is out of range in CSV.');

end;

procedure TCSV.SetLineEnding(AValue: String);
begin
  if FLineEnding=AValue then Exit;
  FLineEnding:=AValue;
end;

procedure TCSV.SetMalformed(AValue: boolean);
begin
  if FMalformed=AValue then Exit;
  FMalformed:=AValue;
end;

function TCSV.EncodeCSV(AValue: String): String;
var
  Q : boolean;
begin
  Q := AValue.Contains(QUOTEDOUBLE);
  if Q then AValue:=StringReplace(AValue, QUOTEDOUBLE, QUOTEDOUBLE+QUOTEDOUBLE, [rfReplaceAll]);
  if Q or AValue.Contains(FDelimiter) or AValue.Contains(CR) or AValue.Contains(LF) then
    EncodeCSV:=QUOTEDOUBLE + AValue + QUOTEDOUBLE
  else
    EncodeCSV:=AValue;
end;

function TCSV.GetCell(Column, Row: Integer): String;
begin
  CheckIndex(Column);
  GetCell:=FColumns[Column].FRows[Row];
end;

function TCSV.GetColumnCount: integer;
begin
  GetColumnCount:=Length(FColumns);
end;

function TCSV.GetHeader(Column: Integer): String;
begin
  CheckIndex(Column);
  GetHeader:=FColumns[Column].Header;
end;

function TCSV.GetRowCount: integer;
begin
  if Length(FColumns) > 0 then
    GetRowCount:=FColumns[0].FRows.Count
  else
    GetRowCount:=0;
end;

procedure TCSV.SetCell(Column, Row: Integer; AValue: String);
begin
  CheckIndex(Column);
  FColumns[Column].FRows[Row]:=AValue;
end;

procedure TCSV.SetFilename(AValue: String);
begin
  if FFilename=AValue then Exit;
  LoadFromFile(AValue);
end;

procedure TCSV.SetHeader(Column: Integer; AValue: String);
begin
  CheckIndex(Column);
  FColumns[Column].Header:=AValue;
end;

constructor TCSV.Create;
begin
  inherited Create;
  FLineEnding:=CRLF;
  FColumns:=[];
  Clear;
end;

destructor TCSV.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCSV.Clear;
var
  I : integer;
begin
  for I := Low(FColumns) to High(FColumns) do
    FColumns[I].Free;
  SetLength(FColumns, 0);
  FDelimiter:=COMMA;
  FFileName:='';
  FMalformed:=False;
end;

function TCSV.AsString: String;
var
  I, R : integer;
  L : String;
begin
  AsString:='';
  if Length(FColumns) = 0 then Exit;
  for I := 0 to High(FColumns) do
    if I < High(FColumns) then
      AsString := AsString + EncodeCSV(FColumns[I].Header) + FDelimiter
    else
      AsString := AsString + EncodeCSV(FColumns[I].Header) + LineEnding;
  for R := 0 to FColumns[0].FRows.Count - 1 do begin
     L := '';
     for I := 0 to High(FColumns) do begin
       L := L + EncodeCSV(Cell[I, R]);
       if I < High(FColumns) then
         L := L + FDelimiter;
     end;
     AsString:=AsString+L+LineEnding;
  end;

end;

function TCSV.SaveAsFile(FileName: String): integer;
begin
  if Filename = '' then
    Raise Exception.Create('cannot save CSV without a file name.');
  SaveAsFile:=PasExt.SaveToFile(FileName,AsString);
end;

function TCSV.SaveToFile: integer;
begin
  SaveToFile:=SaveAsFile(FFileName);
end;

function TCSV.LoadFromFile(FileName: String): integer;
var
  RawData : String;
  MLD : boolean;   // Not the end of the line
  SP, RN, CN : integer; // Start index, Row and Column counters

  function NextValue : String;
  var
    EP : integer;
  begin
    MLD:=False;
    NextValue:='';
    if SP > Length(RawData) then Exit;

    if RawData[SP] = QUOTEDOUBLE then begin
      Inc(SP);
      EP := SP;
      repeat
        if (EP < Length(RawData)) and ((RawData[EP] = QUOTEDOUBLE) and
        (RawData[EP+1] = QUOTEDOUBLE)) then
          Inc(EP,2)
        else if (RawData[EP] = QUOTEDOUBLE) then
          break
        else
          Inc(EP);
      until (EP > Length(RawData));
      NextValue:=StringReplace(Copy(RawData,SP,EP-SP), QUOTEDOUBLE + QUOTEDOUBLE, QUOTEDOUBLE, [rfReplaceAll]);
      Inc(EP);
    end else begin
      EP := SP;
      while (EP <= Length(RawData)) and
      (RawData[EP] <> COMMA) and
      (RawData[EP] <> CR) and (RawData[EP] <> LF) do begin
        Inc(EP);
      end;
      NextValue:=Copy(RawData,SP,EP-SP);
    end;

    MLD := (EP <= Length(RawData)) and (RawData[EP] = COMMA);
    if MLD then Inc(EP);
    while (EP <= Length(RawData)) and
    ((RawData[EP] = CR) or (RawData[EP] = LF)) do begin
      MLD:=False;
      Inc(EP);
    end;
    // WriteLn('[', CN, '/', RN, ']', MLD, ':', SP, '/', EP, ':', NextValue);
    SP:=EP;
  end;

begin
  Clear;
  LoadFromFile:=-1;
  SP:=1;
  FFileName:=FileName;
  if PasExt.LoadFromFile(FileName, RawData, false) <> 0 then Exit;
  LoadFromFile:=0;
  FMalformed:=True;
//  RN:=-1;
//  CN:=-1;
  repeat
    AddColumn(NextValue);
  until not MLD;
  if ColumnCount = 0 then Exit;
  RN:=0;
  CN:=0;
  FMalformed:=False;
  while SP <= Length(RawData) do begin
    if RowCount <= RN then AddRow;
    if ColumnCount <= CN then begin
      AddColumn('');
      FMalformed:=True;
    end;
    SetCell(CN, RN, NextValue);
    Inc(CN);
    if not MLD then begin
      CN:=0;
      Inc(RN);
    end;
  end;
end;

function TCSV.LoadFromFile: integer;
begin
  LoadFromFile:=LoadFromFile(FFileName);
end;

function TCSV.AddColumn(AHeader: String): Integer;
var
  I : Integer;
begin
  SetLength(FColumns, Length(FColumns) + 1);
  AddColumn:= High(FColumns);
  FColumns[AddColumn]:=TCSV_Column.Create;
  FColumns[AddColumn].Header:=AHeader;
  if AddColumn <> 0 then begin
    for I := 1 to RowCount do
      FColumns[AddColumn].FRows.Add('');
  end;
end;

function TCSV.AddRow: Integer;
var
  I : Integer;
begin
  if Length(FColumns)=0 then
    raise Exception.Create('CSV has no columns');
  for I := 0 to High(FColumns) do
    FColumns[I].FRows.Add('');
  AddRow:=FColumns[0].FRows.Count - 1;
end;

function TCSV.FindColumn(ASearch: String; CaseSpecific: boolean): integer;
var
  I : integer;
begin
  FindColumn:=-1;
  if CaseSpecific then begin
    for I := Low(FColumns) to High(FColumns) do
      if FColumns[I].FHeader = ASearch then begin
        FindColumn:=I;
        Exit;
      end;
  end else begin
    ASearch:=UpperCase(ASearch);
    for I := Low(FColumns) to High(FColumns) do
      if UpperCase(FColumns[I].FHeader) = ASearch then begin
        FindColumn:=I;
        Exit;
      end;
  end;
end;

function TCSV.FindRow(ASearch: String; AColumn: integer; CaseSpecific: boolean
  ): integer;
var
  I : integer;
begin
  FindRow:=-1;
  if Length(FColumns)=0 then
    raise Exception.Create('CSV has no columns');
  if CaseSpecific then begin
    for I := 0 to RowCount - 1 do
      if Cell[AColumn,I] = ASearch then begin
        FindRow:=I;
        Exit;
      end;
  end else begin
    ASearch:=UpperCase(ASearch);
    for I := 0 to RowCount - 1 do
      if UpperCase(Cell[AColumn,I]) = ASearch then begin
        FindRow:=I;
        Exit;
      end;
  end;

end;

procedure TCSV.Sort(AColumn: integer; Ascending: boolean);
var
  H : TCSV;
  I : Integer;
  S : TStringList;
  T : String;

  procedure CopyRow(N : integer);
  var
    X, Y : Integer;
  begin
    N := StrToInt(Copy(S[N],LastPos(';', S[N])+1));
    Y:=AddRow;
    for X := 0 to ColumnCount - 1 do
       Cell[X,Y]:=H.Cell[X,N];
  end;

begin
  CheckIndex(AColumn);
  if RowCount = 0 then exit;
  S:=TStringList.Create;
  S.Sorted:=true;
  for I := 0 to RowCount -1 do begin
    T:=ExcludeLeading('A ', UpperCase(Cell[AColumn,I]));
    T:=ExcludeLeading('AN ', T);
    T:=ExcludeLeading('THE ', T);
    S.Add(T+';'+IntToStr(I));
  end;
  H:=TCSV.Create;
  H.FColumns := FColumns;
  FColumns:=[];
  for I := 0 to H.ColumnCount - 1 do AddColumn(H.Header[I]);
  if Ascending then begin
    for I := 0 to S.Count -1 do CopyRow(I);
  end else begin
    for I := S.Count -1 downto 0 do CopyRow(I);
  end;
  H.Free;
  S.Free;
end;

function TCSV.Duplicate: TCSV;
var
  X, Y, R : integer;
begin
  Duplicate:=TCSV.Create;
  for X := 0 to ColumnCount -1 do
    Duplicate.AddColumn(Header[X]);
  for Y := 0 to RowCount -1 do begin
    R:=Duplicate.AddRow;
    for X := 0 to ColumnCount -1 do
      Duplicate.Cell[X,R]:=Cell[X,Y];
  end;
end;

function TCSV.MergeFrom(var CSV: TCSV; Flags : TCSVMerge; CtrlColumn: integer): boolean;
var
  DC: Array of integer;
  X, Y, R, C : integer;
begin
  MergeFrom:=False;
  C:=CSV.FindColumn(Header[CtrlColumn]);
  if C = -1 then Exit;
  DC:=[];
  SetLength(DC, CSV.ColumnCount);
  for X:=0 to CSV.ColumnCount - 1 do begin
    DC[X]:=FindColumn(CSV.Header[X]);
    if mfColumnLock in Flags then Continue;
    if DC[X] = -1 then
      DC[X]:=AddColumn(CSV.Header[X]);
    // WriteLn(X,':', CSV.Header[X], '>', DC[X],':', Header[DC[X]]);
  end;
  for Y := 0 to CSV.RowCount -1 do begin
    R:= FindRow(CSV.Cell[C,Y], CtrlColumn);
    if R = -1 then begin
      if mfMustExist in Flags then
        Continue;
      R :=AddRow
    end
    else begin
      if not (mfOverwrite in Flags) then
        Continue;
    end;
    for X := 0 to CSV.ColumnCount -1 do begin
      if DC[X] <> -1 then
        Cell[DC[X],R]:=CSV.Cell[X,Y];
    end;
  end;
  MergeFrom:=True;
end;

procedure TCSV.InsertColumn(AHeader: String; APos: integer);
var
  I, RC : Integer;
begin
  CheckIndex(APos);
  RC:=RowCount;
  SetLength(FColumns, Length(FColumns) + 1);
  for I := High(FColumns) downto APos + 1 do
    FColumns[I]:=FColumns[I-1];
  FColumns[APos]:=TCSV_Column.Create;
  FColumns[APos].Header:=AHeader;
  for I := 1 to RC do
    FColumns[APos].FRows.Add('');
end;

procedure TCSV.DeleteColumn(AColumn: integer);
var
  I: Integer;
begin
  CheckIndex(AColumn);
  FreeAndNil(FColumns[AColumn]);
  for I := AColumn to High(FColumns) - 1 do
    FColumns[I]:=FColumns[I+1];
  SetLength(FColumns, Length(FColumns) - 1);
end;

{ TCSV_Column }

procedure TCSV_Column.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
end;

function TCSV_Column.GetCell(Row: Integer): String;
begin
  CheckIndex(Row);
  GetCell:=FRows[Row];
end;

procedure TCSV_Column.SetCell(Row: Integer; AValue: String);
begin
  CheckIndex(Row);
  FRows[Row]:=AValue;
end;

procedure TCSV_Column.CheckIndex(Row: integer);
begin
  if (Row<0) or (Row>=FRows.Count) then
    raise Exception.Create('row index ' + IntToStr(Row) + ' is out of range in '
    + FHeader + ' column of CSV.');
end;

constructor TCSV_Column.Create;
begin
  inherited Create;
  FRows := TStringList.Create;
  Clear;
end;

destructor TCSV_Column.Destroy;
begin
  Clear;
  FRows.Free;
  inherited Destroy;
end;

procedure TCSV_Column.Clear;
begin
  FHeader:='';
  FRows.Clear;
end;

end.
