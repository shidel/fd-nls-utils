// Copyright 2025, Jerome Shidel
// BSD 3-Clause License

// NOTE: This is using a very slow string table. Converting UTF8 or HTML to
// CodePage is extremely slow. I should implement something far better.
// Like a Binary Tree. But for now, this is good enough. I've got other
// stuff to do.

{$DEFINE Faster}  // UTF8 chars from 32 to 123 are assumed identidal to ASCII
                  // major performance boost when going from UTF8 to ASCII.
                  // However, if I get around to implementing a better method,
                  /// then this wont be needed.

unit CPext;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, XMLConf,
  { custom units after this }
  PasExt;

  var
    CodePageFilePath : string;
    CodePageError : integer;   // Cleared on function calls
                               // 1 = Unknown language
                               // 2 = Codepage Not Found Error
                               // 3 = Unmappable Character(s)

  function CPtoUTF8(S : String; CodePage : integer = 437) : String;
  function UTF8toCP(S : String; CodePage : integer = 437) : String;
  function CodePage(Language : String = 'en') : integer; // -1 error

implementation

type

  { TCodePage }

  TCodePage = class
  private
    FID: integer;
    FHTML : TArrayOfStrings;
    FUTF8 : TArrayOfStrings;
    function GetHTML(Index: Integer): String;
    function GetUTF8(Index: Integer): String;
    procedure SetHTML(Index: Integer; AValue: String);
    procedure SetUTF8(Index: Integer; AValue: String);
  public
    constructor Create(AID : integer);
    destructor Destroy; override;
    property ID : integer read FID;
    property HTML[Index: Integer] : String read GetHTML write SetHTML;
    property UTF8[Index: Integer] : String read GetUTF8 write SetUTF8;
    function LookupUTF8(const S : String) : integer;
  published
  end;

  TCodePageArray = array of TCodePage;

  { TCodePages }

  TCodePages = class
  private
    FIndex : integer;
    FCPA : TCodePageArray;
    function FindCP(CodePage : integer) : integer;
    function LoadCP(CodePage: integer) : integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Select(CodePage : integer) : boolean;
    function toUTF8(S : String) : String;
    function fromUTF8(S : String) : String;
    // function toHTML(S :String) : String;
   published

  end;


var
  CodePages : TCodePages;

function CPtoUTF8(S : String; CodePage : integer = 437) : String;
begin
  if not CodePages.Select(CodePage) then begin
    CodePageError:=2;
    CPtoUTF8:=S;
  end else begin
    CodePageError:=0;
    CPtoUTF8:=CodePages.toUTF8(S);
  end;
end;

function UTF8toCP(S: String; CodePage: integer = 437): String;
begin
  if not CodePages.Select(CodePage) then begin
     CodePageError:=2;
     UTF8toCP:=S;
  end else begin
    CodePageError:=0;
    UTF8toCP:=CodePages.fromUTF8(S);
  end;
end;

function CodePage(Language: String): integer;
var
  I, C : integer;
  L, D : String;
begin
  CodePage:=437;
  CodePageError:=0;
  Language:=LowerCase(Language);
  if Language='en' then exit;
  CodePage:=-1;
  C:=-1;
  for I := Low(LanguageCodes) to High(LanguageCodes) do begin
    D:=LowerCase(LanguageCodes[I]);
    While (D <> '') and (C = -1) do begin
      L:=PopDelim(D, COMMA);
      if L = Language then C := I;
    end;
    if C <> -1 then Break;
  end;
  if C=-1 then begin
    CodePageError:=1;
    Exit;
  end;
  D:=LowerCase(LanguageCodes[I]);
  PopDelim(D, COMMA);
  PopDelim(D, COMMA);
  C:=StrToInt(PopDelim(D, COMMA));
  if CodePages.Select(C) then CodePage:=C;
end;

function TCodePage.GetHTML(Index: Integer): String;
begin
  // Index < 0 or > 255 will raise an exception;
  GetHTML:=FHTML[Index];
end;

function TCodePage.GetUTF8(Index: Integer): String;
begin
  // Index < 0 or > 255 will raise an exception;
  GetUTF8:=FUTF8[Index];
end;

procedure TCodePage.SetHTML(Index: Integer; AValue: String);
begin
  // Index < 0 or > 255 will raise an exception;
  FHTML[Index]:=AValue;
end;

procedure TCodePage.SetUTF8(Index: Integer; AValue: String);
begin
  // Index < 0 or > 255 will raise an exception;
  FUTF8[Index]:=AValue;
end;

function TCodePage.LookupUTF8(const S: String): integer;
var
  I : integer;
begin
  LookupUTF8:=-1;
  if Length(S) = 0 then Exit;
  {$IFDEF Faster}
  if (Ord(S[1]) > 31) and (Ord(S[1]) < 124) then begin
    LookupUTF8:= Ord(S[1]);
    Exit;
  end;
  {$ENDIF}
  for I := Low(FUTF8) to High(FUTF8) do
    if FUTF8[I] = Copy(S,1,Length(FUTF8[I])) then begin
      LookupUTF8:=I;
      Break;
    end;
end;

constructor TCodePage.Create(AID: integer);
var
  I : Integer;
begin
  inherited Create;
  FID:=AID;
  SetLength(FHTML, 256); // 0 to 255 = 256 ascii characters
  SetLength(FUTF8, Length(FHTML));
  for I := 0 to Length(FHTML) - 1 do begin
    FHTML[I]:=Char(I);
    FUTF8[I]:=Char(I);
  end;
end;

destructor TCodePage.Destroy;
begin
  inherited Destroy;
end;

{ TCodePages }

function TCodePages.FindCP(CodePage: integer): integer;
var
  I : integer;
begin
  for I := 0 to Length(FCPA) - 1 do
    if FCPA[I].ID = CodePage then begin
      FindCP := I;
      Exit;
    end;
  FindCP := -1;
end;

function TCodePages.LoadCP(CodePage: integer): integer;
var
  CPD : TCodePage;
  XML : TXMLConfig;
  I   : integer;
  T   : String;
begin
  LoadCP:=-1;
  if not FileExists(CodePageFilePath + IntToStr(CodePage) + '.xml') then Exit;
  XML:=TXMLConfig.Create(nil);
  try
    XML.LoadFromFile(CodePageFilePath + IntToStr(CodePage) + '.xml');
  except
    XML.Free;
    Exit;
  end;
  I:=XML.GetValue('CODEPAGE/IDENTIFIER',-1);
  if (I <> CodePage) then begin
    XML.Free;
    Exit;
  end;
  CPD:=TCodePage.Create(CodePage);
  SetLength(FCPA, Length(FCPA)+1);
  FCPA[Length(FCPA)-1]:=CPD;
  {$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
  {$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
  { not an issue with how it is being used }
  for I := 0 to 255 do begin
    T:=XML.GetValue('CODEPAGE/ASCII_' + IntToStr(I) + '/UTF8', '');
    if T <> '' then CPD.FUTF8[I]:=IntsToStr(T);
    T:=XML.GetValue('CODEPAGE/ASCII_' + IntToStr(I) + '/HTML', '');
    if T <> '' then CPD.HTML[I]:=IntsToStr(T);
  end;
  {$warn 4105 on}
  {$warn 4104 on}
  XML.Free;
  LoadCP:=Length(FCPA) - 1;
end;

constructor TCodePages.Create;
begin
  inherited Create;
  FIndex:=-1;
  FCPA:=[];
  Select(437);
end;

destructor TCodePages.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodePages.Clear;
var
  I : integer;
begin
  for I := 0 to Length(FCPA) - 1 do
    FreeAndNil(FCPA[I]);
  FCPA:=[];
  FIndex:=-1;
end;

function TCodePages.Select(CodePage: integer): boolean;
var
  I : Integer;
begin
  if FIndex <> -1 then
    if FCPA[FIndex].ID = CodePage then begin
      Select:=True;
      Exit;
    end;
  I := FindCP(CodePage);
  if I = -1 then
    I := LoadCP(CodePage);
  if I <> -1 then
    FIndex := I;
  Select:= I <> -1;
end;

function TCodePages.toUTF8(S: String): String;
var
  I : Integer;
begin
  if FIndex = -1 then begin
    toUTF8:=S;
    Exit;
  end;
  toUTF8:='';
  for I := 1 to Length(S) do
     toUTF8:=toUTF8+FCPA[FIndex].UTF8[Ord(S[I])];
end;

function TCodePages.fromUTF8(S: String): String;
var
  I, C : Integer;
begin
  if FIndex = -1 then begin
    fromUTF8:=S;
    Exit;
  end;
  fromUTF8:='';
  I:=1;
  while I <= Length(S) do begin
    C:=FCPA[FIndex].LookupUTF8(Copy(S, I, 4));
    if C=-1 then begin
      CodePageError:=3;
      Inc(I);
      fromUTF8:=fromUTF8+#$a8;
    end else begin
      fromUTF8:=fromUTF8+Char(C);
      Inc(I, Length(FCPA[FIndex].UTF8[C]));
    end;
  end;
end;

initialization

  CodePageFilePath := IncludeTrailingPathDelimiter(AppExecPath + 'codepages');
  if Not DirectoryExists(CodePageFilePath) then
    CodePageFilePath := IncludeTrailingPathDelimiter(AppExecPath + 'codepage');
  if Not DirectoryExists(CodePageFilePath) then
    CodePageFilePath := IncludeTrailingPathDelimiter(AppExecPath);
  CodePageError:= 0;
  CodePages :=TCodePages.Create;

finalization

  CodePages.Free;

end.
