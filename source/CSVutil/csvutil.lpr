program csvutil;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  Pasext, CSVext, CPext;


const
  CSVfileExt  = '.csv';
  UTF8fileExt = '.UTF-8';
  MasterCSV   = 'master' + CSVfileExt;
  ListingCSV  = 'listing' + CSVfileExt + UTF8fileExt;
  MixedCSV    = 'mixed' + CSVfileext + UTF8fileExt;

type

  { TCSVutility }

  TCSVutility = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function OpenCSV(FileName : String) : TCSV; virtual;
    procedure RequiredFields(var CSV : TCSV; NewField : boolean = false); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure CreateMasterCSV; virtual;
    procedure CreateLanguageCSV; virtual; overload;
    procedure CreateLanguageCSV(Language : string); virtual; overload;
    procedure DualVersions(FileName, AClass : String); virtual;
  end;

{ TCSVutility }

procedure TCSVutility.DoRun;
begin
  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if CodePageFilePath = AppExecPath then begin
    CodePageFilePath:=
      IncludeTrailingPathDelimiter('..') +
      IncludeTrailingPathDelimiter('fd-nls')+
      IncludeTrailingPathDelimiter('codepages');
    if not DirectoryExists(CodepageFilePath) then begin
      WriteLn('unable to locate code page mapping files');
      Terminate;
      exit;
    end;
  end;
  { add your program here }
  CreateMasterCSV;
  CreateLanguageCSV;

  // stop program loop
  Terminate;
end;

function TCSVutility.OpenCSV(FileName: String): TCSV;
var
  C : integer;
begin
  OpenCSV:=TCSV.Create;
  if not OpenCSV.LoadFromFile(FileName) = 0 then begin
    WriteLn('error opening ', FileName, ' file.');
    FreeAndNil(OpenCSV);
    exit;
  end;
  if OpenCSV.Malformed then begin
    WriteLn('csv file ', FileName, ' is malformed.');
    FreeAndNil(OpenCSV);
    exit;
  end;
  C := OpenCSV.FindColumn('id');
  if C<>-1 then Exit;
  C := OpenCSV.FindColumn('package');
  if C=-1 then begin
    WriteLn('csv file ', FileName, ' has no package identifier column.');
    FreeAndNil(OpenCSV);
    exit;
  end;
  OpenCSV.Header[C]:='id';
end;

procedure TCSVutility.RequiredFields(var CSV: TCSV; NewField: boolean);

  procedure AddField(FN: String);
  begin
    if CSV.FindColumn(FN) = -1 then
      CSV.AddColumn(FN);
  end;

begin
  AddField('id');
  AddField('title');
  AddField('description');
  AddField('summary');
  AddField('keywords');
  AddField('platforms');
  AddField('copying-policy');
  if NewField and (CSV.FindColumn('new') = -1) then begin
    CSV.InsertColumn('new', 1);
  end;
end;

constructor TCSVutility.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCSVutility.Destroy;
begin
  inherited Destroy;
end;

procedure TCSVutility.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExtractFileName(ExeName), ' -h');
end;

procedure TCSVutility.CreateMasterCSV;
var
  MC, TC : TCSV;
  D : TArrayOfStrings;
  I : integer;
begin
  WriteLn('creating ', MasterCSV);
  DeleteFile(MasterCSV);
  MC:=TCSV.Create;
  RequiredFields(MC);
  DirScanByDate(D, DirWildCard + CSVfileExt);
  for I := 0 to Length(D) - 1 do begin
    if LowerCase(D[I]) = MasterCSV then continue;
    TC:=OpenCSV(D[I]);
    if Assigned(TC) then begin
      WriteLn('merging ', TC.RowCount, ' items from ', D[I], ' into ', MasterCSV);
      if not MC.MergeFrom(TC, [mfColumnLock]) then
        WriteLn('merge failed.');
      FreeAndNil(TC);
    end;
  end;
  WriteLn('saving ', MC.RowCount, ' items as ', MasterCSV);
  // MC.Sort;
  MC.SaveAsFile(MasterCSV);
  MC.Free;
  WriteLn;
end;

procedure TCSVutility.CreateLanguageCSV;
var
  D : TArrayOfStrings;
  I : integer;
begin
  WriteLn('Update individual language translations');
  DirScan(D, DirWildCard, false, dsDirsOnly);
  for I := 0 to High(D) do
    if FileExists(IncludeTrailingPathDelimiter(D[I]) + ListingCSV) or
    FileExists(IncludeTrailingPathDelimiter(D[I]) + 'listing.csv') then
      CreateLanguageCSV(D[I]);
end;

procedure TCSVutility.CreateLanguageCSV(Language: string);
var
  MC, LC : TCSV;
  I, C, N : integer;
  L : String;
begin
  L := IncludeTrailingPathDelimiter(Language) + ListingCSV;
  // WriteLn(L);
  DualVersions(L, 'listing');
  if FileExists(L) then begin
    // Load existing language translations
    LC:=OpenCSV(L);
    if not Assigned(LC) then exit;
    // WriteLn('update ', Language, ' mixed file');
  end else begin
    LC:=TCSV.Create;
    WriteLn('create ', Language, ' mixed file');
  end;
  RequiredFields(LC, True);
  // clear new column
  C:=LC.FindColumn('new');
  for I:=0 to LC.RowCount - 1 do
    LC.Cell[C,I]:='';

  MC:=OpenCSV(MasterCSV);
  if not Assigned(MC) then begin
    LC.Free;
    Exit;
  end;
  RequiredFields(MC, True);
  // set new column
  C:=MC.FindColumn('new');
  for I:=0 to MC.RowCount - 1 do
    MC.Cell[C,I]:='*';

  // merge csvs, no overwrite existing
  C:=LC.FindColumn('id');
  if not LC.MergeFrom(MC,[],C) then begin
    WriteLn('CSV merge failure');
    LC.Free;
    MC.Free;
    Exit;
  end;

  // Create path for language if needed
  if not DirectoryExists(Language) then
    if not CreateDir(Language) then begin
       WriteLn('unable to create directory ', Language);
       Exit;
    end;

  // Count new
  C:=LC.FindColumn('new');
  N:=0;
  for I := 0 to LC.RowCount - 1 do
    if LC.Cell[C, I] <> '' then Inc(N);
  // save new translation file
  L := IncludeTrailingPathDelimiter(Language) + MixedCSV;
  if LC.SaveAsFile(L) = 0 then begin
    WriteLn('saved ', Language, ' translations, with ', N, ' new item',
    WhenTrue(N=1, '', 's'));
    DualVersions(L, 'mixed');
  end;
  LC.Free;
  // WriteLn;
end;

procedure TCSVutility.DualVersions(FileName, AClass: String);
var
  S : TStringList;
  L, FileNameCP : String;
  CP, ACP, AUTF : longint;
  I : integer;
begin
  L:=ExcludeTrailingPathDelimiter(ExtractFilePath(FileName));
  FileNameCP:=ExtractFilePath(FileName)+ExtractFileBase(FileName);
//  WriteLn(FileName);
//  WriteLn(FileNameCP);
  if FileExists(FileName) and FileExists(FileNameCP) then begin
    ACP:=FileAge(FileNameCP);
    AUTF:=FileAge(FileName);
    if ACP = AUTF then Exit;
    if ACP < AUTF then begin
      if AClass <> 'mixed' then
        WriteLn('code page version of ', AClass, ' ', L, ' translations is out of date.');
      if not DeleteFile(FileNameCP) then begin
        WriteLn('could not delete ', FileNameCP);
        Halt(1);
      end;
    end else begin
      if AClass <> 'mixed' then
        WriteLn('UTF-8 version of ', AClass, ' ', L, ' translations is out of date.');
      if not DeleteFile(FileName) then begin
        WriteLn('could not delete ', FileName, '. Abort!');
        Halt(1);
      end;
    end;
  end;

  CP:= CodePage(L);

  if FileExists(FileName) and (not FileExists(FileNameCP)) then begin
    if CP = -1 then begin
      WriteLn('unable to map ', L, ' to code page.');
      Exit;
    end;
    if AClass <> 'mixed' then
      WriteLn('create code page version of ', AClass, ' ', L, ' translations from UTF-8 version.');
     AUTF:=FileAge(FileName);
     S:=TStringList.Create;
     S.LoadFromFile(FileName);
     for I := 0 to S.Count - 1 do
        S[I]:=UTF8toCP(S[I],CP);
     S.SaveToFile(FileNameCP);
     S.Free;
     FileSetDate(FileNameCP, AUTF);
  end;
  if FileExists(FileNameCP) and (not FileExists(FileName)) then begin
    if CP = -1 then begin
      WriteLn('unable to map ', L, ' to UTF-8.');
      Exit;
    end;
    if AClass <> 'mixed' then
      WriteLn('create UTF-8 version of ', AClass, ' ', L, ' translations from code page version.');
    AUTF:=FileAge(FileNameCP);
    S:=TStringList.Create;
    S.LoadFromFile(FileNameCP);
    for I := 0 to S.Count - 1 do
       S[I]:=CPtoUTF8(S[I],CP);
    S.SaveToFile(FileName);
    S.Free;
    FileSetDate(FileName, AUTF);
  end;
end;

var
  Application: TCSVutility;
begin
  Application:=TCSVutility.Create(nil);
  Application.Title:='Package CSV Utility';
  Application.Run;
  Application.Free;
end.

