unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, PUIExt, FDKit, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls, Menus, ActnList, ComCtrls, ExtCtrls, Buttons,
  XMLConf, LCLType, LCLIntf, EditBtn, IpHtml, Ipfilebroker, opensslsockets,
  fphttpclient, DateUtils, uAppNLS, uLog, Icons;


type

  { TmForm }

  TmForm = class(TForm)
    actAppleAbout: TAction;
    actApplePrefs: TAction;
    actDebugLog: TAction;
    actPackages: TAction;
    actProjects: TAction;
    actSoftwareUpdate: TAction;
    actMenuHelp: TAction;
    actPreferences: TAction;
    actMenuOpts: TAction;
    actMenuFile: TAction;
    alMain: TActionList;
    bbSoftwareUpdate: TButton;
    cbSoftwareUpdate: TComboBox;
    deLocalRepo: TDirectoryEdit;
    ilToolsSmall: TImageList;
    ilFlagsLarge: TImageList;
    ilFlagsSmall: TImageList;
    imgAbout: TImage;
    hpAbout: TIpHtmlPanel;
    lbAvailLanguages: TLabel;
    lbLocalRepo: TLabel;
    lbSoftwareUpdate: TLabel;
    lvLanguages: TListView;
    mMain: TMainMenu;
    pControlArea: TPanel;
    pSeparatorAbout: TPanel;
    pcPrefs: TPageControl;
    pMain: TPanel;
    pcMain: TPageControl;
    pSeparatorUpper: TPanel;
    pSeperatorLower: TPanel;
    sbMain: TStatusBar;
    sPrefs: TSplitter;
    itMinute: TTimer;
    tsPackages: TTabSheet;
    tsProjects: TTabSheet;
    tbMain: TToolBar;
    tbProjects: TToolButton;
    tbPackages: TToolButton;
    tbDebugLog: TToolButton;
    tbPreferences: TToolButton;
    tsRepo: TTabSheet;
    tsLanguages: TTabSheet;
    tsGeneral: TTabSheet;
    tsAbout: TTabSheet;
    tvPrefs: TTreeView;
    tsPrefs: TTabSheet;
    xConfig: TXMLConfig;
    xProperties: TXMLPropStorage;
    procedure actAppleAboutExecute(Sender: TObject);
    procedure actApplePrefsExecute(Sender: TObject);
    procedure actDebugLogExecute(Sender: TObject);
    procedure actPackagesExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actProjectsExecute(Sender: TObject);
    procedure actSoftwareUpdateExecute(Sender: TObject);
    procedure cbSoftwareUpdateChange(Sender: TObject);
    procedure deLocalRepoAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure hpAboutHotClick(Sender: TObject);
    procedure itMinuteTimer(Sender: TObject);
    procedure tsAboutShow(Sender: TObject);
    procedure tsGeneralShow(Sender: TObject);
    procedure tsLanguagesShow(Sender: TObject);
    procedure tsRepoShow(Sender: TObject);
    procedure tvPrefsChange(Sender: TObject; Node: TTreeNode);
  private
    Repository: TFDNLS;
    function AddMenuItem(ToItem : TMenuItem; ActionItem : TBasicAction) : TMenuItem; overload;
    function AddMenuItem(ToItem : TMenuItem; CaptionText : TCaption) : TMenuItem; overload;
    procedure AddPrefsTree(ParentNode : TTreeNode; Pages : TPageControl);
    procedure SelectPrefsPage(Tab : TTabSheet);
    procedure LoadGlyphResources;
    procedure CreateMainMenu;
    procedure CreatePrefsTree;
    procedure CreateAboutText;
    procedure OpenRepository(Location : String);
    procedure SetAppLanguageText(ALanguage : String);
  public
    procedure SoftwareUpdate(Silent : boolean);
  end;

var
  mForm: TmForm;

implementation

{$I version.inc}

{$R *.lfm}

{ TmForm }

procedure TmForm.FormCreate(Sender: TObject);
begin
   Repository := TFDNLS.Create;
   // Hide some design time elements
   pcMain.ShowTabs := False;
   pcPrefs.ShowTabs := False;
   // set Program configuration file
   xConfig.Filename:= AppCfgPath + 'userdata.xml';
   // configure local repository
   OpenRepository(xConfig.GetValue('REPOSITORY/LOCAL/PATH',  ''));
   SetAppLanguageText(xConfig.GetValue('LANGUAGE/USER/NATIVE',  'EN'));
   // Set display config files
   xProperties.FileName := AppCfgFile;
   xProperties.RootNodePath := FormNodePath(Self);
   // Populate UI elements
   LoadGlyphResources;
   CreateMainMenu;
   CreatePrefsTree;
   CreateAboutText;
   // Config verification
   if xConfig.GetValue('VERSION/ABOUT/REVISION',  '') <> SOURCE_REVISION then
      SelectPrefsPage(tsAbout)
   else if not DirectoryExists(xConfig.GetValue('REPOSITORY/LOCAL/PATH', '')) then
      SelectPrefsPage(tsRepo);
end;

procedure TmForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Repository);
end;

procedure TmForm.hpAboutHotClick(Sender: TObject);
begin
  Log(Self, 'About URL click "' +  hpAbout.HotURL + '"');
  OpenURL(hpAbout.HotURL);
end;

procedure TmForm.itMinuteTimer(Sender: TObject);
begin
   // Log(Self, 'Minute Interval Trigger');
  itMinute.Interval := 60 * 1000; { 60 second intervals }
  mForm.SoftwareUpdate(True);
end;

procedure TmForm.tsAboutShow(Sender: TObject);
begin
  xConfig.SetValue('VERSION/ABOUT/REVISION', SOURCE_REVISION);
end;

procedure TmForm.tsGeneralShow(Sender: TObject);
begin
  cbSoftwareUpdate.ItemIndex:= xConfig.GetValue('SOFTWARE/UPDATE/INERVAL', 4);
end;

procedure TmForm.tsLanguagesShow(Sender: TObject);
var
  I : integer;
  LI : TListItem;
begin
  lvLanguages.Clear;
  for I := 0 to Repository.Languages.Count - 1 do begin
    LI := lvLanguages.Items.Add;
    LI.Caption:=Repository.Languages.Captions[I];
  end;
end;

procedure TmForm.tsRepoShow(Sender: TObject);
begin
  deLocalRepo.Directory:=xConfig.GetValue('REPOSITORY/LOCAL/PATH', '');
end;

procedure TmForm.actAppleAboutExecute(Sender: TObject);
begin
   SelectPrefsPage(tsAbout);
end;

procedure TmForm.actApplePrefsExecute(Sender: TObject);
begin
  SelectPrefsPage(tsGeneral);
end;

procedure TmForm.actDebugLogExecute(Sender: TObject);
begin
  if not fLog.Visible then begin
    fLog.Show;
    Self.Show;
  end;
end;

procedure TmForm.actPackagesExecute(Sender: TObject);
begin
  pcMain.ActivePage := tsPackages;
end;

procedure TmForm.actPreferencesExecute(Sender: TObject);
begin
  SelectPrefsPage(tsGeneral);
end;

procedure TmForm.actProjectsExecute(Sender: TObject);
begin
   pcMain.ActivePage := tsProjects;
end;

procedure TmForm.actSoftwareUpdateExecute(Sender: TObject);
begin
  SoftwareUpdate(False);
end;

procedure TmForm.cbSoftwareUpdateChange(Sender: TObject);
begin
  xConfig.SetValue('SOFTWARE/UPDATE/INERVAL', cbSoftwareUpdate.ItemIndex);
  xConfig.Flush;
end;

procedure TmForm.deLocalRepoAcceptDirectory(Sender: TObject; var Value: String);
begin
  OpenRepository(Value);
end;

procedure TmForm.tvPrefsChange(Sender: TObject; Node: TTreeNode);
var
   I : integer;
begin
   for I := 0 to pcPrefs.PageCount - 1 do begin
     if Node.Text = pcPrefs.Pages[I].Caption then begin
       pcPrefs.ActivePage := pcPrefs.Pages[I];
       exit;
     end;
   end;
end;

function TmForm.AddMenuItem(ToItem: TMenuItem; ActionItem: TBasicAction): TMenuItem;
begin
   // Add an item to the main menu based on it's action
   Result := TMenuItem.Create(Self);
   Result.Action := ActionItem;
   if Assigned(ToItem) then
      ToItem.Add(Result)
   else
      mMain.Items.Add(Result);
end;

function TmForm.AddMenuItem(ToItem: TMenuItem; CaptionText: TCaption): TMenuItem;
begin
   // Add an item to the main menu based on it's caption
   Result := TMenuItem.Create(Self);
   Result.Caption := CaptionText;
   if Assigned(ToItem) then
      ToItem.Add(Result)
   else
      mMain.Items.Add(Result);
end;

procedure TmForm.CreateMainMenu;
var
   aMenu   : TMenuitem;
   I, J, G : integer;
   Cat     : String;
begin
   mMain.Items.Clear;
   {$IFDEF MacOS}
     // Create Aplication menu for Apple macOS
     aMenu := AddMenuItem(nil, #$EF#$A3#$BF); { Unicode Apple Logo }
     AddMenuItem(aMenu, actAppleAbout);
     AddMenuItem(aMenu, '-');
     AddMenuItem(aMenu, actApplePrefs);
     AddMenuItem(aMenu, '-');
   {$ENDIF}
   // Add ActionList items to Application MainMenu
   for I := 0 to alMain.ActionCount - 1 do begin
      Cat := Uppercase(alMain.Actions[I].Category);
      if Cat <> 'MENU' then continue;
      aMenu := AddMenuItem(nil, alMain.Actions[I]);
      if not (alMain.Actions[I] is TAction) then Continue;
      G := TAction(alMain.Actions[I]).GroupIndex;
      for J := I + 1 to alMain.ActionCount - 1 do begin
        if not (alMain.Actions[J] is TAction) then Continue;
        if G = TAction(alMain.Actions[J]).GroupIndex then
           AddMenuItem(aMenu, alMain.Actions[J]);
      end;
   end;
end;

procedure TmForm.AddPrefsTree(ParentNode: TTreeNode; Pages: TPageControl);
var
   I : integer;
begin
   // Add the pages of an option to a node inthe preferences tree
  for I := 0 to Pages.PageCount - 1 do begin
    tvPrefs.Items.AddChild(ParentNode, Pages.Pages[I].Caption);
  end;
end;

procedure TmForm.SelectPrefsPage(Tab: TTabSheet);
var
   N : TTreeNode;
begin
  N := tvPrefs.Items.GetFirstNode;
  while Assigned(N) do begin
    if N.Text = Tab.Caption then begin
      N.Selected:=True;
      Break;
    end;
    N := N.GetNext;
  end;
  pcMain.ActivePage := tsPrefs;
end;

procedure TmForm.LoadGlyphResources;
var
   I : Integer;
begin
  for I := Low(IconUI) to High(IconUI) do
      ilToolsSmall.AddLazarusResource(IconUI[I]);
  for I := Low(IconFlags) to High(IconFlags) do
      ilFlagsLarge.AddLazarusResource(IconFlags[I]);
  for I := Low(IconFlags) to High(IconFlags) do
      ilFlagsSmall.AddLazarusResource(IconFlags[I]);
end;

procedure TmForm.CreatePrefsTree;
begin
   // Clear the prefeences tree and populate the root pages
   tvPrefs.Items.Clear;
   AddPrefsTree(nil, pcPrefs);
   tvPrefs.FullExpand;
   tvPrefs.Items.GetFirstNode.Selected:=True;
end;

procedure TmForm.CreateAboutText;
var
  S: TStringStream;
  H: TIpHtml;
begin
  try
    S := TStringStream.Create(
      '<html><body><center>' +
      '<b>' + APP_FILEDESCRIPTION + '</b><br>' +
      Format(msg_AboutVersion, [APP_VERSION, SOURCE_REVISION]) + '<br>' +
      msg_AboutLicense + '<br>' +
      Format(msg_AboutCopyright, [APP_LEGALCOPYRIGHT]) + '<br>' +
      '<br>' +
      Format(msg_AboutIconCopyright, [IconProviderCopyright,
       '(<a href="' + IconProviderURL + '">' + IconProviderUrl + '</a>)<br>',
       '<a href="' + IconCollectionURL + '">' + IconCollectionName + '</a>']) + '<br>' +
       '<br>' +
       Format(msg_AboutCreatedWith, ['<a href="https://www.lazarus-ide.org/">Lazarus IDE</a>',
        '<a href="https://www.freepascal.org/">the Free Pascal Compiler</a>']) + '<br>' +
      '</center></body></html>'
    );
    try
      H:=TIpHtml.Create;
      H.LoadFromStream(S);
    finally
      S.Free;
    end;
    hpAbout.SetHtml(H);
    // TIpHtml is automatically freed by TIpHtmlPanel
  finally
  end;
end;

procedure TmForm.OpenRepository(Location : String);
begin
    if Location <> Repository.Path then begin
      Repository.Path:= Location;
      xConfig.SetValue('REPOSITORY/LOCAL/PATH', Repository.Path);
      xConfig.Flush;
    end;
    Log(Self, 'Open local repository ' + Location);
end;

procedure TmForm.SetAppLanguageText(ALanguage: String);
begin
  if ALanguage <> xConfig.GetValue('LANGUAGE/USER/NATIVE',  '') then begin
    xConfig.SetValue('LANGUAGE/USER/NATIVE',  ALanguage);
    xConfig.Flush;
  end;
  lbAvailLanguages.Caption:=msg_lbAvailLanguages;
  lbLocalRepo.Caption:=msg_lbLocalRepository;
  lbSoftwareUpdate.Caption:=msg_lbSoftwareUpdate;
end;

procedure TmForm.SoftwareUpdate(Silent: boolean);
const
  DTFmt = 'yyyy-mm-dd hh:mm:ss';
var
  Query, R : String;
  I : LongInt;
  SS: TStringStream;
  SL : TStringList;
  Client: TFPHTTPClient;
  DT, LT : TDateTime;
begin
  DT := Now;
  if Silent then begin
     try
       LT := ScanDateTime(DTFmt, xConfig.GetValue('SOFTWARE/UPDATE/CHECKED', 'failed'));
       case xConfig.GetValue('SOFTWARE/UPDATE/INERVAL', 4) of
         0 : { disabled } exit;
         1 : { monthly } begin
           if MonthOf(DT) = MonthOf(LT) then Exit;
           if WeeksBetween(DT, LT) < 3 then Exit;
         end;
         2 : { weekly } begin
           if WeekOf(DT) = WeekOf(LT) then Exit;
           if DaysBetween(DT, LT) < 5 then Exit;
         end;
         3 : { daily } begin
           if DayOf(DT) = DayOf(LT) then Exit;
           if HoursBetween(DT, LT) < 18 then Exit;
         end;
         4 : { hourly } begin
           if HourOf(DT) = HourOf(LT) then Exit;
           if MinutesBetween(DT, LT) < 45 then Exit;
         end;
       end;
     except
       Log(Self, 'no previous software update check data');
       { ignore errors / assume needs checked }
     end;
  end;
  xConfig.SetValue('SOFTWARE/UPDATE/CHECKED', FormatDateTime(DTFmt, DT));
  XConfig.Flush;
  Query := UpdateServer +
    StringReplace(APP_PRODUCTNAME, '-', '', [rfReplaceAll]) +
    '_' + PlatformID + '/' + APP_VERSION;
  Log(Self, 'Check for update to ' + APP_PRODUCTNAME + ' v' + APP_VERSION);
  Log(Self, 'Update URL ' + Query);
  SS := TStringStream.Create('');
  SL := TStringList.Create;
  try
    Client := TFPHTTPClient.Create(nil);
    try
      try
        Client.Get(Query, SS);
        R := SubStr(SS.DataString, '<meta name="update-server"', '/>', False);
        if (R = '') then begin
          if Pos('NO DOWNLOADS', UpperCase(SS.DataString)) > 0 then begin
            // Unknown to server
            Log(Self, APP_PRODUCTNAME + ' is not known to server.');
            if not Silent then
              MessageDlg(msg_NoAppUpdatesAvail, mtConfirmation, [mbOk], 0);
          end else begin
            // No update at present
             Log(Self, APP_PRODUCTNAME + ' is latest version.');
             if not Silent then
               MessageDlg(msg_UsingLatestAppVersion, mtConfirmation, [mbOk], 0);
          end;
        end else begin
           // Update is available
           Explode(R, SL, True);
           for I := 0 to SL.Count - 1 do
             if HasLeading('data-', SL.Strings[I], false) then
               Log(Self, '  ' + SL.Strings[I]);
           Log(Self, 'Download available: ' +
             LookupValue('data-application', SL) +
             ' v' + LookupValue('data-version', SL) +
             ', ' + LookupValue('data-bytes', SL) + ' bytes');
           if MessageDlg(Format(msg_UpdateAvailable, [
             LookupValue('data-application', SL),
             LookupValue('data-version', SL),
             LookupValue('data-bytes', SL)
             ]), mtConfirmation, [mbYes,mbNo], 0) = mrYes then begin
               OpenURL(LookupValue('data-link', SL));
               OpenURL(Query);
             end;
        end;
      except
        on E: Exception do begin
           Log(Self, E.Message);
           if not Silent then
              MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    finally
      FreeAndNil(Client);
    end;
  finally
    SS.Free;
    SL.Free;
  end;
end;

end.

