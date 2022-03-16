unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, PUIExt, FDKit, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls, Menus, ActnList, ComCtrls, ExtCtrls, Buttons,
  XMLConf, LCLType, LCLIntf, EditBtn, IpHtml, Ipfilebroker, ClassExt,
  opensslsockets, fphttpclient, DateUtils,
  uAppNLS, uLog, uPickFlag, uEditCodePage, uPkgListEdit,
  Icons;

type

  { TfMain }

  TfMain = class(TForm)
    actAppleAbout: TAction;
    actApplePrefs: TAction;
    actDebugLog: TAction;
    actAddLanguage: TAction;
    actCodepageNew: TAction;
    actCodepageEdit: TAction;
    actCodepageNone: TAction;
    actRemoveLanguage: TAction;
    actPackages: TAction;
    actProjects: TAction;
    actSoftwareUpdate: TAction;
    actMenuHelp: TAction;
    actPreferences: TAction;
    actMenuOpts: TAction;
    actMenuFile: TAction;
    alMain: TActionList;
    bbSoftwareUpdate: TButton;
    bbRemoveLanguage: TBitBtn;
    cbSoftwareUpdate: TComboBox;
    deLocalRepo: TDirectoryEdit;
    frPkgListEdit: TframePkgListEdit;
    iLangNameVerify: TImage;
    iLangIdVerify: TImage;
    ilToolsMedium: TImageList;
    ilFlagsLarge: TImageList;
    ilFlagsSmall: TImageList;
    ilToolsSmall: TImageList;
    iLangDosVerify: TImage;
    lbComingSoon: TLabel;
    lbUpdateChecked: TLabel;
    lbGitRepo: TLabel;
    leGraphic: TImage;
    imgAbout: TImage;
    hpAbout: TIpHtmlPanel;
    leLangName: TLabeledEdit;
    leLangID: TLabeledEdit;
    leLangDOS: TLabeledEdit;
    leLangCodePage: TLabeledEdit;
    lbAvailLanguages: TLabel;
    lbLocalRepo: TLabel;
    lbSoftwareUpdate: TLabel;
    lvLanguages: TListView;
    mMain: TMainMenu;
    pLangEditRight: TPanel;
    pLangEditLeft: TPanel;
    pLanguagesList: TPanel;
    pLanguages: TPanel;
    pControlArea: TPanel;
    pSeparatorAbout: TPanel;
    pcPrefs: TPageControl;
    pMain: TPanel;
    pcMain: TPageControl;
    pSeparatorUpper: TPanel;
    pSeperatorLower: TPanel;
    sbMain: TStatusBar;
    sbLanguageEdit: TScrollBox;
    sLanguages: TSplitter;
    btAddLanguage: TSpeedButton;
    btCodepage: TSpeedButton;
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
    procedure actAddLanguageExecute(Sender: TObject);
    procedure actAppleAboutExecute(Sender: TObject);
    procedure actApplePrefsExecute(Sender: TObject);
    procedure actCodepageEditExecute(Sender: TObject);
    procedure actCodepageNewExecute(Sender: TObject);
    procedure actDebugLogExecute(Sender: TObject);
    procedure actPackagesExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actProjectsExecute(Sender: TObject);
    procedure actRemoveLanguageExecute(Sender: TObject);
    procedure actRemoveLanguageUpdate(Sender: TObject);
    procedure actSoftwareUpdateExecute(Sender: TObject);
    procedure cbSoftwareUpdateChange(Sender: TObject);
    procedure deLocalRepoAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hpAboutHotClick(Sender: TObject);
    procedure itMinuteTimer(Sender: TObject);
    procedure leGraphicClick(Sender: TObject);
    procedure leLangCodePageChange(Sender: TObject);
    procedure leLangCodePageEditingDone(Sender: TObject);
    procedure leLangDOSChange(Sender: TObject);
    procedure leLangDOSEditingDone(Sender: TObject);
    procedure leLangIDChange(Sender: TObject);
    procedure leLangIDEditingDone(Sender: TObject);
    procedure leLangNameChange(Sender: TObject);
    procedure leLangNameEditingDone(Sender: TObject);
    procedure lvLanguagesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvLanguagesItemChecked(Sender: TObject; Item: TListItem);
    procedure sbLanguageEditResize(Sender: TObject);
    procedure tsAboutShow(Sender: TObject);
    procedure tsGeneralShow(Sender: TObject);
    procedure tsLanguagesShow(Sender: TObject);
    procedure tsPackagesShow(Sender: TObject);
    procedure tsPrefsShow(Sender: TObject);
    procedure tsProjectsShow(Sender: TObject);
    procedure tsRepoShow(Sender: TObject);
    procedure tvPrefsChange(Sender: TObject; Node: TTreeNode);
  private
    function GetActiveLanguage(ALang : String): boolean;
    procedure SetActiveLanguage(ALang : String; AValue: boolean);
  private
    EditLangIndex : integer;
    ReloadNeeded : boolean;
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
    procedure SelectEditLanguage(Index : integer);
    property ActiveLanguage[ALang : String] : boolean read GetActiveLanguage write SetActiveLanguage;
    function LangStatusVerify : boolean;
    function LangIDVerify : boolean;
    function LangNameVerify : boolean;
    function LangDosVerify : boolean;
    function LangCodePageVerify : boolean;
    procedure Reload;
  public
     Repository: TFDNLS;
   procedure SoftwareUpdate(Silent : boolean);
  end;

var
  fMain: TfMain;

implementation

{$I version.inc}

{$R *.lfm}

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
   Log(Self, 'User Language: ' + UserLanguage);
   Repository := TFDNLS.Create;
   // Hide some design time elements
   pcMain.ShowTabs := False;
   pcPrefs.ShowTabs := False;
   // set Program configuration file
   xConfig.Filename:= AppCfgPath + 'userdata.xml';
   // configure local repository
   OpenRepository(GetValueXML(xConfig, 'REPOSITORY/LOCAL/PATH',  ''));
   SetAppLanguageText(GetValueXML(xConfig, 'LANGUAGE/USER/ACTIVE',
     UpperCase(GetEnvironmentVariable('LANG'))));
   // Set display config files
   xProperties.FileName := AppCfgFile;
   xProperties.RootNodePath := DisplayNamePath(Self);
   // Populate and configure UI elements
   LoadGlyphResources;
   CreateMainMenu;
   CreatePrefsTree;
   CreateAboutText;
   // Config verification
   if GetValueXML(xConfig, 'VERSION/ABOUT/REVISION',  '') <> SOURCE_REVISION then
      SelectPrefsPage(tsAbout)
   else if not DirectoryExists(GetValueXML(xConfig, 'REPOSITORY/LOCAL/PATH', '')) then
      SelectPrefsPage(tsRepo);
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Repository);
end;

procedure TfMain.FormHide(Sender: TObject);
begin
  xProperties.WriteInteger(frPkgListEdit.lvPackages.GetNamePath + '/WIDTH',
    frPkgListEdit.lvPackages.Width);
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  frPkgListEdit.lvPackages.Width :=
    xProperties.ReadInteger(frPkgListEdit.lvPackages.GetNamePath + '/WIDTH',
    frPkgListEdit.lvPackages.Width);
end;

procedure TfMain.hpAboutHotClick(Sender: TObject);
begin
  Log(Self, 'About URL click "' +  hpAbout.HotURL + '"');
  OpenURL(hpAbout.HotURL);
end;

procedure TfMain.itMinuteTimer(Sender: TObject);
begin
   // Log(Self, 'Minute Interval Trigger');
  itMinute.Interval := 60 * 1000; { 60 second intervals }
  fMain.SoftwareUpdate(True);
end;

procedure TfMain.leGraphicClick(Sender: TObject);
var
  I : integer;
  S : String;
begin
  fPickFlag.Index := -1;
  S := Repository.Languages.Graphic[EditLangIndex];
  for I := Low(IconFlags) to High(IconFlags) do
      if S = FieldStr(IconFlags[I], 1, -1, '-') then begin
          fPickFlag.Index:=I;
          Break;
      end;
  if fPickFlag.ShowModal = mrOk then begin
     if fPickFLag.Index = -1 then
       Repository.Languages.Graphic[EditLangIndex] := ''
     else
       Repository.Languages.Graphic[EditLangIndex] := FieldStr(IconFlags[fPickFLag.Index], 1, -1, '-');
     SelectEditLanguage(EditLangIndex);
  end;
end;

procedure TfMain.leLangCodePageChange(Sender: TObject);
begin
  if leLangCodePage.Text = '' then begin
    btCodePage.Action := actCodePageNone;
    btCodePage.ImageIndex:=actCodePageNone.ImageIndex;
  end else if Repository.CodePages.IndexOfIdentifier(leLangCodePage.Text) = -1 then begin
    btCodePage.Action := actCodePageNew;
    btCodePage.ImageIndex:=actCodePageNew.ImageIndex;
  end else begin
    btCodePage.Action := actCodePageEdit;
    btCodePage.ImageIndex:=actCodePageEdit.ImageIndex;
  end;
end;

procedure TfMain.leLangCodePageEditingDone(Sender: TObject);
var
  I : integer;
begin
   if EditLangIndex < 0 then exit;
   try
     I := StrtoInt(leLangCodePage.Caption);
   except
     I := -1;
   end;
   Repository.Languages.Codepage[EditLangIndex] := I;
   I := Repository.Languages.Codepage[EditLangIndex];
   if I < 0 then
    leLangCodePage.Caption := ''
   else
    leLangCodePage.Caption := ZeroPad(I, 3);
end;

procedure TfMain.leLangDOSChange(Sender: TObject);
begin
  LangDosVerify;
end;

procedure TfMain.leLangDOSEditingDone(Sender: TObject);
begin
   if EditLangIndex < 0 then exit;
   Repository.Languages.Language[EditLangIndex] := leLangDOS.Caption;
   leLangDOS.Caption := Repository.Languages.Language[EditLangIndex];
end;

procedure TfMain.leLangIDChange(Sender: TObject);
begin
  LangIDVerify;
end;

procedure TfMain.leLangIDEditingDone(Sender: TObject);
var
  S, L, C, N : String;
  I : integer;
begin
   if EditLangIndex < 0 then exit;
   Repository.Languages.Identifier[EditLangIndex] := leLangID.Caption;
   leLangID.Caption := Repository.Languages.Identifier[EditLangIndex];
   if leLangID.Caption <> '' then
     Repository.Languages.FileName[EditLangIndex] := leLangID.Caption + '.xml';
   // Reset/Auto-populate fields
   S := Uppercase(Repository.Languages.Identifier[EditLangIndex]);
   if S <> '' then
     for I := 0 to Length(LanguageCodes) - 1 do
       if Uppercase(FieldStr(LanguageCodes[I],0,',')) = S then begin
         Repository.Languages.Identifier[EditLangIndex] := FieldStr(LanguageCodes[I],0,',');
         L := FieldStr(LanguageCodes[I],1,',');
         C := FieldStr(LanguageCodes[I],2,',');
         N := FieldStr(LanguageCodes[I],3,',');
         if Repository.Languages.Caption[EditLangIndex] = '' then
           Repository.Languages.Caption[EditLangIndex] := N;
         if Repository.Languages.Language[EditLangIndex] = '' then
           Repository.Languages.Language[EditLangIndex] := L;
         if (Repository.Languages.CodePage[EditLangIndex] = -1) then
           try
             Repository.Languages.Codepage[EditLangIndex] := StrToInt(C);
           except
             Repository.Languages.Codepage[EditLangIndex] := -1;
           end;
         break;
       end;
    SelectEditLanguage(EditLangIndex);
    LangStatusVerify;
end;

procedure TfMain.leLangNameChange(Sender: TObject);
begin
  LangNameVerify;
end;

procedure TfMain.leLangNameEditingDone(Sender: TObject);
begin
   if EditLangIndex < 0 then exit;
   if leLangName.Caption = led_NewLanguage then begin
     Repository.Languages.Caption[EditLangIndex] := '';
   end else begin
     Repository.Languages.Caption[EditLangIndex] := leLangName.Caption;
     leLangName.Caption := Repository.Languages.Caption[EditLangIndex];
   end;
   lvLanguages.Items.Item[EditLangIndex].Caption :=
     Repository.Languages.Caption[EditLangIndex];
end;

procedure TfMain.lvLanguagesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  SelectEditLanguage(Item.Index);
end;

procedure TfMain.lvLanguagesItemChecked(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item) then begin
     ActiveLanguage[Repository.Languages.Identifier[Item.Index]] :=
       Item.Checked;
  end;
end;

procedure TfMain.sbLanguageEditResize(Sender: TObject);
begin
    sbLanguageEdit.VertScrollBar.Page:= sbLanguageEdit.Height;
end;

procedure TfMain.tsAboutShow(Sender: TObject);
begin
  SetValueXML(xConfig, 'VERSION/ABOUT/REVISION', SOURCE_REVISION);
end;

procedure TfMain.tsGeneralShow(Sender: TObject);
begin
  cbSoftwareUpdate.ItemIndex:= GetValueXML(xConfig, 'SOFTWARE/UPDATE/INTERVAL', 4);
  lbUpdateChecked.Caption:=Format(lbl_UpdateChecked,
    [GetValueXML(xConfig, 'SOFTWARE/UPDATE/CHECKED', msg_UnknownDateTime)]);
end;

procedure TfMain.tsLanguagesShow(Sender: TObject);
var
  I : integer;
  LI : TListItem;
begin
  lvLanguages.BeginUpdate;
  lvLanguages.Clear;
  Repository.Languages.Reload;
  for I := 0 to Repository.Languages.Count - 1 do begin
    LI := lvLanguages.Items.Add;
    LI.Caption:=Repository.Languages.Caption[I];
    LI.Checked:=ActiveLanguage[Repository.Languages.Identifier[I]];
  end;
  lvLanguages.EndUpdate;
  SelectEditLanguage(-1);
end;

procedure TfMain.tsPackagesShow(Sender: TObject);
begin
  Reload;
  frPkgListEdit.Refresh;
end;

procedure TfMain.tsPrefsShow(Sender: TObject);
begin
  ReloadNeeded := true;
end;

procedure TfMain.tsProjectsShow(Sender: TObject);
begin
  Reload;
  lbComingSoon.Caption:=lbl_ComingSoon;
end;

procedure TfMain.tsRepoShow(Sender: TObject);
begin
  deLocalRepo.Directory:=GetValueXML(xConfig, 'REPOSITORY/LOCAL/PATH', '');

end;

procedure TfMain.actAppleAboutExecute(Sender: TObject);
begin
   SelectPrefsPage(tsAbout);
end;

procedure TfMain.actAddLanguageExecute(Sender: TObject);
var
  I : integer;
  LI : TListItem;
begin
  I := Repository.Languages.Add;
  Log(Self, 'Result ' + IntToStr(I));
  if I <> -1 then begin
    LI := lvLanguages.Items.Add;
    LI.Caption:=Repository.Languages.Caption[I];
    lvLanguages.Selected:=LI;
    LI.MakeVisible(false);
    leLangId.SetFocus;
    EditLangIndex := I;
  end;
end;

procedure TfMain.actApplePrefsExecute(Sender: TObject);
begin
  SelectPrefsPage(tsGeneral);
end;

procedure TfMain.actCodepageEditExecute(Sender: TObject);
begin
  fEditCodePage.Clear;
  fEditCodePage.Codepage:=leLangCodePage.Caption;
  fEditCodePage.ShowModal;
  leLangCodePageChange(Self);
end;

procedure TfMain.actCodepageNewExecute(Sender: TObject);
begin
  actCodePageEditExecute(Sender);
end;

procedure TfMain.actDebugLogExecute(Sender: TObject);
begin
  if not fLog.Visible then begin
    fLog.Caption := dlg_DebugLog;
    fLog.Show;
    Self.Show;
  end;
end;

procedure TfMain.actPackagesExecute(Sender: TObject);
begin
  pcMain.ActivePage := tsPackages;
end;

procedure TfMain.actPreferencesExecute(Sender: TObject);
begin
  SelectPrefsPage(tsGeneral);
end;

procedure TfMain.actProjectsExecute(Sender: TObject);
begin
   pcMain.ActivePage := tsProjects;
end;

procedure TfMain.actRemoveLanguageExecute(Sender: TObject);
begin
  if EditLangIndex <> -1 then begin
    if MessageDlg(Format(msg_VerifyLanguageDelete, []),
      mtConfirmation, [mbYes,mbNo], 0) = mrYes then begin
        ActiveLanguage[Repository.Languages.Identifier[EditLangIndex]] := False;
        Repository.Languages.Delete(EditLangIndex);
        EditLangIndex := -1;
        tsLanguagesShow(Self);
      end;
  end;
end;

procedure TfMain.actRemoveLanguageUpdate(Sender: TObject);
begin
  actRemoveLanguage.Enabled:=EditLangIndex <> -1;
end;

procedure TfMain.actSoftwareUpdateExecute(Sender: TObject);
begin
  SoftwareUpdate(False);
  lbUpdateChecked.Caption:=Format(lbl_UpdateChecked,
    [GetValueXML(xConfig, 'SOFTWARE/UPDATE/CHECKED', msg_UnknownDateTime)]);
end;

procedure TfMain.cbSoftwareUpdateChange(Sender: TObject);
begin
  SetValueXML(xConfig, 'SOFTWARE/UPDATE/INTERVAL', cbSoftwareUpdate.ItemIndex);
  xConfig.Flush;
end;

procedure TfMain.deLocalRepoAcceptDirectory(Sender: TObject; var Value: String);
begin
  OpenRepository(Value);
end;

procedure TfMain.tvPrefsChange(Sender: TObject; Node: TTreeNode);
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

function TfMain.GetActiveLanguage(ALang : String): boolean;
begin
  Result := False;
  ALang := UpperCase(AlphaOnly(ALang));
  if Length(ALang) < 2 then exit;
  Result := GetValueXML(xConfig, 'LANGUAGE/EDIT/' + Copy(ALang,1,2) + '/' + ALang, False);
end;

procedure TfMain.SetActiveLanguage(ALang : String; AValue: boolean);
begin
  ALang := UpperCase(AlphaOnly(ALang));
  if Length(ALang) < 2 then exit;
  SetValueXML(xConfig, 'LANGUAGE/EDIT/' + Copy(ALang,1,2) + '/' + ALang, AValue);
  xConfig.Flush;
end;

function TfMain.AddMenuItem(ToItem: TMenuItem; ActionItem: TBasicAction): TMenuItem;
begin
   // Add an item to the main menu based on it's action
   Result := TMenuItem.Create(Self);
   Result.Action := ActionItem;
   if Assigned(ToItem) then
      ToItem.Add(Result)
   else
      mMain.Items.Add(Result);
end;

function TfMain.AddMenuItem(ToItem: TMenuItem; CaptionText: TCaption): TMenuItem;
begin
   // Add an item to the main menu based on it's caption
   Result := TMenuItem.Create(Self);
   Result.Caption := CaptionText;
   if Assigned(ToItem) then
      ToItem.Add(Result)
   else
      mMain.Items.Add(Result);
end;

procedure TfMain.CreateMainMenu;
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

procedure TfMain.AddPrefsTree(ParentNode: TTreeNode; Pages: TPageControl);
var
   I : integer;
begin
   // Add the pages of an option to a node inthe preferences tree
  for I := 0 to Pages.PageCount - 1 do begin
    tvPrefs.Items.AddChild(ParentNode, Pages.Pages[I].Caption);
  end;
end;

procedure TfMain.SelectPrefsPage(Tab: TTabSheet);
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

procedure TfMain.LoadGlyphResources;
var
   I : Integer;
begin
  for I := Low(IconUI) to High(IconUI) do begin
      ilToolsMedium.AddLazarusResource(IconUI[I]);
      ilToolsSmall.AddLazarusResource(IconUI[I]);
  end;
  for I := Low(IconFlags) to High(IconFlags) do begin
      ilFlagsLarge.AddLazarusResource(IconFlags[I]);
      ilFlagsSmall.AddLazarusResource(IconFlags[I]);
  end;
end;

procedure TfMain.CreatePrefsTree;
begin
   // Clear the prefeences tree and populate the root pages
   tvPrefs.Items.Clear;
   AddPrefsTree(nil, pcPrefs);
   tvPrefs.FullExpand;
   tvPrefs.Items.GetFirstNode.Selected:=True;
end;

procedure TfMain.CreateAboutText;
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

procedure TfMain.OpenRepository(Location : String);
var
  GCF, S : String;
begin
    GCF := IncludeTrailingPathDelimiter(Location) +
      IncludeTrailingPathDelimiter('.git') + 'config';
    if (Location = '') or (not FileExists(GCF)) then begin
      lbGitRepo.Caption:=lbl_NotGitRepo;
    end else begin
      S := '';
      LoadFromFile(GCF, S, False);
      if Pos('url =', S) > 0 then
        S := Trim(Copy(S, Pos('url =', S) + 5, Length(S)))
      else
        S := '';
      if Pos(LF, S) > 0 then SetLength(S, Pos(LF, S) - 1);
      if Pos(CR, S) > 0 then SetLength(S, Pos(CR, S) - 1);
      lbGitRepo.Caption:=Format(lbl_GitRepo, [S]);
    end;
    if Location <> Repository.Path then begin
      Log(Self, 'Open local repository ' + Location);
      Repository.Path:= Location;
      Log(Self, 'Repository ' + Location + ' open');
      SetValueXML(xConfig, 'REPOSITORY/LOCAL/PATH', Repository.Path);
      xConfig.Flush;
    end;
end;

procedure TfMain.SetAppLanguageText(ALanguage: String);
var
  I : integer;
begin
  if ALanguage <> GetValueXML(xConfig, 'LANGUAGE/USER/ACTIVE',  '') then begin
    SetValueXML(xConfig, 'LANGUAGE/USER/ACTIVE',  ALanguage);
    xConfig.Flush;
  end;
  lbAvailLanguages.Caption:=lbl_AvailLanguages;
  lbLocalRepo.Caption:=lbl_LocalRepository;
  lbSoftwareUpdate.Caption:=lbl_SoftwareUpdate;
  deLocalRepo.DialogTitle:=dlg_LocalRepo;
  I := cbSoftwareUpdate.ItemIndex;
  cbSoftwareUpdate.Clear;
  cbSoftwareUpdate.Items.Add(cbox_SoftwareUpdateOff);
  cbSoftwareUpdate.Items.Add(cbox_SoftwareUpdateMonthly);
  cbSoftwareUpdate.Items.Add(cbox_SoftwareUpdateWeekly);
  cbSoftwareUpdate.Items.Add(cbox_SoftwareUpdateDaily);
  cbSoftwareUpdate.Items.Add(cbox_SoftwareUpdateHourly);
  cbSoftwareUpdate.ItemIndex := I;
  tsGeneral.Caption:=tab_PrefGeneral;
  tsRepo.Caption:=tab_PrefRepository;
  tsLanguages.Caption:=tab_PrefLanguages;
  tsAbout.Caption:=tab_PrefAbout;
  leLangName.EditLabel.Caption:=led_LanguageName;
  leLangID.EditLabel.Caption:=led_LanguageID;
  leLangDOS.EditLabel.Caption:=led_LanguageDOS;
  leLangCodePage.EditLabel.Caption:=led_LanguageCodePage;
end;

procedure TfMain.SelectEditLanguage(Index : integer);
var
  S, N, G : String;
  I : integer;
begin
  EditLangIndex := Index;
  if Index < 0 then begin
    btCodePage.Action := actCodePageNone;
    btCodePage.ImageIndex:=actCodePageNone.ImageIndex;
    leLangName.Text:='';
    leLangID.Text:='';
    leLangDOS.Text:='';
    leLangCodePage.Text:='';
    try
      leGraphic.Picture.LoadFromLazarusResource(IconUI[12]);
    except
      leGraphic.Picture.Clear;
    end;
    sbLanguageEdit.Enabled:=False;
  end else begin
    G := '';
    leLangName.Text:=Repository.Languages.Caption[EditLangIndex];
    if leLangName.Text = '' then
      leLangName.Text:=led_NewLanguage;
    lvLanguages.Items.Item[EditLangIndex].Caption:=leLangName.Text; { update list if needed }
    leLangID.Text:=Repository.Languages.Identifier[EditLangIndex];
    leLangDOS.Text:=Repository.Languages.Language[EditLangIndex];
    if Repository.Languages.Codepage[EditLangIndex] > 0 then
      leLangCodePage.Text:=ZeroPad(Repository.Languages.Codepage[EditLangIndex], 3)
    else
      leLangCodePage.Text:= '';
    sbLanguageEdit.Enabled:=True;
    if Repository.Languages.Graphic[EditLangIndex] <> '' then begin
      G := IconPrefix + Repository.Languages.Graphic[EditLangIndex];
    end else if Repository.Languages.Identifier[EditLangIndex] <> '' then begin
      N := '';
      S := Uppercase(Repository.Languages.Identifier[EditLangIndex]);
      for I := 0 to Length(LanguageCodes) - 1 do
        if Uppercase(FieldStr(LanguageCodes[I],0,',')) = S then begin
          G := FieldStr(LanguageCodes[I],4,',');
          break;
        end;
      for I := 0 to Length(CountryData) - 1 do
        if G = FieldStr(CountryData[I], 1, ',') then begin
          N := IconFlags[I];
          Break;
        end;
      Log(self, G + '>' + N);
      G := N;
    end;
    if G = '' then begin
      leGraphic.Picture.LoadFromLazarusResource(IconFlags[0]);
    end else begin
      leGraphic.Picture.LoadFromLazarusResource(G);
    end;
  end;
  LangStatusVerify;
  // Reset Edit Area to top
  sbLanguageEdit.VertScrollBar.Position:=0;
  sbLanguageEdit.VertScrollBar.Range:=bbRemoveLanguage.Top + bbRemoveLanguage.Height + 2;
end;

function TfMain.LangStatusVerify: boolean;
begin
  Result := LangIDVerify;
  Result := LangDosVerify and Result;
  Result := LangNameVerify and Result;
  Result := LangCodepageVerify and Result;
//  if (EditLangIndex > 0) and (not Result) then
//    lvLanguages.Items.Item[EditLangIndex].Checked:=False;
end;

function TfMain.LangIDVerify: boolean;
var
  I : integer;
  L : String;
begin
  L := Trim(Uppercase(leLangID.Caption));
  Result := (Length(L) = 2) or ((Length(L) = 5) and ((L[3] = '_') or (L[3] = '-')));
  if Result then
    for I := 0 to Repository.Languages.Count - 1 do
      if (I <> EditLangIndex) and (L = Uppercase(Repository.Languages.Data[I].Identifier)) then begin
        Result := False;
        Break;
      end;
  if (EditLangIndex < 0) or Result then
    iLangIDVerify.Picture.Clear
  else
    iLangIDVerify.Picture.LoadFromLazarusResource(IconUI[16]);
end;

function TfMain.LangNameVerify: boolean;
var
  I : integer;
  L : String;
begin
  L := Trim(Uppercase(leLangName.Caption));
  Result := L <> '';
  if Result then
    for I := 0 to Repository.Languages.Count - 1 do
      if (I <> EditLangIndex) and (L = Uppercase(Repository.Languages.Data[I].Caption)) then begin
        Result := False;
        Break;
      end;
  if (EditLangIndex < 0) or Result then
    iLangNameVerify.Picture.Clear
  else
    iLangNameVerify.Picture.LoadFromLazarusResource(IconUI[16]);
end;

function TfMain.LangDosVerify: boolean;
var
  I : integer;
  L : String;
begin
  L := Trim(Uppercase(leLangDOS.Caption));
  Result := (Length(L) > 1) and (Length(L) < 4);
  if Result then
    for I := 0 to Repository.Languages.Count - 1 do
      if (I <> EditLangIndex) and (L = Uppercase(Repository.Languages.Data[I].Language)) then begin
        Result := False;
        Break;
      end;
  if (EditLangIndex < 0) or Result then
    iLangDosVerify.Picture.Clear
  else
    iLangDosVerify.Picture.LoadFromLazarusResource(IconUI[16]);
end;

function TfMain.LangCodePageVerify: boolean;
begin
  Result := btCodePage.Action = actCodePageEdit;
end;

procedure TfMain.Reload;
begin
  if not ReloadNeeded then exit;
  ReloadNeeded := false;
  Log(Self, 'Repository Reload Triggered');
  exit;
  Repository.Reload;
  frPkgListEdit.Clear;
  frPkgListEdit.Reload;
end;

procedure TfMain.SoftwareUpdate(Silent: boolean);
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
       LT := ScanDateTime(DTFmt, GetValueXML(xConfig, 'SOFTWARE/UPDATE/CHECKED', 'failed'));
       case GetValueXML(xConfig, 'SOFTWARE/UPDATE/INTERVAL', 4) of
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
  SetValueXML(xConfig, 'SOFTWARE/UPDATE/CHECKED', FormatDateTime(DTFmt, DT));
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

