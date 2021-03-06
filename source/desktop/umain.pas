unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, FDKit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ActnList, ComCtrls, ExtCtrls, Buttons,
  XMLConf, LCLType, LCLIntf, EditBtn, Spin, IpHtml, Ipfilebroker, ClassExt,
  opensslsockets, fphttpclient, DateUtils,
  uAppNLS, uAppCfg, uLog, uPickFlag, uEditCodePage, uPkgListEdit,
  Icons, Types;

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
    cbLanguageEnable: TCheckBox;
    cbWideDOSFont: TCheckBox;
    cbAlwaysEnglish: TCheckBox;
    deLocalRepo: TDirectoryEdit;
    Edit1: TEdit;
    frPkgListEdit: TframePkgListEdit;
    iLangNameVerify: TImage;
    iLangIdVerify: TImage;
    ilToolsMedium: TImageList;
    ilFlagsLarge: TImageList;
    ilFlagsSmall: TImageList;
    ilToolsSmall: TImageList;
    iLangDosVerify: TImage;
    lbMasterFontSize: TLabel;
    lbDescriptions: TLabel;
    lbPreviewInterval: TLabel;
    lbSimulator: TLabel;
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
    Memo1: TMemo;
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
    seMasterFont: TSpinEdit;
    sPrefs: TSplitter;
    itMinute: TTimer;
    tsDescriptions: TTabSheet;
    tbPreviewInterval: TTrackBar;
    tsSimulator: TTabSheet;
    tbSepA: TToolButton;
    tbSepB: TToolButton;
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
    procedure actAddLanguageExecute(Sender: TObject);
    procedure actAddLanguageUpdate(Sender: TObject);
    procedure actAppleAboutExecute(Sender: TObject);
    procedure actApplePrefsExecute(Sender: TObject);
    procedure actCodepageEditExecute(Sender: TObject);
    procedure actCodepageNewExecute(Sender: TObject);
    procedure actDebugLogExecute(Sender: TObject);
    procedure actPackagesExecute(Sender: TObject);
    procedure actPackagesUpdate(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actProjectsExecute(Sender: TObject);
    procedure actRemoveLanguageExecute(Sender: TObject);
    procedure actRemoveLanguageUpdate(Sender: TObject);
    procedure actSoftwareUpdateExecute(Sender: TObject);
    procedure cbAlwaysEnglishChange(Sender: TObject);
    procedure cbLanguageEnableClick(Sender: TObject);
    procedure cbSoftwareUpdateChange(Sender: TObject);
    procedure cbWideDOSFontChange(Sender: TObject);
    procedure deLocalRepoAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hpAboutHotClick(Sender: TObject);
    procedure itMinuteTimer(Sender: TObject);
    procedure lbMasterFontSizeResize(Sender: TObject);
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
    procedure lvLanguagesClick(Sender: TObject);
    procedure lvLanguagesItemChecked(Sender: TObject; Item: TListItem);
    procedure sbLanguageEditResize(Sender: TObject);
    procedure seMasterFontChange(Sender: TObject);
    procedure tbPreviewIntervalChange(Sender: TObject);
    procedure tsAboutShow(Sender: TObject);
    procedure tsDescriptionsShow(Sender: TObject);
    procedure tsGeneralShow(Sender: TObject);
    procedure tsLanguagesShow(Sender: TObject);
    procedure tsPackagesShow(Sender: TObject);
    procedure tsPrefsShow(Sender: TObject);
    procedure tsProjectsShow(Sender: TObject);
    procedure tsRepoShow(Sender: TObject);
    procedure tsSimulatorShow(Sender: TObject);
    procedure tvPrefsChange(Sender: TObject; Node: TTreeNode);
    procedure SetLangCheckBox(State : boolean);
  private
    function GetActiveLanguage(ALang : String): boolean;
    procedure SetActiveLanguage(ALang : String; AValue: boolean);
    procedure SwitchModes;
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
   function ActiveLanguages(ExcludeEnglish : boolean = false) : TStringArray;
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
   Log(Self, 'User Home Path: ' + UserHomePath);
   Log(Self, 'Program Data Path: ' + AppDataPath);
   Log(Self, 'Program Config Path: ' + AppCfgPath);
   Log(Self, 'Program Config File: ' + AppCfgFile);
   Log(Self, 'Update Server: ' + UpdateServer);
   Log(Self, '');
   Repository := TFDNLS.Create;
   FDNLS.AutoCreate := True;
   // Hide some design time elements
   pcMain.ShowTabs := False;
   pcPrefs.ShowTabs := False;
   // configure local repository
   OpenRepository(GetValueXML(Settings, 'REPOSITORY/LOCAL/PATH',  ''));
   SetAppLanguageText(GetValueXML(Settings, 'LANGUAGE/USER/ACTIVE',
     UpperCase(GetEnvironmentVariable('LANG'))));
   GetPropertyState(Self);
   // Populate and configure UI elements
   LoadGlyphResources;
   CreateMainMenu;
   CreatePrefsTree;
   CreateAboutText;
   // Config verification
   if GetValueXML(Settings, 'VERSION/ABOUT/REVISION',  '') <> SOURCE_REVISION then
      SelectPrefsPage(tsAbout)
   else if not DirectoryExists(GetValueXML(Settings, 'REPOSITORY/LOCAL/PATH', '')) then
      SelectPrefsPage(tsRepo);
   {$if defined(darwin)}
     cbLanguageEnable.Visible:=False;
   {$else}
     bbRemoveLanguage.Top := cbLanguageEnable.Top - 8;
   {$endif}
   {$if defined(linux)}
     lvLanguages.CheckBoxes:=False;
   {$endif}
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  try
    Properties.Flush;
  finally
  end;
  try
    Settings.Flush;
  finally
  end;
  try
    FreeAndNil(Repository);
  finally
  end;
end;

procedure TfMain.FormHide(Sender: TObject);
begin
  SetPropertyState(Self);
  SetPropertyState(pLanguagesList, 'HEIGHT', pLanguagesList.Height);
  SetPropertyState(tvPrefs, 'WIDTH', tvPrefs.Width);
  SetPropertyState(frPkgListEdit.lvPackages, 'WIDTH',
    frPkgListEdit.lvPackages.Width);
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  pLanguagesList.Height:= GetPropertyState(pLanguagesList, 'HEIGHT',
    pLanguagesList.Height);
  tvPrefs.Width := GetPropertyState(tvPrefs, 'WIDTH', tvPrefs.Width);
  frPkgListEdit.lvPackages.Width := GetPropertyState(frPkgListEdit.lvPackages,
    'WIDTH', frPkgListEdit.lvPackages.Width);
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

procedure TfMain.lbMasterFontSizeResize(Sender: TObject);
begin
    seMasterFont.Left:=RightOf(lbMasterFontSize) + 8;
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
  {$IF defined(linux)}
  {$ELSE}
  SelectEditLanguage(Item.Index);
  if Change = ctImage then begin end;
  {$ENDIF}
end;

procedure TfMain.lvLanguagesClick(Sender: TObject);
begin
  {$IF defined(linux)}
  if Assigned(lvLanguages.Selected) then
    SelectEditLanguage(lvLanguages.Selected.Index);
  {$ENDIF}

end;

procedure TfMain.lvLanguagesItemChecked(Sender: TObject; Item: TListItem);
begin
  {$IF defined(linux)}
  {$ELSE}
   if Assigned(Item) then begin
     Log(self, 'Select Language ' + IntToStr(Item.Index) + ' ' + WhenTrue(Item.Checked, 'true', 'false'));
     ActiveLanguage[Repository.Languages.Identifier[Item.Index]] := Item.Checked;
     SetLangCheckBox(Item.Checked);
   end;
  {$ENDIF}
end;

procedure TfMain.sbLanguageEditResize(Sender: TObject);
begin
    sbLanguageEdit.VertScrollBar.Page:= sbLanguageEdit.Height;
end;

procedure TfMain.seMasterFontChange(Sender: TObject);
begin
  SetSetting('DETAILS/MASTER/FONT/SIZE', seMasterFont.Value);
end;

procedure TfMain.tbPreviewIntervalChange(Sender: TObject);
var
  X : integer;
begin
    X := tbPreviewInterval.Position * 50;
    if X = 0 then X := 10;
    lbPreviewInterval.Caption:=Format(lbl_PreviewInterval, [IntToStr(X)]);
    SetSetting('PREVIEW/INTERVAL', X);
    Settings.Flush;
end;

procedure TfMain.tsAboutShow(Sender: TObject);
begin
  SetValueXML(Settings, 'VERSION/ABOUT/REVISION', SOURCE_REVISION);
end;

procedure TfMain.tsDescriptionsShow(Sender: TObject);
begin
  lbDescriptions.Caption:=lbl_Descriptions;
  lbMasterFontSize.Caption:=lbl_MasterFontSize;
  seMasterFont.Left:=RightOf(lbMasterFontSize) + 8;
  seMasterFont.Value:=GetSetting('DETAILS/MASTER/FONT/SIZE',
    abs(GetFontData(Font.Reference.Handle).Height));
end;

procedure TfMain.tsGeneralShow(Sender: TObject);
begin
  cbSoftwareUpdate.ItemIndex:= GetValueXML(Settings, 'SOFTWARE/UPDATE/INTERVAL', 4);
  lbUpdateChecked.Caption:=Format(lbl_UpdateChecked,
    [GetValueXML(Settings, 'SOFTWARE/UPDATE/CHECKED', msg_UnknownDateTime)]);
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

procedure TfMain.SwitchModes;
begin
  frPkgListEdit.lvPackages.Selected:=nil;
  frPkgListEdit.StatusPanel:=nil;
  sbMain.Panels.Clear;

end;

procedure TfMain.tsPackagesShow(Sender: TObject);
begin
  SwitchModes;
  Reload;
  frPkgListEdit.Refresh;
  frPkgListEdit.StatusPanel:=sbMain.Panels.Add;
end;

procedure TfMain.tsPrefsShow(Sender: TObject);
begin
  SwitchModes;
  ReloadNeeded := true;
end;

procedure TfMain.tsProjectsShow(Sender: TObject);
begin
  SwitchModes;
  Reload;
  lbComingSoon.Caption:=lbl_ComingSoon;
end;

procedure TfMain.tsRepoShow(Sender: TObject);
begin
  deLocalRepo.Directory:=GetSetting('REPOSITORY/LOCAL/PATH', '');
end;

procedure TfMain.tsSimulatorShow(Sender: TObject);
var
  X : integer;
begin
  tbPreviewInterval.Position:=GetSetting('PREVIEW/INTERVAL', 500) div 50;
  cbWideDOSFont.Checked := GetSetting('PREVIEW/WIDEFONT', FALSE);
  cbAlwaysEnglish.Checked := GetSetting('PREVIEW/ENGLISH', FALSE);
  X := tbPreviewInterval.Position * 50;
  if X = 0 then X := 10;
  lbPreviewInterval.Caption:=Format(lbl_PreviewInterval, [IntToStr(X)]);
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
    {$if defined(linux)}
      LI.Caption:=Repository.Languages.Caption[I];
      if LI.Caption = '' then LI.Caption:=led_NewLanguage;
      Self.SelectEditLanguage(I);
      leLangId.SetFocus;
      LI.MakeVisible(False);
      LI.Selected:=True;
    {$else}
      LI.Caption:=Repository.Languages.Caption[I];
      lvLanguages.Selected:=LI;
      LI.MakeVisible(false);
      leLangId.SetFocus;
      EditLangIndex := I;
    {$endif}
  end;
end;

procedure TfMain.actAddLanguageUpdate(Sender: TObject);
begin
  actAddLanguage.Visible:=True;
  actAddLanguage.Enabled:=Repository.Path <> '';
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
    // Self.Show;
  end;
end;

procedure TfMain.actPackagesExecute(Sender: TObject);
begin
  pcMain.ActivePage := tsPackages;
end;

procedure TfMain.actPackagesUpdate(Sender: TObject);
begin
    actPackages.Enabled:=Repository.Path <> '';
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
    [GetValueXML(Settings, 'SOFTWARE/UPDATE/CHECKED', msg_UnknownDateTime)]);
end;

procedure TfMain.cbAlwaysEnglishChange(Sender: TObject);
begin
  SetSetting('PREVIEW/ENGLISH', cbAlwaysEnglish.Checked);
  Settings.Flush;
end;

procedure TfMain.cbLanguageEnableClick(Sender: TObject);
begin
  if Assigned(lvLanguages.Selected) then begin
    Log(self, 'Select Language (cb) ' + IntToStr(lvLanguages.Selected.Index) + ' ' + WhenTrue(cbLanguageEnable.Checked, 'true', 'false'));
    ActiveLanguage[Repository.Languages.Identifier[lvLanguages.Selected.Index]] := cbLanguageEnable.Checked;
    lvLanguages.Selected.Checked:=not lvLanguages.Selected.Checked;
  end;
end;

procedure TfMain.cbSoftwareUpdateChange(Sender: TObject);
begin
  SetValueXML(Settings, 'SOFTWARE/UPDATE/INTERVAL', cbSoftwareUpdate.ItemIndex);
  Settings.Flush;
end;

procedure TfMain.cbWideDOSFontChange(Sender: TObject);
begin
  SetSetting('PREVIEW/WIDEFONT', cbWideDOSFont.Checked);
  Settings.Flush;
end;

procedure TfMain.deLocalRepoAcceptDirectory(Sender: TObject; var Value: String);
begin
  OpenRepository(Value);
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := CloseAction;
  frPkgListEdit.SelectEdit(nil);
  Repository.PackageLists.SaveChanges;
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

procedure TfMain.SetLangCheckBox(State: boolean);
var
  Hold : TNotifyEvent;
begin
  Hold:=cbLanguageEnable.OnClick;
  cbLanguageEnable.OnClick:=nil;
  cbLanguageEnable.Checked:=State;
  cbLanguageEnable.OnClick:=Hold;
end;

function TfMain.GetActiveLanguage(ALang : String): boolean;
begin
  Result := False;
  ALang := UpperCase(AlphaOnly(ALang));
  if Length(ALang) < 2 then exit;
  Result := GetValueXML(Settings, 'LANGUAGE/EDIT/' + Copy(ALang,1,2) + '/' + ALang, False);
end;

procedure TfMain.SetActiveLanguage(ALang : String; AValue: boolean);
begin
  ALang := UpperCase(AlphaOnly(ALang));
  if Length(ALang) < 2 then exit;
  SetValueXML(Settings, 'LANGUAGE/EDIT/' + Copy(ALang,1,2) + '/' + ALang, AValue);
  Settings.Flush;
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
   {$IF defined(darwin)}
     // Create Aplication menu for Apple macOS
     aMenu := AddMenuItem(nil, #$EF#$A3#$BF); { Unicode Apple Logo }
     AddMenuItem(aMenu, actAppleAbout);
     AddMenuItem(aMenu, '-');
     AddMenuItem(aMenu, actApplePrefs);
     AddMenuItem(aMenu, '-');
   {$ELSEIF defined(windows)}
   {$ENDIF}
   Exit;
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
      '<html><body' +
      {$IF defined(darwin)}
      {$ELSE}
        ' style="background:LightGray;"' +
      {$ENDIF}
      '><center>' +
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
      SetValueXML(Settings, 'REPOSITORY/LOCAL/PATH', Repository.Path);
      Settings.Flush;
    end;
end;

procedure TfMain.SetAppLanguageText(ALanguage: String);
var
  I : integer;
begin
  if ALanguage <> GetValueXML(Settings, 'LANGUAGE/USER/ACTIVE',  '') then begin
    SetValueXML(Settings, 'LANGUAGE/USER/ACTIVE',  ALanguage);
    Settings.Flush;
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
  cbLanguageEnable.Caption:=lbl_LanguageEnabled;

  tsSimulator.Caption:=tab_Simulator;
  lbSimulator.Caption:=lbl_Simulator;
  cbWideDOSFont.Caption:=cbox_WideDOSFont;
  cbAlwaysEnglish.Caption:=cbox_AlwaysEnglish;
  lbPreviewInterval.Caption:=Format(lbl_PreviewInterval, ['500']);

  tsDescriptions.Caption:=tab_Descriptions;
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
    SetLangCheckBox(False);
    try
      leGraphic.Picture.LoadFromLazarusResource(IconUI[icon_NoImage]);
    except
      leGraphic.Picture.Clear;
    end;
    sbLanguageEdit.Enabled:=False;
  end else begin
    G := '';
    leLangName.Text:=Repository.Languages.Caption[EditLangIndex];
    if leLangName.Text = '' then
      leLangName.Text:=led_NewLanguage;
    SetLangCheckBox(lvLanguages.Items[EditLangIndex].Checked);
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

function TfMain.ActiveLanguages(ExcludeEnglish: boolean): TStringArray;
var
  I, C : integer;
begin
  Result := [];
  C := 0;
  SetLength(Result, Repository.Languages.Count);
  Repository.Languages.Reload;
  for I := 0 to Repository.Languages.Count - 1 do
    if ActiveLanguage[Repository.Languages.Identifier[I]] then
      if (not ExcludeEnglish) or
      (lowercase(Repository.Languages.Identifier[I]) <> 'en_us') then begin
        Result[C] := Repository.Languages.Identifier[I];
        Inc(C);
      end;
  SetLength(Result,C);
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
    iLangIDVerify.Picture.LoadFromLazarusResource(IconUI[icon_Error]);
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
    iLangNameVerify.Picture.LoadFromLazarusResource(IconUI[icon_Error]);
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
    iLangDosVerify.Picture.LoadFromLazarusResource(IconUI[icon_Error]);
end;

function TfMain.LangCodePageVerify: boolean;
begin
  Result := btCodePage.Action = actCodePageEdit;
end;

procedure TfMain.Reload;
begin
  if not ReloadNeeded then exit;
  Log(Self, 'Repository Reload Triggered');
  frPkgListEdit.Clear;
  Repository.Reload;
  frPkgListEdit.Reload;
  ReloadNeeded := false;
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
       LT := ScanDateTime(DTFmt, GetValueXML(Settings, 'SOFTWARE/UPDATE/CHECKED', 'failed'));
       case GetValueXML(Settings, 'SOFTWARE/UPDATE/INTERVAL', 4) of
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
  SetValueXML(Settings, 'SOFTWARE/UPDATE/CHECKED', FormatDateTime(DTFmt, DT));
  Settings.Flush;
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

