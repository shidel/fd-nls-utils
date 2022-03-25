unit uAppNLS;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt;

resourcestring
  msg_NoAppUpdatesAvail         = 'No updates are available at this time.';
  msg_UsingLatestAppVersion     = 'You are currently using the latest version available.';
  msg_UpdateAvailable           = '%0:s version %1:s is available. ' + CRLF + 'Download now?';
  msg_AboutVersion              = 'Version %0:s (r%1:s)';
  msg_AboutLicense              = 'BSD 3-Clause License';
  msg_AboutCopyright            = 'Copyright (c) %0:s';
  msg_AboutIconCopyright        = 'Most icons are Copyright (c) %0:s, %1:s and may be download in the %2:s collection';
  msg_AboutCreatedWith          = 'Created with the %0:s and %1:s';
  msg_VerifyLanguageDelete      = 'WARNING: This will affect all repository contributors!' + CRLF + CRLF +
                                  'You may wish to only disable the language on your' + CRLF +
                                  'computer by unchecking it.' + CRLF + CRLF +
                                  'Are you sure you want to delete this Language?';

  msg_UnknownDateTime           = '(unknown)';

  lbl_AvailLanguages            = 'Available languages';
  lbl_LocalRepository           = 'Location of local repository';
  lbl_SoftwareUpdate            = 'Check for software updates';
  lbl_NoCodepageFonts           = 'no codepage font available';
  lbl_UsingCodepageFont         = 'using %0:s font for codepage';
  lbl_UpdateChecked             = 'Last update check was %0:s';
  lbl_NotGitRepo                = 'Not a git repository';
  lbl_GitRepo                   = 'Git repository %0:s';
  lbl_ComingSoon                = 'not implemented, coming soon';
  lbl_LanguageEnabled           = 'Enable Language';
  lbl_Simulator                 = 'DOS Codepage Preview Simulator';

  dlg_LocalRepo                 = 'FD-NLS Local Repository';
  dlg_DebugLog                  = 'Debug Log';
  dlg_PickFlag                  = 'Select image';
  dlg_ConfirmDelete             = 'Confirm Deletion';
  dlg_EditCodePage              = 'Codepage %0:s';

  cbox_SoftwareUpdateOff        = 'disabled';
  cbox_SoftwareUpdateMonthly    = 'Monthly';
  cbox_SoftwareUpdateWeekly     = 'Weekly';
  cbox_SoftwareUpdateDaily      = 'Daily';
  cbox_SoftwareUpdateHourly     = 'Hourly';

  tab_PrefGeneral               = 'General';
  tab_PrefRepository            = 'Repository';
  tab_Simulator                 = 'Simulator';
  tab_Descriptions              = 'Packages';
  tab_PrefLanguages             = 'Languages';
  tab_PrefAbout                 = 'About';

  led_LanguageName              = 'Language name';
  led_LanguageID                = 'Language identifier';
  led_LanguageDOS               = 'DOS ID';
  led_LanguageCodepage          = 'Code page';
  led_NewLanguage               = 'New Language';

  lvh_EditCpDOS                 = 'DOS';
  lvh_EditCpValue               = 'Value';
  lvh_EditCPUnicode             = 'Editor';
  lvh_EditCpUTF8                = 'UTF-8';
  lvh_EditCpHTML                = 'HTML';

  lbl_EditCpValue               = 'Ascii Code %0:s';
  lbl_EditCPUnicode             = 'Unicode';
  lbl_EditCpUTF8                = 'UTF-8';
  lbl_EditCpHTML                = 'HTML';
  lbl_DOSCP                     = 'CP %0:s';

  btn_OK                        = 'OK';
  btn_Cancel                    = 'Cancel';

  dos_PkgGroup                  = 'Eingerichtet!';
  dos_PkgAuthor                 = 'Author''s name';
  dos_PkgVersion                = 'Version X.YZ';
  dos_PkgPlatforms              = '';

  lbl_PreviewInterval           = 'Simulator refresh interval %0:sms';
  cbox_WideDOSFont              = 'Display 8-bit font as 9-bits wide.';
  cbox_AlwaysEnglish            = 'Revert to English preview on selection change.';

  lbl_Descriptions              = 'Package description details';
  lbl_MasterFontSize            = 'Master details font size:';

implementation

end.

