unit uAppNLS;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt;

const
  msg_NoAppUpdatesAvail         = 'No updates are available at this time.';
  msg_UsingLatestAppVersion     = 'You are currently using the latest version available.';
  msg_UpdateAvailable           = '%0:s version %1:s is available. ' + CRLF + 'Download now?';
  msg_AboutVersion              = 'Version %0:s (r%1:s)';
  msg_AboutLicense              = 'BSD 3-Clause License';
  msg_AboutCopyright            = 'Copyright (c) %0:s';
  msg_AboutIconCopyright        = 'Most icons are Copyright (c) %0:s, %1:s and may be download in the %2:s collection';
  msg_AboutCreatedWith          = 'Created with the %0:s and %1:s';

  msg_lbAvailLanguages          = 'Available languages';
  msg_lbLocalRepository         = 'Location of local repository';
  msg_lbSoftwareUpdate          = 'Check for software updates';

implementation

end.

