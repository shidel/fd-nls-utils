// Application, FPC & Lazarus Version Information

const
{$IF declared(TAppVersionInfoType)}

  VersionInfo : TAppVersionInfoType = (
  { The default Free Pascal Compiler }
    FreePascal : (
      Version : '3.3.1';
      Revision : '';
      Target : 'x86_64';
    );
  { The Lazarus I.D.E }
    Lazarus : (
      Version : '';
      Revision : '';
    );
  { Source & Subversion Last Changed Commit }
    Source : (
      Version : '0.0.0.1';
      Revision : '12';
      Online : '';
      URL : 'http://github.com/shidel/fd-nls-utils';
      Commit : 'fce00218d03f34cfe059f6e07ee372e6da2f1c39';
    );
  { Version Build Atributes } 
    Attributes : ( 
      Debug : False;
      PreRelease : True;
      Patched : False;
      PrivateBuild : False;
      SpecialBuild : False;
      Language : '';
      CharSet : '';
      Date : '2022-02-21 05:27:51';
    );
  { General Application Information }
    Application : (
      Identifier : 'com.jeromeshidel.freedosnlsdesktop';
      Version : '0.0.0.1';
      Comments : '';
      CompanyName : 'Jerome Shidel';
      FileDescription : 'FreeDOS NLS Desktop Utility';
      InternalName : 'FD-NLS Desktop';
      LegalCopyright : '2022 Jerome Shidel';
      LegalTrademarks : '';
      OriginalFilename : 'fdnls';
      ProductName : 'FreeDOS NLS Desktop';
      ProductVersion : '';
      Year : '2022';
    );
  );

{$ELSE}

  { The default Free Pascal Compiler }
  FPC_VERSION='3.3.1';
  FPC_REVISION='';
  FPC_TARGET='x86_64';

  { Platform specific and cross-compilers }

  { The Lazarus I.D.E }
  LAZARUS_VERSION='LAZARUS_VERSION='';';
  LAZARUS_REVISION='LAZARUS_REVISION='';';

  { Source & Subversion Last Changed Commit }
  SOURCE_VERSION='0.0.0.1';
  SOURCE_REVISION='12';
  SOURCE_URL='http://github.com/shidel/fd-nls-utils';
  SOURCE_ONLINE='';
  SOURCE_COMMIT='fce00218d03f34cfe059f6e07ee372e6da2f1c39';

  { Version Build Atributes } 
  BUILD_DEBUG=False;
  BUILD_PRERELEASE=True;
  BUILD_PATCHED=False;
  BUILD_PRIVATE=False;
  BUILD_SPECIAL=False;
  BUILD_LANGUAGE='';
  BUILD_CHARSET='';
  BUILD_DATE='2022-02-21 05:27:51';

  { General Application Information }
  APP_IDENTIFIER='com.jeromeshidel.freedosnlsdesktop';
  APP_VERSION='0.0.0.1';
  APP_COMPANYNAME='Jerome Shidel';
  APP_FILEDESCRIPTION='FreeDOS NLS Desktop Utility';
  APP_INTERNALNAME='FD-NLS Desktop';
  APP_LEGALCOPYRIGHT='2022 Jerome Shidel';
  APP_ORIGINALFILENAME='fdnls';
  APP_PRODUCTNAME='FreeDOS NLS Desktop';
  APP_YEAR='2022';

{$ENDIF}
