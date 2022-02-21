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
      Version : '0.0.0.3';
      Revision : '19';
      Online : '';
      URL : 'http://github.com/shidel/fd-nls-utils';
      Commit : 'f96877440521db74653c8998e95178373a033143';
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
      Date : '2022-02-21 16:50:13';
    );
  { General Application Information }
    Application : (
      Identifier : 'com.jeromeshidel.fd-nls';
      Version : '0.0.0.3';
      Comments : '';
      CompanyName : 'Jerome Shidel';
      FileDescription : 'FreeDOS NLS Desktop Utility';
      InternalName : 'FD-NLS';
      LegalCopyright : '2022 Jerome Shidel';
      LegalTrademarks : '';
      OriginalFilename : 'fdnls-desktop';
      ProductName : 'FD-NLS';
      ProductVersion : '1.0';
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
  SOURCE_VERSION='0.0.0.3';
  SOURCE_REVISION='19';
  SOURCE_URL='http://github.com/shidel/fd-nls-utils';
  SOURCE_ONLINE='';
  SOURCE_COMMIT='f96877440521db74653c8998e95178373a033143';

  { Version Build Atributes } 
  BUILD_DEBUG=False;
  BUILD_PRERELEASE=True;
  BUILD_PATCHED=False;
  BUILD_PRIVATE=False;
  BUILD_SPECIAL=False;
  BUILD_LANGUAGE='';
  BUILD_CHARSET='';
  BUILD_DATE='2022-02-21 16:50:13';

  { General Application Information }
  APP_IDENTIFIER='com.jeromeshidel.fd-nls';
  APP_VERSION='0.0.0.3';
  APP_COMPANYNAME='Jerome Shidel';
  APP_FILEDESCRIPTION='FreeDOS NLS Desktop Utility';
  APP_INTERNALNAME='FD-NLS';
  APP_LEGALCOPYRIGHT='2022 Jerome Shidel';
  APP_ORIGINALFILENAME='fdnls-desktop';
  APP_PRODUCTNAME='FD-NLS';
  APP_PRODUCTVERSION='1.0';
  APP_YEAR='2022';

{$ENDIF}
