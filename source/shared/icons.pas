(*

The icons used are not redistributable and Copyright Icons8, http://icons8.com

A collection of the needed icons can be freely downloaded from their website
at https://icons8.com/icons/share-collections/uRjmCz4CbAnb at 100px resolution

Once downloaded, extract the files into a subdirectory called icons8 and run
the shell script icons.sh to create the resource file need to compile

*)

unit Icons;

{$I defines.inc}

interface

uses LResources;

const

  { 3rd Party Icon and Copyright Information }

  IconProviderCopyright = '2022 Icons8';
  IconProviderURL       = 'https://icons8.com';
  IconCollectionName    = 'APP-FDNLS';
  IconCollectionURL     = 'https://icons8.com/icons/share-collections/uRjmCz4CbAnb';

  IconUI : array of string = (
    'bug',                 { 0 }
    'gear',                { 1 }
    'scroll',              { 2 }
    'hierarchy',           { 3 }
    'list',                { 4 }
    'add',                 { 5 }
    'remove',
    'error',
    'check-circle',
    'save',
    'save-all',
    'form',
    'category',
    'git',
    'github',
    'gitlab',
    'project',
    'project-setup',
    'group-of-projects',
    'folder-tree',
    'hint',
    'bug',
    'dos',
    'trash-can',
    'repository',
    'commit-git',
    'compare-git',
    'language'
  );

  IconFlags : array of string = (
    'aland-islands',
    'albania',
    'algeria',
    'andorra',
    'angola',
    'anguilla',
    'antarctica',
    'antigua-and-barbuda',
    'argentina',
    'armenia',
    'aruba',
    'australia',
    'austria',
    'azerbaijan',
    'bahamas',
    'bahrain',
    'bangladesh',
    'barbados',
    'belarus',
    'belgium',
    'belize',
    'benin',
    'bermuda',
    'bhutan',
    'bolivia',
    'bosnia-and-herzegovina',
    'botswana',
    'brazil',
    'british-indian-ocean-territory',
    'british-virgin-islands',
    'brunei-darussalam',
    'bulgaria',
    'burkina-faso',
    'burundi',
    'cambodia',
    'cameroon',
    'canada',
    'cape-verde',
    'cayman-islands',
    'central-african-republic',
    'chad',
    'chile',
    'china',
    'christmas-island',
    'cocos-keeling-islands',
    'colombia',
    'comoros',
    'congo',
    'cook-islands',
    'costa-rica',
    'croatia',
    'cuba',
    'curaao',
    'cyprus',
    'czech-republic',
    'democratic-republic-congo',
    'denmark',
    'djibouti',
    'dominica',
    'dominican-republic',
    'ecuador',
    'egypt',
    'el-salvador',
    'equatorial-guinea',
    'eritrea',
    'estonia',
    'ethiopia',
    'falkland-islands',
    'faroe-islands',
    'fiji',
    'finland',
    'flag-of-afghanistan',
    'france',
    'french-polynesia',
    'french-southern-territories',
    'gabon',
    'gambia',
    'georgia',
    'germany',
    'ghana',
    'gibraltar',
    'great-britain',
    'greece',
    'greenland',
    'grenada',
    'guam',
    'guatemala',
    'guernsey',
    'guinea',
    'guinea-bissau',
    'guyana',
    'haiti-flag',
    'honduras',
    'hongkong-flag',
    'hungary',
    'iceland',
    'india',
    'indonesia',
    'iran',
    'iraq',
    'ireland',
    'isle-of-man',
    'israel',
    'italy',
    'ivory-coast',
    'jamaica',
    'japan',
    'jersey',
    'jordan',
    'kazakhstan',
    'kenya',
    'kiribati',
    'kuwait',
    'kyrgyzstan',
    'laos',
    'latvia',
    'lebanon',
    'lesotho',
    'liberia',
    'libya',
    'liechtenstein',
    'lithuania',
    'luxembourg',
    'macao',
    'macedonia',
    'madagacar',
    'malawi',
    'malaysia',
    'maldives',
    'mali',
    'malta',
    'marshall-islands',
    'mauritania',
    'mauritius',
    'merge-git',
    'mexico',
    'micronesia',
    'moldova',
    'monaco',
    'mongolia',
    'montenegro',
    'montserrat',
    'morocco',
    'mozambique-flag',
    'myanmar',
    'namibia',
    'nauru',
    'nepal',
    'netherlands',
    'new-zealand',
    'nicaragua',
    'niger',
    'nigeria-flag',
    'niue',
    'north-korea',
    'northern-mariana-islands',
    'notification',
    'oman',
    'pakistan',
    'panama',
    'papua-new-guinea',
    'paraguay',
    'peru',
    'philippines',
    'pitcairn-islands',
    'poland',
    'portugal',
    'puerto-rico',
    'qatar',
    'romania',
    'russian-federation',
    'rwanda',
    'saint-kitts-and-nevis',
    'saint-lucia',
    'saint-vincent-and-the-grenadines',
    'samoa',
    'san-marino',
    'sao-tome-and-principe',
    'saudi-arabia',
    'scandinavian',
    'senegal',
    'serbia',
    'seychelles',
    'sierra-leone',
    'singapore',
    'slovakia',
    'slovenia',
    'solomon-islands',
    'somalia',
    'south-georgia-and-the-south-sandwich-islands',
    'south-korea',
    'south-sudan',
    'spain',
    'spain-flag',
    'sri-lanka',
    'sudan',
    'suriname',
    'swaziland',
    'sweden',
    'switzerland',
    'syria',
    'tajikistan',
    'tanzania',
    'thailand',
    'timor-leste',
    'togo',
    'tokelau',
    'tonga',
    'trinidad-and-tobago',
    'turkey',
    'turkmenistan',
    'turks-and-caicos-islands',
    'tuvalu',
    'uganda',
    'ukraine',
    'united-arab-emirates',
    'united-states-virgin-islands',
    'uruguay',
    'usa',
    'ussr',
    'uzbekistan',
    'vanuatu',
    'vatican-city',
    'venezuela',
    'vietnam',
    'western-sahara',
    'yemen',
    'zambia',
    'zimbabwe'
  );

  IconFlagsCode : array of string = (
    'ax,ala', { aland-islands }
    'al,alb', { albania }
    'dz,dza', { algeria }
    'ad,and', { andorra }
    'ao,ago', { angola }
    'ai,aia', { anguilla }
    'aq,ata', { antarctica }
    'ag,atg', { antigua-and-barbuda }
    'ar,arg', { argentina }
    'am,arm', { armenia }
    'aw,abw', { aruba }
    'au,aus', { australia }
    'at,aut', { austria }
    'az,aze', { azerbaijan }
    'bs,bhs', { bahamas }
    'bh,bhr', { bahrain }
    'bg,bgd', { bangladesh }
    'bb,brb', { barbados }
    'by,blr', { belarus }
    'be,bel', { belgium }
    'bz,blz', { belize }
    'bj,ben', { benin }
    'bm,bmu', { bermuda }
    'bt,btn', { bhutan }
    'bo,bol', { bolivia }
    'ba,bih', { bosnia-and-herzegovina }
    'bw,bwa', { botswana }
    'br,bra', { brazil }
    'io,iot', { british-indian-ocean-territory }
    'vg,vgb', { british-virgin-islands }
    'bn,brn', { brunei-darussalam }
    '', { bulgaria }
    '', {  burkina-faso }
    '', {  burundi }
    '', {  cambodia }
    '', {  cameroon }
    '', {  canada }
    '', {  cape-verde }
    '', {  cayman-islands }
    '', {  central-african-republic }
    '', {  chad }
    '', {  chile }
    '', {  china }
    '', {  christmas-island }
    '', {  cocos-keeling-islands }
    '', {  colombia }
    '', {  comoros }
    '', {  congo }
    '', {  cook-islands }
    '', {  costa-rica }
    '', {  croatia }
    '', {  cuba }
    '', {  curaao }
    '', {  cyprus }
    '', {  czech-republic }
    '', {  democratic-republic-congo }
    '', {  denmark }
    '', {  djibouti }
    '', {  dominica }
    '', {  dominican-republic }
    '', {  ecuador }
    '', {  egypt }
    '', {  el-salvador }
    '', {  equatorial-guinea }
    '', {  eritrea }
    '', {  estonia }
    '', {  ethiopia }
    '', {  falkland-islands }
    '', {  faroe-islands }
    '', {  fiji }
    '', {  finland }
    '', {  flag-of-afghanistan }
    'gf,guf', {  france }
    'pf,pyf', {  french-polynesia }
    'tf,atf', {  french-southern-territories }
    '', {  gabon }
    '', {  gambia }
    '', {  georgia }
    'de,deu', {  germany }
    '', {  ghana }
    '', {  gibraltar }
    'gb,gbr', {  great-britain }
    '', {  greece }
    '', {  greenland }
    '', {  grenada }
    '', {  guam }
    '', {  guatemala }
    '', {  guernsey }
    '', {  guinea }
    '', {  guinea-bissau }
    '', {  guyana }
    '', {  haiti-flag }
    '', {  honduras }
    '', {  hongkong-flag }
    '', {  hungary }
    '', {  iceland }
    '', {  india }
    '', {  indonesia }
    '', {  iran }
    '', {  iraq }
    '', {  ireland }
    '', {  isle-of-man }
    '', {  israel }
    '', {  italy }
    '', {  ivory-coast }
    '', {  jamaica }
    '', {  japan }
    '', {  jersey }
    '', {  jordan }
    '', {  kazakhstan }
    '', {  kenya }
    '', {  kiribati }
    '', {  kuwait }
    '', {  kyrgyzstan }
    '', {  laos }
    '', {  latvia }
    '', {  lebanon }
    '', {  lesotho }
    '', {  liberia }
    '', {  libya }
    '', {  liechtenstein }
    '', {  lithuania }
    '', {  luxembourg }
    '', {  macao }
    '', {  macedonia }
    '', {  madagacar }
    '', {  malawi }
    '', {  malaysia }
    '', {  maldives }
    '', {  mali }
    '', {  malta }
    '', {  marshall-islands }
    '', {  mauritania }
    '', {  mauritius }
    '', {  merge-git }
    '', {  mexico }
    '', {  micronesia }
    '', {  moldova }
    '', {  monaco }
    '', {  mongolia }
    '', {  montenegro }
    '', {  montserrat }
    '', {  morocco }
    '', {  mozambique-flag }
    '', {  myanmar }
    '', {  namibia }
    '', {  nauru }
    '', {  nepal }
    '', {  netherlands }
    '', {  new-zealand }
    '', {  nicaragua }
    '', {  niger }
    '', {  nigeria-flag }
    '', {  niue }
    '', {  north-korea }
    '', {  northern-mariana-islands }
    '', {  notification }
    '', {  oman }
    '', {  pakistan }
    '', {  panama }
    '', {  papua-new-guinea }
    '', {  paraguay }
    '', {  peru }
    '', {  philippines }
    '', {  pitcairn-islands }
    '', {  poland }
    '', {  portugal }
    '', {  puerto-rico }
    '', {  qatar }
    '', {  romania }
    '', {  russian-federation }
    '', {  rwanda }
    '', {  saint-kitts-and-nevis }
    '', {  saint-lucia }
    '', {  saint-vincent-and-the-grenadines }
    '', {  samoa }
    '', {  san-marino }
    '', {  sao-tome-and-principe }
    '', {  saudi-arabia }
    '', {  scandinavian }
    '', {  senegal }
    '', {  serbia }
    '', {  seychelles }
    '', {  sierra-leone }
    '', {  singapore }
    '', {  slovakia }
    '', {  slovenia }
    '', {  solomon-islands }
    '', {  somalia }
    '', {  south-georgia-and-the-south-sandwich-islands }
    '', {  south-korea }
    '', {  south-sudan }
    'es,esp', {  spain }
    '', {  spain-flag }
    '', {  sri-lanka }
    '', {  sudan }
    '', {  suriname }
    '', {  swaziland }
    'se,swe', {  sweden }
    'ch,che', {  switzerland }
    '', {  syria }
    '', {  tajikistan }
    '', {  tanzania }
    '', {  thailand }
    '', {  timor-leste }
    '', {  togo }
    '', {  tokelau }
    '', {  tonga }
    '', {  trinidad-and-tobago }
    'tr,tur', {  turkey }
    '', {  turkmenistan }
    '', {  turks-and-caicos-islands }
    '', {  tuvalu }
    '', {  uganda }
    'ua,ukr', {  ukraine }
    'ae,are', {  united-arab-emirates }
    'vi,vir', {  united-states-virgin-islands }
    '', {  uruguay }
    'us,usa', {  usa }
    '', {  ussr }
    '', {  uzbekistan }
    '', {  vanuatu }
    '', {  vatican-city }
    '', {  venezuela }
    '', {  vietnam }
    '', {  western-sahara }
    '', {  yemen }
    '', {  zambia }
    ''  {  zimbabwe }
  );

implementation

procedure Initialize;
var
  I : integer;
begin
  {$I icons.lrs}
  for I := Low(IconUI) to High(IconUI) do
      IconUI[I] := 'icons100-' + IconUI[I];
  for I := Low(IconFlags) to High(IconFlags) do
      IconFlags[I] := 'icons100-' + IconFlags[I];
end;

initialization

   Initialize;

end.

