unit uLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PasExt, PUIExt, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls;

type

  { TfLog }

  TfLog = class(TForm)
    LogText: TMemo;
    xProperties: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAutoShow: boolean;
    procedure SetAutoShow(AValue: boolean);

  public
    procedure Add(AMessage : String; TimeStamp : boolean = true);
  published
    property AutoShow : boolean read FAutoShow write SetAutoShow;
  end;

var
  fLog: TfLog;

procedure Log(Sender : TForm; Message : String); overload;

implementation

var
  PreLog : TStringList;

procedure Log(Sender: TForm; Message: String);
begin
  if not Assigned(fLog) then begin
     if Assigned(PreLog) then begin
          PreLog.Append(Message);
     end;
     exit;
  end;
  with fLog do begin
       if (not Visible) and (AutoShow) then Show;
       Add(Message);
  end;
end;

{$R *.lfm}

{ TfLog }

procedure TfLog.FormCreate(Sender: TObject);
begin
   xProperties.FileName := AppCfgFile;
   xProperties.RootNodePath := FormNodePath(Self);
   LogText.Clear;
   Add('Log started on ' + FormatDateTime('yyyy-mm-dd', Now));
   Add('');
   if Assigned(PreLog) then begin
      LogText.Append(PreLog.Text);
      Add('');
      FreeAndNil(PreLog);
   end;
   FAutoShow := False;
end;

procedure TfLog.FormShow(Sender: TObject);
begin
  Position:=poDesigned;
end;

procedure TfLog.SetAutoShow(AValue: boolean);
begin
  if FAutoShow=AValue then Exit;
  FAutoShow:=AValue;
end;

procedure TfLog.Add(AMessage: String; TimeStamp : boolean = true);
begin
   if TimeStamp and (AMessage <> '') then
     LogText.Append(FormatDateTime('hh:mm:ss.zzz', Now) + TAB + ' ' + AMessage)
   else
     LogText.Append(AMessage);
end;

initialization
  PreLog := TStringList.Create;

end.

