unit uLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PasExt, PUIExt, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls, IDEWindowIntf;

type

  { TfLog }

  TfLog = class(TForm)
    LogText: TMemo;
    xProperties: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
  private
    FAutoShow: boolean;
    procedure SetAutoShow(AValue: boolean);

  public
    procedure Add(AMessage : String);
  published
    property AutoShow : boolean read FAutoShow write SetAutoShow;
  end;

var
  fLog: TfLog;

procedure Log(Sender : TForm; Message : String); overload;

implementation

procedure Log(Sender: TForm; Message: String);
begin
  if not Assigned(fLog) then exit;
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
   FAutoShow := True;
end;

procedure TfLog.SetAutoShow(AValue: boolean);
begin
  if FAutoShow=AValue then Exit;
  FAutoShow:=AValue;
end;

procedure TfLog.Add(AMessage: String);
begin
   LogText.Append(AMessage);
end;

end.

