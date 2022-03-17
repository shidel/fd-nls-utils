unit DosView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, PASExt;

type

  { TCRTView }

  { TDosScreen }

  TDosScreen = class (TPersistent)
  private
    FBackground: TColor;
    FBitmap: TBitmap;
    FBorder: integer;
    FBorderColor: TColor;
    FFont: TArrayOfBytes;
    FScreenMax: TPoint;
    FTextColor: TColor;
    FWhereXY: TPoint;
    procedure SetBackground(AValue: TColor);
    procedure SetBorder(AValue: integer);
    procedure SetBorderColor(AValue: TColor);
    procedure SetFont(AValue: TArrayOfBytes);
    procedure SetScreenMax(AValue: TPoint);
    procedure SetTextColor(AValue: TColor);
    procedure SetWhereXY(AValue: TPoint);
  protected
    procedure AdjustSize;
  public
    constructor Create(MaxX : Integer = 80; MaxY : Integer = 25); virtual;
    destructor Destroy; override;
    property Bitmap : TBitmap read FBitmap;
    property Font : TArrayOfBytes read FFont write SetFont;
    function FontHeight : integer;
    function FontWidth : integer;
    property Border : integer read FBorder write SetBorder;
    property ScreenMax : TPoint read FScreenMax write SetScreenMax;
    property WhereXY : TPoint read FWhereXY write SetWhereXY;
    property TextColor : TColor read FTextColor write SetTextColor;
    property Background : TColor read FBackground write SetBackground;
    property BorderColor : TColor read FBorderColor write SetBorderColor;
    procedure ClearScreen;
    procedure PutChar(C : Char);
    procedure PutString(S : String);
    procedure WriteText(S : String);
    procedure GotoXY(X, Y : integer); overload;
    procedure GotoXY(XY : TPoint); overload;
  published

  end;

implementation

{ TDosScreen }

procedure TDosScreen.SetScreenMax(AValue: TPoint);
begin
  if FScreenMax=AValue then Exit;
  FScreenMax:=AValue;
  AdjustSize;
end;

procedure TDosScreen.SetTextColor(AValue: TColor);
begin
  if FTextColor=AValue then Exit;
  FTextColor:=AValue;
end;

procedure TDosScreen.SetFont(AValue: TArrayOfBytes);
begin
  // if FFont=AValue then Exit;
  FFont:=AValue;
  AdjustSize;
end;

procedure TDosScreen.SetBackground(AValue: TColor);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
end;

procedure TDosScreen.SetBorder(AValue: integer);
begin
  if FBorder=AValue then Exit;
  FBorder:=AValue;
  AdjustSize;
end;

procedure TDosScreen.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  ClearScreen;
end;

procedure TDosScreen.SetWhereXY(AValue: TPoint);
begin
  if FWhereXY=AValue then Exit;
  FWhereXY:=AValue;
end;

procedure TDosScreen.AdjustSize;
begin
  if FontHeight = 0 then
    FBitMap.SetSize(FontWidth * FScreenMax.X + Border * 2, 8 * FScreenMax.Y + Border * 2)
  else
    FBitMap.SetSize(FontWidth * FScreenMax.X + Border * 2, FontHeight * FScreenMax.Y + Border * 2);
  ClearScreen;
end;

function TDosScreen.FontHeight: integer;
begin
    Result := Length(FFont) div 256;
end;

function TDosScreen.FontWidth: integer;
begin
  Result := 8;
end;

constructor TDosScreen.Create(MaxX : Integer = 80; MaxY : Integer = 25);
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  ScreenMax.SetLocation(MaxX,MaxY);
end;

destructor TDosScreen.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TDosScreen.ClearScreen;
begin
  if FBorder > 0 then begin
    FBitMap.Canvas.Brush.Color:= FBorderColor;
    FBitMap.Canvas.FillRect(0, 0, FBitMap.Width, FBitMap.Height);
  end;
  FBitMap.Canvas.Brush.Color:= FBackground;
  FBitMap.Canvas.FillRect(FBorder, FBorder,
    FBitMap.Width - FBorder, FBitMap.Height - FBorder);
  WhereXY.SetLocation(1,1);
end;

procedure TDosScreen.PutChar(C: Char);
var
  S, V : integer;
  XX, YY : integer;
  X, Y : integer;
begin
  try
    S := Ord(C);
    S := S * FontHeight;
    XX := FontWidth * (FWhereXY.X - 1) + FBorder;
    YY := FontHeight * (FWhereXY.Y - 1) + FBorder;
    for Y := 0 to FontHeight - 1 do begin
       V := Font[S + Y];
       for X := 0 to 7 do
         if ((V shr (7 - X)) and 1 = 1) then
           FBitmap.Canvas.Pixels[XX + X, YY + Y] := FTextColor
         else
           FBitmap.Canvas.Pixels[XX + X, YY + Y] := FBackground;
    end;
  finally
    Inc(FWhereXY.X);
    if FWhereXY.X > FScreenMax.X then begin
      FWhereXY.X := 1;
      Inc(FWhereXY.Y);
      if FWhereXY.Y > FScreenMax.Y then FWhereXY.Y := 1;
    end;
  end;
end;

procedure TDosScreen.PutString(S: String);
var
  I : integer;
begin
  for I := 1 to Length(S) do
      PutChar(S[I]);
end;

procedure TDosScreen.WriteText(S: String);
var
  I : integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      CR : begin
        // ignore them
      end;
      LF : begin
         FWhereXY.X := 1;
         Inc(FWhereXY.Y);
         if FWHereXY.Y > FScreenMax.Y then FWhereXY.Y := 1
      end;
    else
      PutChar(S[I]);
    end;
end;

procedure TDosScreen.GotoXY(X, Y: integer);
begin
  FWhereXY.X:=X;
  FWhereXY.Y:=Y;
end;

procedure TDosScreen.GotoXY(XY: TPoint);
begin
  FWhereXY := XY;
end;

end.

