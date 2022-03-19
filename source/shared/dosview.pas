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
    FWindMax: TPoint;
    FWindMin: TPoint;
    function GetWhereX: integer;
    function GetWhereXY: TPoint;
    function GetWhereY: integer;
    procedure SetBackground(AValue: TColor);
    procedure SetBorder(AValue: integer);
    procedure SetBorderColor(AValue: TColor);
    procedure SetFont(AValue: TArrayOfBytes);
    procedure SetScreenMax(AValue: TPoint);
    procedure SetTextColor(AValue: TColor);
    procedure SetWhereX(AValue: integer);
    procedure SetWhereXY(AValue: TPoint);
    procedure SetWhereY(AValue: integer);
    procedure SetWindMax(AValue: TPoint);
    procedure SetWindMin(AValue: TPoint);
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
    property WindMin : TPoint read FWindMin write SetWindMin;
    property WindMax : TPoint read FWindMax write SetWindMax;
    property WhereXY : TPoint read GetWhereXY write SetWhereXY;
    property WhereX  : integer read GetWhereX write SetWhereX;
    property WhereY : integer read GetWhereY write SetWhereY;
    property TextColor : TColor read FTextColor write SetTextColor;
    property Background : TColor read FBackground write SetBackground;
    property BorderColor : TColor read FBorderColor write SetBorderColor;
    procedure ClearScreen;
    procedure Window(x1, y1, x2, y2 : integer); overload;
    procedure PutChar(C : Char);
    procedure PutString(S : String);
    procedure WriteText(S : String);
    procedure WriteTextLn(S : String);
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
  FWindMin.SetLocation(1,1);
  FWindMax.SetLocation(FScreenMax);
  FWhereXY.SetLocation(1,1);
  AdjustSize;
end;

procedure TDosScreen.SetTextColor(AValue: TColor);
begin
  if FTextColor=AValue then Exit;
  FTextColor:=AValue;
end;

procedure TDosScreen.SetWhereX(AValue: integer);
begin
  FWhereXY.X := AValue;
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

function TDosScreen.GetWhereXY: TPoint;
begin
  Result.SetLocation(FWhereXY);
end;

function TDosScreen.GetWhereX: integer;
begin
  Result := FWhereXY.X;
end;

function TDosScreen.GetWhereY: integer;
begin
  Result := FWhereXY.Y;
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
  FWhereXY.SetLocation(AValue);
end;

procedure TDosScreen.SetWhereY(AValue: integer);
begin
  FWhereXY.Y:=AValue;
end;

procedure TDosScreen.SetWindMax(AValue: TPoint);
begin
  if FWindMax=AValue then Exit;
  FWindMax:=AValue;
  FWhereXY.SetLocation(1,1);
end;

procedure TDosScreen.SetWindMin(AValue: TPoint);
begin
  if FWindMin=AValue then Exit;
  FWindMin:=AValue;
  FWhereXY.SetLocation(1,1);
end;

procedure TDosScreen.AdjustSize;
begin
  if FontHeight = 0 then
    FBitMap.SetSize(FontWidth * FScreenMax.X + Border * 2, 16 * FScreenMax.Y + Border * 2)
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
  Window(1,1,FScreenMax.X,FScreenMax.Y);
end;

procedure TDosScreen.Window(x1, y1, x2, y2: integer);
begin
  FWindMin.SetLocation(x1, y1);
  FWindMax.SetLocation(x2, y2);
  FWhereXY.SetLocation(1,1);
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
    XX := FontWidth * (FWhereXY.X - 1 + FWindMin.X - 1) + FBorder;
    YY := FontHeight * (FWhereXY.Y - 1 + FWindMin.Y - 1) + FBorder;
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
    if (FWhereXY.X + FWindMin.X -  1 > FWindMax.X)  then begin
      FWhereXY.X := 1;
      Inc(FWhereXY.Y);
      if (FWhereXY.Y + FWindMin.Y - 1 > FWindMax.Y) then FWindMax.Y := 1;
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
         if (FWhereXY.Y + FWindMin.Y - 1 > FWindMax.Y) then FWhereXY.Y := 1
      end;
    else
      PutChar(S[I]);
    end;
end;

procedure TDosScreen.WriteTextLn(S: String);
begin
  WriteText(S + LF);
end;


procedure TDosScreen.GotoXY(X, Y: integer);
begin
  FWhereXY.X:=X;
  FWhereXY.Y:=Y;
end;

procedure TDosScreen.GotoXY(XY: TPoint);
begin
  FWhereXY.SetLocation(XY);
end;

end.

