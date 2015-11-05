unit Ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, Interfaces;
Type
  {PFloatPoint = ^TFloatPoint;
  TFloatPoint = record
    X,Y: double;
  end;

  ICoordConvert = Interface
  ['{2FACBF76-4176-4037-8AE9-3A0F45643521}']
    procedure LogToScreen(lX, lY: integer; var sX, sY: double);  overload;
    procedure ScreenToLog(sX, sY: double; var lX, lY: integer); overload;
    function LogToScreen(Value: double): integer; overload;
    function ScreenToLog(Value: integer): double; overload;
  end; }

  TFigure = Class(TObject)
    private
      FFigures: array of TFigure; static;
      FColor: TColor;
      FWidth: integer;
    public
      constructor Create(SetColor: TColor; SetWidth: integer);
      class procedure addFigure(figure: TFigure); static;
      class function RecentFigure(): TFigure; static;
      procedure Draw(Canvas: TCanvas); virtual; abstract;
  end;

  TPen = Class(TFigure)
    private
      FPoints: array of TPoint;
    public
      procedure addPoint(point: TPoint);
      procedure Draw(Canvas: TCanvas); override;
  end;

  TErase = Class(TFigure)
    private
      FPoints: array of TPoint;
    public
      procedure addPoint(point: TPoint);
      procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = Class(TFigure)
    public
      TopLeft, BottomRight: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TPolyline = Class(TFigure)
    private
      FLines: array of TLine;
    public
      procedure AddLine();
      function RecentLine(): TLine;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TRectangle = Class(TFigure)
    public
      TopLeft, BottomRight: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRectangle = Class(TFigure)
    public
      TopLeft, BottomRight: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = Class(TFigure)
    public
      TopLeft, BottomRight: TPoint;
      procedure Draw(Canvas: TCanvas); override;
  end;

implementation

constructor TFigure.Create(SetColor: TColor; SetWidth: Integer);
begin
  FColor:= SetColor;
  FWidth:= SetWidth;
end;

class procedure TFigure.addFigure(figure: TFigure);
begin
  SetLength(TFigure.FFigures, Length(Tfigure.FFigures)+1);
  TFigure.FFigures[High(TFigure.FFigures)]:= figure;
end;

class function TFigure.RecentFigure(): TFigure;
begin
  result:=FFigures[High(FFigures)];
end;

procedure TPen.addPoint(point: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= point;
end;

procedure TPen.Draw(Canvas: TCanvas);
var Point: TPoint;
begin
  with Canvas do begin
    Pen.Color:=FColor;
    Pen.Width:=FWidth;
    MoveTO(FPoints[0]);
    for point in FPoints do
      LineTo(point);
  end;
end;

procedure TErase.addPoint(point: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)]:= point;
end;

procedure TErase.Draw(Canvas: TCanvas);
var Point: Tpoint;
begin
  with Canvas do begin
    Pen.Color:=clWhite;
    Pen.Width:=15;
    MoveTO(FPoints[0]);
    for point in FPoints do
      LineTo(point);
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=FColor;
    Pen.Width:=FWidth;
    MoveTO(TopLeft);
    LineTo(BottomRight);
  end;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var FLine: Tline;
begin
  Canvas.Pen.Color:=FColor;
  CAnvas.Pen.Width:=FWidth;
  for FLine in FLines do
    Fline.Draw(Canvas);
end;

procedure TPolyline.AddLine();
begin
  SetLength(FLines, Length(FLines) + 1);
  FLines[High(FLines)]:= TLine.Create(FColor, FWidth);
end;

function TPolyline.RecentLine(): Tline;
begin
  result:=FLines[High(FLines)];
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=FColor;
    Pen.Width:=FWidth;
    Brush.Style:=bsClear;
    Rectangle(TopLeft.X, TopLeft.y, BottomRight.X, BottomRight.Y);
  end;
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=FColor;
    Pen.Width:=FWidth;
    Brush.Style:=bsClear;
    RoundRect(TopLeft.X, TopLeft.y, BottomRight.X, BottomRight.Y, 20, 20);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=FColor;
    Pen.Width:=FWidth;
    Brush.Style:=bsClear;
    Ellipse(TopLeft.X, TopLeft.y, BottomRight.X, BottomRight.Y);
  end;
end;

end.

