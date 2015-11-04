unit Ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons;
Type
  TFigure = Class(TObject)
    private
      YFigures: array of TFigure; static;
      YColor: TColor;
      YWidth: integer;
    public
      constructor Create(SetColor: TColor; SetWidth: integer);
      class procedure addFigure(figure: TFigure); static;
      class function RecentFigure(): TFigure; static;
      procedure Draw(Canvas: TCanvas); virtual; abstract;
  end;

  TPen = Class(TFigure)
    private
      YPoints: array of TPoint;
    public
      procedure addPoint(point: TPoint);
      procedure Draw(Canvas: TCanvas); override;
  end;

  TErase = Class(TFigure)
    private
      YPoints: array of TPoint;
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
      YLines: array of TLine;
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
  YColor:= SetColor;
  YWidth:= SetWidth;
end;

class procedure TFigure.addFigure(figure: TFigure);
begin
  SetLength(TFigure.YFigures, Length(Tfigure.Yfigures)+1);
  TFigure.YFigures[High(TFigure.YFigures)]:= figure;
end;

class function TFigure.RecentFigure(): TFigure;
begin
  result:=YFigures[High(YFigures)];
end;

procedure TPen.addPoint(point: TPoint);
begin
  SetLength(YPoints, Length(YPoints) + 1);
  YPoints[High(YPoints)]:= point;
end;

procedure TPen.Draw(Canvas: TCanvas);
var Point: TPOint;
begin
  with Canvas do begin
    Pen.Color:=YColor;
    Pen.Width:=YWidth;
    MoveTO(YPoints[0]);
    for point in YPoints do
        lineTo(point);
  end;
end;

procedure TErase.addPoint(point: TPoint);
begin
  SetLength(YPoints, Length(YPoints) + 1);
  YPoints[High(YPoints)]:= point;
end;

procedure TErase.Draw(Canvas: TCanvas);
var Point: Tpoint;
begin
  with Canvas do begin
    Pen.Color:=clWhite;
    Pen.Width:=15;
    MoveTO(YPoints[0]);
    for point in YPoints do
        lineTo(point);
    //Pen.Color:=clBlack;
    //Pen.Width:=1;
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=YColor;
    Pen.Width:=YWidth;
    MoveTO(TopLeft);
    LineTo(BottomRight);
  end;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var YLine: Tline;
begin
  Canvas.Pen.Color:=YColor;
  CAnvas.Pen.Width:=YWidth;
  for Yline in YLines do
      with Canvas do begin
        MoveTo(Yline.TopLeft);
        LineTo(Yline.BottomRight);
      end;
end;

procedure TPolyline.AddLine();
begin
  SetLength(YLines, Length(YLines) + 1);
  YLines[High(YLines)]:= TLine.Create(YColor, YWidth);
end;

function TPolyline.RecentLine(): Tline;
begin
  result:=YLines[High(YLines)];
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=YColor;
    Pen.Width:=YWidth;
    Brush.Style:=bsClear;
    Rectangle(TopLeft.X, TopLeft.y, BottomRight.X, BottomRight.Y);
  end;
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=YColor;
    Pen.Width:=YWidth;
    Brush.Style:=bsClear;
    RoundRect(TopLeft.X, TopLeft.y, BottomRight.X, BottomRight.Y, 20, 20);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  with Canvas do begin
    Pen.Color:=YColor;
    Pen.Width:=YWidth;
    Brush.Style:=bsClear;
    Ellipse(TopLeft.X, TopLeft.y, BottomRight.X, BottomRight.Y);
  end;
end;

end.

