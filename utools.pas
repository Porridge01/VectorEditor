unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, UFigures;

Type
  TTool = Class(TObject)
    public
      YColor: TColor; static;
      YWidth: Integer; static;
      Tools: array of TTool; static;
      SetImgBtn: TBitMap;
      constructor Create(PathToFile: String); overload;
      Class procedure addTool(Tool: TTool); static;
      class procedure SetColor(Color: TColor); static;
      class procedure SetWidth(Width: Integer); static;
      procedure OnMouseDown(Shift: TShiftState; X, Y: Integer);
       virtual; abstract;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       virtual; abstract;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       virtual; abstract;
  end;


  TTPen = Class(TTool)
    public
      procedure onMouseDown(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       override;
  end;

  TTErase = Class(TTool)
    public
      procedure onMouseDown(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       override;
  end;

  TTLine = Class(TTool)
    public
      procedure onMouseDown(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       override;
  end;

  TTPolyline = Class(TTool)
    public
      ClickMemory: boolean;
      procedure onMouseDown(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       override;
  end;

  TTRectangle = Class(TTool)
    public
      procedure onMouseDown(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       override;
  end;

  TTRoundRectangle = Class(TTool)
    public
      procedure onMouseDown(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       override;
  end;

  TTEllipse = Class(TTool)
    public
      procedure onMouseDown(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseMove(Shift: TShiftState; X, Y: Integer);
       override;
      procedure onMouseUp(Shift: TShiftState; X, Y: Integer);
       override;
  end;


implementation

constructor TTool.Create(PathToFile: string);
begin
  SetImgBtn:=TBitmap.Create;
  SetImgBtn.LoadFromFile(PathToFile);
end;

class procedure TTool.addTool(tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)]:= tool;
end;

class procedure TTool.SetColor(Color: TColor);
begin
  YColor:= Color;
end;

class procedure TTool.SetWidth(Width: Integer);
begin
  YWidth:= Width;
end;


procedure TTPen.onMouseDown(Shift: TShiftState; X, Y: Integer);
begin
     TFigure.addFigure(Tpen.Create(YColor, YWidth));
end;

procedure TTPen.onMouseMove(Shift: TShiftState; X, Y: Integer);
begin
    if not (ssLeft in Shift) and not(ssRight in Shift)  then Exit;
    (TFigure.RecentFigure() as Tpen).addPoint(point(X,Y));
end;

procedure TTPen.onMouseUp(Shift: TShiftState; X, Y: Integer);
begin

end;


procedure TTErase.onMouseDown(Shift: TShiftState; X, Y: Integer);
begin
     TFigure.addFigure(TErase.Create(YColor, YWidth));
end;

procedure TTErase.onMouseMove(Shift: TShiftState; X, Y: Integer);
begin
    if not (ssLeft in Shift) and not(ssRight in Shift)  then Exit;
    (TFigure.RecentFigure() as TErase).addPoint(point(X,Y));
end;

procedure TTErase.onMouseUp(Shift: TShiftState; X, Y: Integer);
begin

end;


procedure TTLine.onMouseDown(Shift: TShiftState; X, Y: Integer);
begin
    TFigure.addFigure(TLine.Create(YColor, YWidth));
    (TFigure.RecentFigure() as Tline).TopLeft:=point(X,Y);
end;

procedure TTLine.onMouseMove(Shift: TShiftState; X, Y: Integer);
begin
    if not (ssLeft in Shift) and not(ssRight in Shift)  then Exit;
    (TFigure.RecentFigure() as TLine).BottomRight:=point(X,Y);
end;

procedure TTLine.onMouseUp(Shift: TShiftState; X, Y: Integer);
begin
  (TFigure.RecentFigure() as TLine).BottomRight:=point(X,Y);
end;


procedure TTPolyline.onMouseDown(Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) then begin
     (Tfigure.RecentFigure() as TPolyline).RecentLine().BottomRight:=point(X,Y);
     ClickMemory:=False;
     Exit;
  end;

  if ClickMemory then
    (Tfigure.RecentFigure() as TPolyline).RecentLine().BottomRight:=point(X,Y)
  else
    ClickMemory:=True;
    TFigure.AddFigure(TPolyline.Create(YColor, YWidth));
    (TFIgure.RecentFigure() as TPolyline).addLine;
    (Tfigure.RecentFigure() as TPolyline).RecentLine().TopLeft:=point(X,Y);
  end;

procedure TTPolyline.onMouseMove(Shift: TShiftState; X, Y: Integer);
begin
    (TFigure.RecentFigure() as TPolyline).RecentLine().BottomRight:=point(X,Y);
end;

procedure TTPolyline.onMouseUp(Shift: TShiftState; X, Y: Integer);
begin


end;


procedure TTRectangle.onMouseDown(Shift: TShiftState; X, Y: Integer);
begin
     TFigure.addFigure(TRectangle.Create(YColor, YWidth));
    (TFigure.RecentFigure() as TRectangle).TopLeft:=point(X,Y);
end;

procedure TTRectangle.onMouseMove(Shift: TShiftState; X, Y: Integer);
begin
    if not (ssLeft in Shift) and not(ssRight in Shift)  then Exit;
    (TFigure.RecentFigure() as TRectangle).BottomRight:=point(X,Y);
end;

procedure TTRectangle.onMouseUp(Shift: TShiftState; X, Y: Integer);
begin
    (TFigure.RecentFigure() as TRectangle).BottomRight:=point(X,Y);
end;


procedure TTRoundRectangle.onMouseDown(Shift: TShiftState; X, Y: Integer);
begin
     TFigure.addFigure(TRoundRectangle.Create(YColor, YWidth));
    (TFigure.RecentFigure() as TRoundRectangle).TopLeft:=point(X,Y);
end;

procedure TTRoundRectangle.onMouseMove(Shift: TShiftState; X, Y: Integer);
begin
    if not (ssLeft in Shift) and not(ssRight in Shift)  then Exit;
    (TFigure.RecentFigure() as TRoundRectangle).BottomRight:=point(X,Y);
end;

procedure TTRoundRectangle.onMouseUp(Shift: TShiftState; X, Y: Integer);
begin
    (TFigure.RecentFigure() as TROundRectangle).BottomRight:=point(X,Y);
end;


procedure TTEllipse.onMouseDown(Shift: TShiftState; X, Y: Integer);
begin
     TFigure.addFigure(TEllipse.Create(YColor, YWidth));
    (TFigure.RecentFigure() as TEllipse).TopLeft:=point(X,Y);
end;

procedure TTEllipse.onMouseMove(Shift: TShiftState; X, Y: Integer);
begin
    if not (ssLeft in Shift) and not(ssRight in Shift)  then Exit;
    (TFigure.RecentFigure() as TEllipse).BottomRight:=point(X,Y);
end;

procedure TTEllipse.onMouseUp(Shift: TShiftState; X, Y: Integer);
begin
    (TFigure.RecentFigure() as TEllipse).BottomRight:=point(X,Y);
end;

initialization
TTool.addTool(TTPen.Create('img\pen.bmp'));
TTool.addTool(TTErase.Create('img\Erease.bmp'));
TTool.addTool(TTLine.Create('img\line.bmp'));
TTool.addTool(TTPolyline.Create('img\polyline.bmp'));
TTool.addTool(TTRectangle.Create('img\rectangle.bmp'));
TTool.addTool(TTRoundRectangle.Create('img\RoundRectangle.bmp'));
TTool.addTool(TTEllipse.Create('img\Ellipse.bmp'));

end.

