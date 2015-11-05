unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, Menus, ExtDlgs, StdCtrls, Grids, UTools,
  UFigures, UAbout;

type

  { TMainForm }

  TMainForm = class(TForm)
    FillColorDialog: TColorDialog;
    PenColorDialog: TColorDialog;
    DrawGrid: TDrawGrid;
    PalettePanel: TPanel;
    Panel1: TPanel;
    PColorPanel: TPanel;
    FillColorPanel: TPanel;
    SetWidthBox: TComboBox;
    Save: TMenuItem;
    MMenu: TMainMenu;
    Fail: TMenuItem;
    Help: TMenuItem;
    Dlg1: TSavePictureDialog;
    SaveDialog1: TSaveDialog;
    YExit: TMenuItem;
    About: TMenuItem;
    PaintBox: TPaintBox;
    Panel: TPanel;
    ScrollBox: TScrollBox;
    Stat1: TStatusBar;
    procedure AboutClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FillColorPanelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PColorPanelClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SetColor(Shift: TShiftState);
    procedure SetWidthBoxChange(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure YExitClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  IndexOfBtn: integer;
  SetBtn: TBitBtn;

const
  ArColors: array[0..12,0..1] of TColor = ((clWhite, clSilver), (clBlack, clMaroon),
  (clGreen, clOlive), (clNavy, clPurple), (clTeal, clRed), (clLime, clYellow),
  (clBlue, clFuchsia), (clAqua, clGray), (clSkyBlue,clMedGray),
  (clCream,clMoneyGreen), (clLtGray, clHotLight), (clWindowText, clHighlight),
  (clWindowFrame, clActiveCaption));

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to high(TTool.Tools) do begin
    SetBtn:=TBitBtn.Create(Self);
    with SetBtn do begin
      Name:=TTool.Tools[i].ToString+IntToStr(i);
      Caption:='';
      Parent:=Self;
      Spacing:=1;
      Width:=25;
      Height:=25;
      Glyph:=TTool.Tools[i].SetImgBtn;
      Top:=10+(i div 2) * 35;
      Left:=10+(i mod 2) * 35;
      OnClick:= @mainForm.ToolClick;
      Tag:=i;
      if i = 0 then SetBtn.Click;
    end;
  end;
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (ssRight in Shift) then Exit;
  SetColor(Shift);
  TTool.Tools[IndexOfBtn].OnMouseDown(Shift, X,Y);
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  stat1.Panels[1].text := 'x:'+inttostr(x)+ '  y:'+inttostr(y);
  if (ssLeft in Shift) or (ssRight in Shift) then begin
  TTool.Tools[IndexOfBtn].OnMouseMove(Shift, X,Y);
  Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (ssRight in Shift) then Exit;
  TTool.Tools[IndexOfBtn].OnMouseUp(Shift, X,Y);
end;

procedure TMainForm.AboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with DrawGrid.Canvas do begin
    Brush.Color:= ArColors[aCol,aRow];
    FillRect(aRect);
  end;
end;

procedure TMainForm.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: integer;
begin
  DrawGrid.MouseToCell(X, Y, Col, Row);
  if Shift = [ssLeft] then
    begin
      PaintBox.Canvas.Pen.Color := ArColors[Col, Row];
      PColorPanel.Color := ArColors[Col, Row];
      PenColorDialog.Color := ArColors[Col, Row];
    end;
    if (Shift = [ssRight]) then
    begin
      PaintBox.Canvas.Brush.Color := ArColors[Col, Row];
      FillColorPanel.Color := ArColors[Col, Row];
      FillColorDialog.Color := ArColors[Col, Row];
    end;
end;

procedure TMainForm.FillColorPanelClick(Sender: TObject);
begin
  FillColorDialog.Execute;
  PaintBox.Canvas.Brush.Color := FillColorDialog.Color;
  FillColorPanel.Color := FillColorDialog.Color;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var figure: TFigure;
begin
  for figure in TFigure.YFigures do
      figure.Draw(PaintBox.Canvas);
end;

procedure TMainForm.PColorPanelClick(Sender: TObject);
begin
  PenColorDialog.Execute;
  PaintBox.Canvas.Pen.Color := PenColorDialog.Color;
  PColorPanel.Color := PenColorDialog.Color;
end;

procedure TMainForm.SaveClick(Sender: TObject);
var bmp1: TBitmap;
    Dest, Source: TRect;
begin
  Dlg1.Execute;
  bmp1 := TBitmap.Create;
  try
    with bmp1 do
    begin
      Width   := PaintBox.Width;
      Height  := PaintBox.Height;
      Dest    := Rect(0, 0, Width, Height);
    end;
    with PaintBox do
      Source := Rect(0, 0, Width, Height);
      bmp1.Canvas.CopyRect(Dest, PaintBox.Canvas, Source);
      bmp1.SaveToFile(Dlg1.FileName);
  finally
    bmp1.Free;
  end;
end;

procedure TMainForm.SetColor(Shift: TShiftState);
begin
  if Shift = [ssLeft] then
  TTool.SetColor(PColorPanel.Color);
  if Shift = [ssRight] then
  TTool.SetColor(FillColorPanel.Color);
end;

procedure TMainForm.SetWidthBoxChange(Sender: TObject);
begin
  TTool.SetWidth(StrToInt(SetWidthBox.Text));
end;

procedure TMainForm.ToolClick(Sender: TObject);
begin
  IndexOfBtn:=(Sender as TBitBtn).Tag;
end;

procedure TMainForm.YExitClick(Sender: TObject);
begin
  MainForm.Close;
end;

end.

