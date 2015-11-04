unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, Menus, ExtDlgs, ColorBox, StdCtrls, UTools, UFigures,
  UAbout;

type

  { TMainForm }

  TMainForm = class(TForm)
    SetColorBox: TColorBox;
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
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SetColorBoxChange(Sender: TObject);
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
  TTool.Tools[IndexOfBtn].OnMouseDown(Shift, X,Y);
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  stat1.Panels[1].text := 'x:'+inttostr(x)+ '  y:'+inttostr(y);
  if (ssLeft in Shift) or (ssRight in Shift) then begin
  TTool.Tools[IndexOfBtn].OnMouseMove(Shift, X,Y);
  PaintBox.Invalidate;
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

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var figure: TFigure;
begin
  for figure in TFigure.YFigures do
      figure.Draw(PaintBox.Canvas);
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

procedure TMainForm.SetColorBoxChange(Sender: TObject);
begin
  TTool.SetColor(SetColorBox.Selected);
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

