unit uProperty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, Buttons, Spin, StdCtrls, Types;

type

  TProperty = class
    ParentPanel: TPanel;
    procedure ObjectsCreate virtual; abstract;
  end;

  TPenStyleProperty = class(TProperty)
    ToolPenStyle: TComboBox;
    procedure ObjectsCreate override;
    procedure PenStyleChange(Sender: TObject);
  end;

  TBrushStyleProperty = class(TProperty)
    ToolBrushStyle: TComboBox;
    procedure ObjectsCreate override;
    procedure BrushStyleChange(Sender: TObject);
  end;

  TWidthProperty = class(TProperty)
    ToolWidth: TSpinEdit;
    procedure ObjectsCreate override;
    procedure WidthChange(Sender: TObject);
  end;

  TPenColorProperty = class(TProperty)
    ToolColor: TColorButton;
    procedure ObjectsCreate override;
    procedure ColorChange(Sender: TObject);
  end;

  TBrushColorProperty = class(TProperty)
    ToolColor: TColorButton;
    procedure ObjectsCreate override;
    procedure ColorChange(Sender: TObject);
  end;

  TRoundProperty = class(TProperty)
    ToolRoundX: TSpinEdit;
    ToolRoundY: TSpinEdit;
    procedure ObjectsCreate override;
    procedure RoundChangeX(Sender: TObject);
    procedure RoundChangeY(Sender: TObject);
  end;

const
  Pen_Styles: array [0..4] of string =
    (
    'psSolid', 'psDash', 'psDot',
    'psDashDot', 'psDashDotDot'
    );
  Brush_Styles: array [0..7] of string =
    (
    'bsClear', 'bsSolid', 'bsHorizontal',
    'bsVertical', 'bsFDiagonal', 'bsBDiagonal',
    'bsCross', 'bsDiagCross'
    );

var
  PWidth: byte = 1;
  PRoundX: byte = 50;
  PRoundY: byte = 50;
  PPenColor: TColor = ClBlack;
  PBrushColor: TColor = ClWhite;
  PTop: integer = 0;
  PPenStyle: TPenStyle = psSolid;
  PBrushStyle: TBrushStyle = bsClear;
  IndexPPenStyle: byte = 0;
  IndexPBrushStyle: byte = 0;

implementation

procedure TPenStyleProperty.ObjectsCreate;
var
  i: integer;
  PenStyleLabel: TLabel;
begin
  PenStyleLabel := TLabel.Create(ParentPanel);
  PenStylelabel.Caption := 'PenStyle';
  PenStyleLabel.Parent := ParentPanel;
  PenStyleLabel.Top := PTop;
  PTop := PTop + PenStyleLabel.Height;

  ToolPenStyle := TComboBox.Create(ParentPanel);
  ToolPenStyle.Top := PTop;
  ToolPenStyle.Name := 'ToolPenStyle';
  ToolPenStyle.Width := ParentPanel.Width;
  for i := 0 to High(Pen_Styles) do
    ToolPenStyle.Items.Add(Pen_Styles[i]);
  ToolPenStyle.ItemIndex := IndexPPenStyle;
  ToolPenStyle.Parent := ParentPanel;
  ToolPenStyle.OnChange := @PenStyleChange;
  PTop := PTop + ToolPenStyle.Height;
end;

procedure TPenStyleProperty.PenStyleChange(Sender: TObject);
begin
  case ToolPenStyle.ItemIndex of
    0: PPenStyle := psSolid;
    1: PPenStyle := psDash;
    2: PPenStyle := psDot;
    3: PPenStyle := psDashDot;
    4: PPenStyle := psDashDotDot;
  end;
  IndexPPenStyle := ToolPenStyle.ItemIndex;
end;

procedure TBrushStyleProperty.ObjectsCreate;
var
  i: integer;
  BrushStyleLabel: TLabel;
begin
  BrushStyleLabel := TLabel.Create(ParentPanel);
  BrushStylelabel.Caption := 'BrushStyle';
  BrushStyleLabel.Parent := ParentPanel;
  BrushStyleLabel.Top := PTop;
  PTop := PTop + BrushStyleLabel.Height;

  ToolBrushStyle := TComboBox.Create(ParentPanel);
  ToolBrushStyle.Top := PTop;
  ToolBrushStyle.Name := 'ToolBrushStyle';
  ToolBrushStyle.Width := ParentPanel.Width;
  for i := 0 to High(Brush_Styles) do
    ToolBrushStyle.Items.Add(Brush_Styles[i]);
  ToolBrushStyle.ItemIndex := IndexPBrushStyle;
  ToolBrushStyle.Parent := ParentPanel;
  ToolBrushStyle.OnChange := @BrushStyleChange;
  PTop := PTop + ToolBrushStyle.Height;
end;

procedure TBrushStyleProperty.BrushStyleChange(Sender: TObject);
begin
  case ToolBrushStyle.ItemIndex of
    0: PBrushStyle := bsClear;
    1: PBrushStyle := bsSolid;
    2: PBrushStyle := bsHorizontal;
    3: PBrushStyle := bsVertical;
    4: PBrushStyle := bsFDiagonal;
    5: PBrushStyle := bsBDiagonal;
    6: PBrushStyle := bsCross;
    7: PBrushStyle := bsDiagCross;
  end;
  IndexPBrushStyle := ToolBrushStyle.ItemIndex;
end;

procedure TWidthProperty.ObjectsCreate;
var
  WidthLabel: TLabel;
begin
  WidthLabel := TLabel.Create(ParentPanel);
  WidthLabel.Caption := 'Width';
  WidthLabel.Parent := ParentPanel;
  WidthLabel.Top := PTop;
  PTop := PTop + WidthLabel.Height;

  ToolWidth := TSpinEdit.Create(ParentPanel);
  ToolWidth.Top := PTop;
  ToolWidth.Name := 'ToolWidth';
  ToolWidth.Width := ParentPanel.Width;
  ToolWidth.MaxValue := 255;
  ToolWidth.MinValue := 1;
  ToolWidth.Value := PWidth;
  ToolWidth.Parent := ParentPanel;
  ToolWidth.OnChange := @WidthChange;
  PTop := PTop + ToolWidth.Height;
end;

procedure TWidthProperty.WidthChange(Sender: TObject);
begin
  PWidth := ToolWidth.Value;
end;

procedure TPenColorProperty.ObjectsCreate;
var
  ColorLabel: TLabel;
begin
  ColorLabel := TLabel.Create(ParentPanel);
  ColorLabel.Caption := 'PenColor';
  ColorLabel.Parent := ParentPanel;
  ColorLabel.Top := PTop;
  PTop := PTop + ColorLabel.Height;

  ToolColor := TColorButton.Create(ParentPanel);
  ToolColor.Top := PTop;
  ToolColor.Name := 'ToolPenColor';
  ToolColor.Width := ParentPanel.Width;
  ToolColor.ButtonColor := PPenColor;
  ToolColor.Parent := ParentPanel;
  ToolColor.OnColorChanged := @ColorChange;
  PTop := PTop + ToolColor.Height;
end;

procedure TPenColorProperty.ColorChange(Sender: TObject);
begin
  PPenColor := ToolColor.ButtonColor;
end;

procedure TBrushColorProperty.ObjectsCreate;
var
  ColorLabel: TLabel;
begin
  ColorLabel := TLabel.Create(ParentPanel);
  ColorLabel.Caption := 'BrushColor';
  ColorLabel.Parent := ParentPanel;
  ColorLabel.Top := PTop;
  PTop := PTop + ColorLabel.Height;

  ToolColor := TColorButton.Create(ParentPanel);
  ToolColor.Top := PTop;
  ToolColor.Name := 'ToolBrushColor';
  ToolColor.Width := ParentPanel.Width;
  ToolColor.ButtonColor := PBrushColor;
  ToolColor.Parent := ParentPanel;
  ToolColor.OnColorChanged := @ColorChange;
  PTop := PTop + ToolColor.Height;
end;

procedure TBrushColorProperty.ColorChange(Sender: TObject);
begin
  PBrushColor := ToolColor.ButtonColor;
end;

procedure TRoundProperty.ObjectsCreate;
var
  RoundXLabel: TLabel;
  RoundYLabel: TLabel;
begin
  RoundXLabel := TLabel.Create(ParentPanel);
  RoundXLabel.Caption := 'RoundX';
  RoundXLabel.Parent := ParentPanel;
  RoundXLabel.Top := PTop;
  PTop := PTop + RoundYLabel.Height;

  ToolRoundX := TSpinEdit.Create(ParentPanel);
  ToolRoundX.Top := PTop;
  ToolRoundX.Name := 'ToolRoundX';
  ToolRoundX.Width := ParentPanel.Width;
  ToolRoundX.MaxValue := 100;
  ToolRoundX.MinValue := 1;
  ToolRoundX.Value := PRoundX;
  ToolRoundX.Parent := ParentPanel;
  ToolRoundX.OnChange := @RoundChangeX;
  PTop := PTop + ToolRoundX.Height;

  RoundYLabel := TLabel.Create(ParentPanel);
  RoundYLabel.Caption := 'RoundY';
  RoundYLabel.Parent := ParentPanel;
  RoundYLabel.Top := PTop;
  PTop := PTop + RoundYLabel.Height;

  ToolRoundY := TSpinEdit.Create(ParentPanel);
  ToolRoundY.Top := PTop;
  ToolRoundY.Name := 'ToolRoundY';
  ToolRoundY.Width := ParentPanel.Width;
  ToolRoundY.MaxValue := 100;
  ToolRoundY.MinValue := 1;
  ToolRoundY.Value := PRoundY;
  ToolRoundY.Parent := ParentPanel;
  ToolRoundY.OnChange := @RoundChangeY;
  PTop := PTop + ToolRoundY.Height;
end;

procedure TRoundProperty.RoundChangeX(Sender: TObject);
begin
  PRoundX := ToolRoundX.Value;
end;

procedure TRoundProperty.RoundChangeY(Sender: TObject);
begin
  PRoundY := ToolRoundY.Value;
end;

end.
