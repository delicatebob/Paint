unit Utools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, Buttons, Spin, StdCtrls,
  UFigures, Types, uProperty;

type


  TTool = class
    Properties: array of TProperty;
    figureclass: TfigureClass;
    Name, Icon: string;
    procedure FigureCreate(Point: TPoint); virtual; abstract;
    procedure AddPoint(Point: TPoint); virtual; abstract;
  end;

  TLineTool = class(TTool)
  end;

  TFigureTool = class(TLineTool)
  end;

  TPencilTool = class(TLineTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TPolylineTool = class(TLineTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TRectangleTool = class(TFigureTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TEllipseTool = class(TFigureTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TRoundRectangleTool = class(TFigureTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  THandTool = class(TFigureTool)
    FIRSTPOINT: TPoint;
    FIRSTOFFSET: TPoint;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

var

  Tools: array of TTool;

implementation

uses Umain, UScale;

procedure TPencilTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create;
  Figures[High(Figures)].Awidth := PWidth;
  Figures[High(Figures)].APenColor := PPenColor;
  Figures[High(Figures)].APenStyle := PPenStyle;
  IsDrawing := True;
  Tools[0].AddPoint(Point);
end;

procedure TPencilTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
  begin
    SetLength(DPoints, length(DPoints) + 1);
    DPoints[high(DPoints)] := Scrn2Wrld(Point);
  end;
end;

procedure TPolylineTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPolyline.Create;
  Figures[High(Figures)].Awidth := PWidth;
  Figures[High(Figures)].APenColor := PPenColor;
  Figures[High(Figures)].APenStyle := PPenStyle;
  IsDrawing := True;
  with (Figures[High(Figures)] as TPolyline) do
  begin
    SetLength(DPoints, 1);
    DPoints[0] := Scrn2Wrld(Point);
  end;
end;

procedure TPolylineTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
  begin
    SetLength(DPoints, 2);
    DPoints[1] := Scrn2Wrld(Point);
  end;
end;

procedure TRectangleTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create;
  Figures[High(Figures)].Awidth := PWidth;
  Figures[High(Figures)].APenColor := PPenColor;
  Figures[High(Figures)].ABrushColor := PBrushColor;
  Figures[High(Figures)].APenStyle := PPenStyle;
  Figures[High(Figures)].ABrushStyle := PBrushStyle;
  IsDrawing := True;
  with (Figures[High(Figures)] as TRectangle) do
  begin
    SetLength(DPoints, 1);
    DPoints[0] := Scrn2Wrld(Point);
  end;
end;

procedure TRectangleTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
  begin
    SetLength(DPoints, 2);
    DPoints[1] := Scrn2Wrld(Point);
  end;
end;

procedure TEllipseTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create;
  Figures[High(Figures)].Awidth := PWidth;
  Figures[High(Figures)].APenColor := PPenColor;
  Figures[High(Figures)].ABrushColor := PBrushColor;
  Figures[High(Figures)].APenStyle := PPenStyle;
  Figures[High(Figures)].ABrushStyle := PBrushStyle;
  IsDrawing := True;
  with (Figures[High(Figures)] as TEllipse) do
  begin
    SetLength(DPoints, 1);
    DPoints[0] := Scrn2Wrld(Point);
  end;
end;

procedure TEllipseTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
  begin
    SetLength(DPoints, 2);
    DPoints[1] := Scrn2Wrld(Point);
  end;
end;

procedure TRoundRectangleTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create;
  Figures[High(Figures)].Awidth := PWidth;
  Figures[High(Figures)].APenColor := PPenColor;
  Figures[High(Figures)].ABrushColor := PBrushColor;
  Figures[High(Figures)].APenStyle := PPenStyle;
  Figures[High(Figures)].ABrushStyle := PBrushStyle;
  Figures[High(Figures)].ARoundX := PRoundX;
  Figures[High(Figures)].ARoundY := PRoundY;
  IsDrawing := True;
  with (Figures[High(Figures)] as TRoundRectangle) do
  begin
    SetLength(DPoints, 1);
    DPoints[0] := Scrn2Wrld(Point);
  end;
end;

procedure TRoundRectangleTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
  begin
    SetLength(DPoints, 2);
    DPoints[1] := Scrn2Wrld(Point);
  end;
end;

procedure THandTool.FigureCreate(Point: TPoint);
begin
  FIRSTPOINT := Point;
  FIRSTOFFSET := offset;
end;

procedure THandTool.AddPoint(Point: TPoint);
begin
  offset := FIRSTOFFSET + FIRSTPOINT - Point;
end;

constructor TPencilTool.Create;
begin
  SetLength(Properties, 3);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenColorProperty.Create;
  Properties[2] := TPenStyleProperty.Create;
end;

constructor TPolylineTool.Create;
begin
  SetLength(Properties, 3);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenColorProperty.Create;
  Properties[2] := TPenStyleProperty.Create;
end;

constructor TRectangleTool.Create;
begin
  SetLength(Properties, 5);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenColorProperty.Create;
  Properties[2] := TPenStyleProperty.Create;
  Properties[3] := TBrushColorProperty.Create;
  Properties[4] := TBrushStyleProperty.Create;
end;

constructor TEllipseTool.Create;
begin
  SetLength(Properties, 5);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenColorProperty.Create;
  Properties[2] := TPenStyleProperty.Create;
  Properties[3] := TBrushColorProperty.Create;
  Properties[4] := TBrushStyleProperty.Create;
end;

constructor TRoundRectangleTool.Create;
begin
  SetLength(Properties, 6);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenColorProperty.Create;
  Properties[2] := TPenStyleProperty.Create;
  Properties[3] := TBrushColorProperty.Create;
  Properties[4] := TBrushStyleProperty.Create;
  Properties[5] := TRoundProperty.Create;
end;

procedure RegisterTool(ATool: TTool; Aclass: TfigureClass; AName: string; AIcon: string);
begin
  Setlength(Tools, length(Tools) + 1);
  Tools[High(Tools)] := ATool;
  Tools[High(Tools)].FigureClass := Aclass;
  Tools[High(Tools)].Name := AName;
  Tools[high(Tools)].Icon := AIcon;
end;

initialization
  RegisterTool(TPencilTool.Create, Tpencil, 'Pencil', 'Pencil.jpg');
  RegisterTool(TPolylineTool.Create, TPolyline, 'Polyline', 'Polyline.jpg');
  RegisterTool(TRectangleTool.Create, TRectangle, 'Rectangle', 'Rectangle.jpg');
  RegisterTool(TEllipseTool.Create, TEllipse, 'Ellipse', 'Ellipse.jpg');
  RegisterTool(TRoundRectangleTool.Create, TRoundRectangle, 'RoundRectangle',
    'RoundRectangle.jpg');
  RegisterTool(THandTool.Create, nil, 'Hand', 'Hand.jpg');

end.
