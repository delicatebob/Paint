unit Utools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, Buttons, LCLType, LCLIntf,
  UFigures, Types, uProperty;

type

  TAttributeTool = (Line, Figure, RoundFigure);

  TTool = class
    Properties: array of TProperty;
    figureclass: TfigureClass;
    Name, Icon: string;
    procedure FigureCreate(Point: TPoint); virtual; abstract;
    procedure AddPoint(Point: TPoint); virtual; abstract;
    procedure MouseUp(Point: TPoint); virtual;
  end;

  TLinesTool = class(TTool)
    constructor Create; virtual;
    procedure FigureCreate(Point: TPoint); override;
  end;

  TFiguresTool = class(TLinesTool)
    constructor Create; override;
    procedure FigureCreate(Point: TPoint); override;
  end;

  TPencilTool = class(TLinesTool)
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TPolylineTool = class(TLinesTool)
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TRectangleTool = class(TFiguresTool)
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TEllipseTool = class(TFiguresTool)
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TRoundRectangleTool = class(TFiguresTool)
    constructor Create; override;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  THandTool = class(TTool)
    firstpoint: TPoint;
    firstoffset: TPoint;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
  end;

  TSelectTool = class(TRoundRectangleTool)
    AttributeTool:TAttributeTool;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure MouseUp(Point: TPoint); override;
    procedure RectSelectTool(Point: TPoint);
    procedure PointSelectTool(Point: TPoint);
  end;

var

  Tools: array of TTool;

implementation

uses Umain, UScale;

procedure TTool.MouseUp(Point: TPoint);
begin
  //nothing
end;

procedure TPencilTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create;
  IsDrawing := True;
  inherited;
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
  IsDrawing := True;
  inherited;
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
  IsDrawing := True;
  inherited;
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
  IsDrawing := True;
  inherited;
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
  IsDrawing := True;
  inherited;
  with (Figures[high(Figures)] as TRoundRectangle) do
  begin
    ARoundX:=PRoundX;
    ARoundY:=PRoundY;
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
  firstpoint := Point;
  firstoffset := offset;
end;

procedure THandTool.AddPoint(Point: TPoint);
begin
  offset := firstoffset + firstpoint - Point;
end;

procedure TSelectTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TFrame.Create;
  IsDrawing := True;
  with (Figures[High(Figures)] as TFrame) do
  begin
    SetLength(DPoints, 2);
    DPoints[0] := Scrn2Wrld(Point);
    DPoints[1] := Scrn2Wrld(Point);
  end;
end;

procedure TSelectTool.AddPoint(Point: TPoint);
begin
  Figures[High(Figures)].DPoints[1] := Scrn2Wrld(Point);
end;

procedure TSelectTool.MouseUp(Point: TPoint);
var
  ToolRegion: HRGN;
  i: integer;
begin
  with Figures[High(Figures)] do
  begin
    Region := CreateRectRgn(WorldToScreen(DPoints[0]).X,
      WorldToScreen(DPoints[0]).Y, WorldToScreen(DPoints[1]).X,
      WorldToScreen(DPoints[1]).Y);
  end;
  for i := 0 to High(Figures) - 1 do
  begin
    if (CombineRgn(ToolRegion, Figures[i].Region, Figures[High(Figures)].Region,
      RGN_AND) <> NullRegion) then
      Figures[i].isSelected := False;
  end;
  with Figures[High(Figures)] do
  begin
    if (not ((DPoints[0].X = DPoints[1].X) and (DPoints[0].Y = DPoints[1].Y))) then
      RectSelectTool(Point)
    else
      PointSelectTool(Point);
  end;
  SetLength(Figures, Length(Figures) - 1);
end;

procedure TSelectTool.RectSelectTool(Point: TPoint);
var
  i: integer;
  ToolRegion: HRGN;
begin
  for i := 0 to High(Figures) - 1 do
  begin
    DeleteObject(Figures[i].Region);
    Figures[i].CreateRegion;
    ToolRegion := CreateRectRgn(1, 1, 2, 2);
    if (CombineRgn(ToolRegion, Figures[i].Region, Figures[High(Figures)].Region,
      RGN_AND) <> NULLREGION) then
      if (Figures[i].isSelected = False) then
        Figures[i].isSelected := True
      else
        Figures[i].isSelected := False;
    DeleteObject(ToolRegion);
  end;
end;

procedure TSelectTool.PointSelectTool(Point: TPoint);
var
  I: integer;
begin
  for i := High(Figures) - 1 downto Low(Figures) do
    with Figures[i] do
    begin
      DeleteObject(Region);
      CreateRegion;
      if (PtInRegion(Region, Point.X, Point.Y) = True) then
        if (isSelected = False) then
          isSelected := True
        else
          isSelected := False;
    end;
end;

constructor TLinesTool.Create;
begin
  SetLength(Properties, 3);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenColorProperty.Create;
  Properties[2] := TPenStyleProperty.Create;
end;

constructor TFiguresTool.Create;
begin
  inherited;
  SetLength(Properties, 5);
  Properties[3] := TBrushColorProperty.Create;
  Properties[4] := TBrushStyleProperty.Create;
end;

constructor TRoundRectangleTool.Create;
begin
  inherited;
  SetLength(Properties, 6);
  Properties[5] := TRoundProperty.Create;
end;

procedure TLinesTool.FigureCreate(Point: TPoint);
begin
  with (Figures[High(Figures)] as TLines) do
  begin
  SetLength(DPoints, 2);
  DPoints[0] := Scrn2Wrld(Point);
  DPoints[1] := Scrn2Wrld(Point);
  AWidth := PWidth;
  APenColor := PPenColor;
  APenStyle := PPenStyle;
  end;
end;

procedure TFiguresTool.FigureCreate(Point: TPoint);
begin
  inherited;
  with (Figures[High(Figures)] as TFigures) do
  begin
  ABrushStyle := PBrushStyle;
  ABrushColor := PBrushColor;
  end;
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
  RegisterTool(TPencilTool.Create, Tpencil, 'Pencil', 'Pencil.png');
  RegisterTool(TPolylineTool.Create, TPolyline, 'Polyline', 'Polyline.png');
  RegisterTool(TRectangleTool.Create, TRectangle, 'Rectangle', 'Rectangle.png');
  RegisterTool(TEllipseTool.Create, TEllipse, 'Ellipse', 'Ellipse.png');
  RegisterTool(TRoundRectangleTool.Create, TRoundRectangle, 'RoundRectangle',
    'RoundRectangle.png');
  RegisterTool(THandTool.Create, nil, 'Hand', 'Hand.png');
  RegisterTool(TSelectTool.Create, Nil, 'Select', 'Select.png');

end.
