unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, LCLType, LCLIntf, Math, Uscale;

type

  TFigure = class
    isSelected: boolean;
    Region: HRGN;
    DPoints: array of TDoublePoint;
    procedure CreateRegion; virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure DrawOutline(Point1, Point2: TDoublePoint; ACanvas: TCanvas); virtual;
  end;

  TfigureClass = class of Tfigure;

  Tlines = class(Tfigure)
    APenColor: TColor;
    AWidth: byte;
    APenStyle: TPenStyle;
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TFigures = class(Tlines)
    ABrushStyle: TBrushStyle;
    ABrushColor: TColor;
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TPencil = class(Tlines)
    procedure CreateRegion; override;
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TPolyline = class(TLines)
    procedure CreateRegion; override;
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TRectangle = class(TFigures)
    procedure CreateRegion; override;
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TEllipse = class(TFigures)
    procedure CreateRegion; override;
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TRoundRectangle = class(TFigures)
    ARoundX: byte;
    ARoundY: byte;
    procedure CreateRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TFrame = class(TFigures)
    procedure Draw(ACanvas: Tcanvas); override;
  end;

var
  Figures: array of Tfigure;

implementation

procedure Tpencil.Draw(ACanvas: TCanvas);
var
  i: integer;
  Max, Min:TDoublePoint;
begin
  inherited;
  for i := 1 to High(DPoints) do
    ACanvas.Line(WorldToScreen(DPoints[i - 1]), WorldToScreen(DPoints[i]));
  if(isSelected) then
  begin
    DeleteObject(Region);
    Min:=DoublePoint(MaxFloat,MaxFloat);
    Max := DoublePoint(MinFloat,MinFloat);
    for i := 1 to high(DPoints) do
    begin
      if DPoints[i].X > Max.X then Max.X:=DPoints[i].X;
      if DPoints[i].Y > Max.Y then Max.Y:=DPoints[i].Y;
      if DPoints[i].X < Min.X then Min.X:=DPoints[i].X;
      if DPoints[i].Y < Min.Y then Min.Y:=DPoints[i].Y;
    end;
    DrawOutline(Min,Max,ACanvas);
    end;
end;

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  inherited;
    ACanvas.Line(WorldToScreen(DPoints[0]), WorldToScreen(DPoints[1]));
  if(isSelected) then
  begin
    DeleteObject(Region);
    DrawOutline(DPoints[0], DPoints[1], ACanvas);
end;
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
    ACanvas.Rectangle(WorldToScreen(DPoints[0]).x, WorldToScreen(DPoints[0]).y,
      WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y);
  if(isSelected) then
  begin
    DeleteObject(Region);
    DrawOutline(DPoints[0], DPoints[1], ACanvas);
end;
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
    ACanvas.Ellipse(WorldToScreen(DPoints[0]).x, WorldToScreen(DPoints[0]).y,
      WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y);
  if(isSelected) then
  begin
    DeleteObject(Region);
    DrawOutline(DPoints[0], DPoints[1], ACanvas);
end;
end;

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
    ACanvas.RoundRect(WorldToScreen(DPoints[0]).x, WorldToScreen(DPoints[0]).y,
      WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y, ARoundX, ARoundY);
  if(isSelected) then
  begin
    DeleteObject(Region);
    DrawOutline(DPoints[0], DPoints[1], ACanvas);
end;
end;

procedure TFrame.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := ClBlack;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Frame(WorldToScreen(DPoints[0]).x, WorldToScreen(DPoints[0]).y,
    WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y);
end;

procedure LineRegion(p1, p2: TPoint; var tempPoints: array of TPoint;
  Width: integer);
begin
  if (abs(p2.x - p1.x) > 45) then
  begin
    tempPoints[0].x := p1.x - Width div 2;
    tempPoints[0].y := p1.y - 5 - Width;
    tempPoints[1].x := p2.x + Width div 2;
    tempPoints[1].y := p2.y - 5 - Width;
    tempPoints[2].x := p2.x + Width div 2;
    tempPoints[2].y := p2.y + 5 + Width;
    tempPoints[3].x := p1.x - Width div 2;
    tempPoints[3].y := p1.y + 5 + Width;
  end
  else
  begin
    tempPoints[0].x := p1.x - 5 - Width;
    tempPoints[0].y := p1.y - Width div 2;
    tempPoints[1].x := p2.x - 5 - Width;
    tempPoints[1].y := p2.y + Width div 2;
    tempPoints[2].x := p2.x + 5 + Width;
    tempPoints[2].y := p2.y + Width div 2;
    tempPoints[3].x := p1.x + 5 + Width;
    tempPoints[3].y := p1.y - Width div 2;
  end;
end;

procedure TPencil.CreateRegion;
var
  RegionPoints: array[0..3] of TPoint;
  P1, P2: TPoint;
  i:integer;
  CurRgn: HRGN;
begin
  for i := 0 to High(DPoints) - 1 do
  begin
    P1 := WorldToScreen(DPoints[i]);
    P2 := WorldToScreen(DPoints[i + 1]);
    LineRegion(P1, P2, RegionPoints, AWidth);
    if (i = Low(DPoints)) then
      Region := CreatePolygonRgn(RegionPoints, 3, 2);
    CurRgn := CreatePolygonRgn(RegionPoints, 3, 2);
    CombineRgn(Region, Region, CurRgn, RGN_OR);
    DeleteObject(CurRgn);
  end;
end;

procedure TPolyLine.CreateRegion;
var
  RegionPoints: array[0..3] of TPoint;
  P1, P2: TPoint;
begin
  P1 := WorldToScreen(DPoints[0]);
  P2 := WorldToScreen(DPoints[1]);
  LineRegion(P1, P2, RegionPoints, AWidth);
  Region := CreatePolygonRgn(RegionPoints, 3, 2);
end;

procedure TRectangle.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(DPoints[0]);
  RegionRect.BottomRight := WorldToScreen(DPoints[1]);
  Region := CreateRectRgn(RegionRect.Left, RegionRect.Top, RegionRect.Right,
    RegionRect.Bottom);
end;

procedure TEllipse.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(DPoints[0]);
  RegionRect.BottomRight := WorldToScreen(DPoints[1]);
  Region := CreateEllipticRgn(RegionRect.Left, RegionRect.Top,
    RegionRect.Right, RegionRect.Bottom);
end;

procedure TRoundRectangle.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(DPoints[0]);
  RegionRect.BottomRight := WorldToScreen(DPoints[1]);
  Region := CreateRoundRectRgn(RegionRect.Left, RegionRect.Top,
    RegionRect.Right, RegionRect.Bottom, ARoundX, ARoundY);
end;

procedure TLines.Draw(ACanvas: Tcanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := APenStyle;
end;

procedure TFigures.Draw(ACanvas: Tcanvas);
begin
  inherited;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Brush.Style := ABrushStyle;
end;

procedure TFigure.DrawOutline(Point1, Point2: TDoublePoint; ACanvas: TCanvas);
var
  Temp: TDoublePoint;
begin
  if(Point1.X > Point2.X) then
  begin
    Temp.X := Point1.X;
    Point1.X := Point2.X;
    Point2.X := Temp.X;
  end;
  if(Point1.Y > Point2.Y) then
  begin
    Temp.Y := Point1.Y;
    Point1.Y := Point2.Y;
    Point2.Y := Temp.Y;
  end;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psDash;
  ACanvas.Frame(WorldToScreen(Point1).X - 5, WorldToScreen(Point1).Y - 5,
               WorldToScreen(Point2).X + 5, WorldToScreen(Point2).Y + 5);
end;

end.
