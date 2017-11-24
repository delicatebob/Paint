unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Uscale;

type

  TFigure = class
    DPoints: array of TDoublePoint;
    Points: array of Tpoint;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
  end;

  TfigureClass = class of Tfigure;

  Tlines = class(Tfigure)
    AWidth: byte;
    APenColor: TColor;
    APenStyle: TPenStyle;
  end;

  TFigures = class(Tlines)
    ABrushColor: TColor;
    ABrushStyle: TBrushStyle;
  end;

  TPencil = class(Tlines)
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TPolyline = class(TLines)
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TRectangle = class(TFigures)
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TEllipse = class(TFigures)
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TRoundRectangle = class(TFigures)
    ARoundX: byte;
    ARoundY: byte;
    procedure Draw(ACanvas: TCanvas); override;
  end;

var
  Figures: array of Tfigure;

implementation

procedure Tpencil.Draw(ACanvas: TCanvas);
var
  i: integer;
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := APenStyle;
  for i := 1 to High(DPoints) do
    ACanvas.Line(WorldToScreen(DPoints[i - 1]), WorldToScreen(DPoints[i]));
end;

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := APenStyle;
  if High(DPoints) = 1 then
    // Если создана вторая точка, иначе он будет брать точку начала координат.
    ACanvas.Line(WorldToScreen(DPoints[0]), WorldToScreen(DPoints[1]));
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := APenStyle;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Brush.Style := ABrushStyle;
  if High(DPoints) = 1 then
    // Если создана вторая точка, иначе он будет брать точку начала координат.
    ACanvas.Rectangle(WorldToScreen(DPoints[0]).x, WorldToScreen(DPoints[0]).y,
      WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y);
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := APenStyle;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Brush.Style := ABrushStyle;
  if High(DPoints) = 1 then
    // Если создана вторая точка, иначе он будет брать точку начала координат.
    ACanvas.Ellipse(
      WorldToScreen(DPoints[0]).x, WorldToScreen(DPoints[0]).y,
      WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y);
end;

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := APenStyle;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Brush.Style := ABrushStyle;
  if High(DPoints) = 1 then
    // Если создана вторая точка, иначе он будет брать точку начала координат.
    ACanvas.RoundRect(WorldToScreen(DPoints[0]).x, WorldToScreen(DPoints[0]).y,
      WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y, ARoundX, ARoundY);
end;

end.
