unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Uscale;

type

  TFigure = class
    DPoints: array of TDoublePoint;
    Points: array of Tpoint;
    APenColor: TColor;
    ABrushColor: TColor;
    AWidth: Byte;
    ARoundX: Byte;
    ARoundY: Byte;
    APenStyle:TPenStyle;
    ABrushStyle:TBrushStyle;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
  end;

  TfigureClass = class of Tfigure;

  Tlines = class(Tfigure)
  end;

  //TFigures = class(Tfigure)
  // ABrushColor: TColor;
  //end;

  TPencil = class(Tlines)
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TPolyline = class(TLines)
    procedure Draw(ACanvas: Tcanvas); override;
  end;

  TRectangle = class(TFigure)
      procedure Draw(ACanvas: Tcanvas); override;
  end;

  TEllipse = class(TFigure)
      procedure Draw(ACanvas: Tcanvas); override;
  end;

  TRoundRectangle = class(TFigure)
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
  ACanvas.Pen.Style:=APenStyle;
  for i := 1 to High(DPoints) do
    ACanvas.Line(WorldToScreen(DPoints[i - 1]), WorldToScreen(DPoints[i]));
end;

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style:=APenStyle;
  if High(DPoints) = 1 then // Если создана вторая точка, иначе он будет брать точку начала координат.
    ACanvas.Line(WorldToScreen(DPoints[0]), WorldToScreen(DPoints[1]));
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style:=APenStyle;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Brush.Style:=ABrushStyle;
  if High(DPoints) = 1 then // Если создана вторая точка, иначе он будет брать точку начала координат.
  ACanvas.Rectangle(WorldToScreen(DPoints[0]).x,WorldToScreen(DPoints[0]).y, WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y);
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style:=APenStyle;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Brush.Style:=ABrushStyle;
  if High(DPoints) = 1 then // Если создана вторая точка, иначе он будет брать точку начала координат.
  ACanvas.Ellipse(WorldToScreen(DPoints[0]).x,WorldToScreen(DPoints[0]).y, WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y);
end;

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style:=APenStyle;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Brush.Style:=ABrushStyle;
  if High(DPoints) = 1 then // Если создана вторая точка, иначе он будет брать точку начала координат.
  ACanvas.RoundRect(WorldToScreen(DPoints[0]).x,WorldToScreen(DPoints[0]).y, WorldToScreen(DPoints[1]).x, WorldToScreen(DPoints[1]).y, ARoundX, ARoundY);
end;

end.
