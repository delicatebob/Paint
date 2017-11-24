unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Controls;

type
  TDoublePoint = record
    X: double;
    Y: double;
  end;

function WorldToScreen(P: TDoublePoint): TPoint;
function Scrn2Wrld(P: TPoint): TDoublePoint;

var
  Zoom: integer;
  Offset: TPoint;

implementation

function WorldToScreen(P: TDoublePoint): TPoint;
begin
  Result.X := round(P.X * Zoom / 100) - Offset.x;
  Result.y := round(P.Y * Zoom / 100) - Offset.y;
end;


function Scrn2Wrld(P: TPoint): TDoublePoint;
begin
  Result.X := (P.x + Offset.x) / Zoom * 100;
  Result.Y := (P.y + Offset.y) / Zoom * 100;
end;

initialization
  Zoom := 100;
  Offset.x := 0;
  Offset.y := 0;
end.

