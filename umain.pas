unit Umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, Buttons, Spin, StdCtrls, aboutprogramModule, Utools,
  UFigures, Uscale, Types, uProperty;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutProgramMenu: TMenuItem;
    PaintBox: TPaintBox;
    ToolsAndPropertyPanel: TPanel;
    procedure PropertyPanelCreate;
    procedure DelPropertyPanel;
    procedure AboutProgramMenuClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ToolSpeedbuttonclick(Asender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
  private
    PropertyPanel: TPanel;
    { private declarations }
  public
    { public declarations }
  end;

var
  TopButtonAndPanel: integer = 0;
  MainForm: TMainForm;
  IsDrawing: boolean;
  PenColor: TColor = clBlack;
  BrushColor: TColor = clwhite;
  Tool: TTool;
  OnPressButton: TSpeedButton;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  IsDrawing := True;
  if ssleft in shift then
  begin
    Tool.FigureCreate(Point(X, Y));
  end;
  Paintbox.invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if IsDrawing then
  begin
    Tool.AddPoint(Point(X, Y));
  end;
  Paintbox.invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  IsDrawing := False;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  i: integer;
begin
  // Белый фон Begin
  PaintBox.Canvas.Pen.Color := clWhite;
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.Rectangle(0, 0, PaintBox.Width, PaintBox.Height);
  // end
  for i := 0 to high(Figures) do
    Figures[i].Draw(PaintBox.Canvas);
end;

procedure TMainForm.DelPropertyPanel;
begin
  // Удаление старой панели  begin
  if PropertyPanel <> nil then
    FreeAndNil(PropertyPanel);
  // end

  PTop := 0;
end;

procedure TMainForm.PropertyPanelCreate;
var
  i: integer;
begin
  PropertyPanel := TPanel.Create(ToolsAndPropertyPanel);
  PropertyPanel.Parent := ToolsAndPropertyPanel;
  PropertyPanel.Name := 'PropertyPanel';
  PropertyPanel.Caption := '';
  PropertyPanel.Width := ToolsAndPropertyPanel.Width;
  PropertyPanel.Height := 640;
  PropertyPanel.Top := TopButtonAndPanel + ToolsAndPropertyPanel.Width div 2;
  for i := 0 to High(Tool.Properties) do
  begin
    Tool.Properties[i].ParentPanel := PropertyPanel;
    Tool.Properties[i].ObjectsCreate;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ButtonIcon: TPicture;
  AButton: TSpeedbutton;
  i: integer;
begin
  for i := 0 to High(Tools) do
  begin
    Abutton := TSpeedbutton.Create(ToolsAndPropertyPanel);
    if i = 0 then
    begin
      Abutton.Enabled := False;
      OnPressButton := Abutton;
    end;
    Abutton.Name := Tools[i].Name;
    ButtonIcon := TPicture.Create;
    ButtonIcon.LoadFromFile('pic\' + Tools[i].icon);
    AButton.Glyph := ButtonIcon.Bitmap;
    ButtonIcon.Free;
    Abutton.Width := ToolsAndPropertyPanel.Width div 2;
    Abutton.Height := ToolsAndPropertyPanel.Width div 2;
    if (i mod 2 = 1) then
      AButton.left := ToolsAndPropertyPanel.Width div 2;
    if (i mod 2 = 0) and (i <> 0) then
      TopButtonAndPanel := TopButtonAndPanel + ToolsAndPropertyPanel.Width div 2;
    Abutton.Top := TopButtonAndPanel;
    Abutton.Tag := i;
    Abutton.Parent := ToolsAndPropertyPanel;
    Abutton.onClick := @ToolSpeedbuttonclick;
  end;
  PropertyPanelCreate;
end;

procedure Tmainform.ToolSpeedbuttonclick(Asender: TObject);
begin
  OnPressButton.Enabled := True;
  OnPressButton := Asender as Tspeedbutton;
  OnPressButton.Enabled := False;
  tool := Tools[(Asender as Tspeedbutton).tag];
  DelPropertyPanel;
  PropertyPanelCreate;
end;

//Main Menu Begin
procedure TMainForm.ExitMenuClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.HelpMenuClick(Sender: TObject);
begin
  if aboutprogramForm.Showing then
    aboutprogramMenu.Enabled := False
  else
    aboutprogramMenu.Enabled := True;
end;

procedure TMainForm.AboutProgramMenuClick(Sender: TObject);
begin
  aboutprogramForm.Show;
end;
//Main Menu End


initialization
  Tool := Tools[0];

end.
