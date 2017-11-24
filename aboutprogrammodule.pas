unit aboutprogramModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutProgramForm }

  TAboutProgramForm = class(TForm)
    Information: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutProgramForm: TAboutProgramForm;

implementation

{$R *.lfm}

end.

