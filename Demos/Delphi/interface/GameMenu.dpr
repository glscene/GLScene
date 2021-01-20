{
  A simple demo that shows how to use the TGLGameMenu component

  Version history:
    20/02/07 - DaStr - Initial version
    03/07/07 - DaStr - Keyboard renamed to GLKeyboard (BugTracker ID = 1678646)
}
program GameMenu;

uses
  Forms,
  GameMenuFm in 'GameMenuFm.pas' {FormGameMenu};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGameMenu, FormGameMenu);
  Application.Run;
end.
