{: GLConsole demo
  See the interface part of GLConsole.pas for details...
  History:
   <li> 10/11/12 - PW - Added FPS output with GLSimpleNavigation instead of TTimer
   <li> 07/02/07 - DaStr - Initial version
  </ul>
}
program ConsoleD;

uses
  Forms,
  fConsoleD in 'fConsoleD.pas' {FormConsole};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormConsole, FormConsole);
  Application.Run;
end.
