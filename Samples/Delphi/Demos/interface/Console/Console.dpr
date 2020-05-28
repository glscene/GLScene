{: GLConsole demo
  See the interface part of GLConsole.pas for details...
  History:
   <li> 10/11/12 - PW - Added FPS output with GLSimpleNavigation instead of TTimer
   <li> 07/02/07 - DaStr - Initial version
  </ul>
}
program Console;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
