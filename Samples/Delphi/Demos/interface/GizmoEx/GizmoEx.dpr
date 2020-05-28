{: GLGizmoEx component demo.

  Version History:
  <li> 10/11/2012 - PW - Changed FPS output to Panel1.Caption
  <li> 29/09/2007 - DaStr - Initial version.
  <li> 07/10/2009 - Predator - Updated version.
}
program GizmoEx;

uses
  Forms,
  DemoGizmoForm in 'DemoGizmoForm.pas' {Form1};

{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.

