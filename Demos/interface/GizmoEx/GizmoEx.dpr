{: GLGizmoEx component demo.

  Version History:
  <li> 10/11/2012 - PW - Changed FPS output to Panel1.Caption
  <li> 29/09/2007 - DaStr - Initial version.
  <li> 07/10/2009 - Predator - Updated version.
}
program GizmoEx;

uses
  Forms,
  GizmoexFm in 'GizmoexFm.pas' {FormGizmoEx};

{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFormGizmoEx, FormGizmoEx);
  Application.Run;
End.

