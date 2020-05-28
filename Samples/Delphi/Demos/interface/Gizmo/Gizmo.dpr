{: GLGizmo component demo.
  Version History:
  29/09/2007 - DaStr - Improved by Degiovani, Delauney, Mrqzzz and DaStr
  21/07/2006 - arvidas - Initial version by Arvydas Juskevicius
                         aka adirex arvydas@adirex.com
-------------------------------------------------------

This demo shows the implementation of Gizmos like in 3D Studio Max.
Gizmos are very useful when moving or rotating objects in 3D.
Click an object to select it.

Navigation:

Change mode between moving and rotating
Select one of the arrows or corners with mouse and drag it.
Use mouse scroll to scroll the view.
Right mouse button click and move to rotate the camera.
}
program Gizmo;

uses
  Forms,
  DemoGizmoForm in 'DemoGizmoForm.pas' {Form1};

{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.

