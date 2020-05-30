{: Using materials in a TGLDirectOpenGL OnRender.

   This demo shows how to dynamically create materials in a material library
   and use them in a TGLDirectOpenGL to render your own stuff.<br>
   The render is quite simple: two quads, each with its own texture. The
   TGLDirectOpenGL is placed in a small hierarchy with a torus and dummy cube,
   and the rotation animation are handled by those two object to show that
   the OnRender code uses the hierarchy.

  <b>History : </b><font size=-1><ul>
      <li>21/04/10 - Yar - Removed direct state changing
    </ul></font>
}
program customquad;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
