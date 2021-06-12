{: Using materials in a TGLDirectOpenGL OnRender.

   This demo shows how to dynamically create materials in a material library
   and use them in a TGLDirectOpenGL to render your own stuff.
   The render is quite simple: two quads, each with its own texture. The
   TGLDirectOpenGL is placed in a small hierarchy with a torus and dummy cube,
   and the rotation animation are handled by those two object to show that
   the OnRender code uses the hierarchy.

  <b>History : </b><font size=-1><ul>
      <li>21/04/10 - Yar - Removed direct state changing
    </ul></font>
}
program CustomQuad;

uses
  Forms,
  CustomQuadFm in 'CustomQuadFm.pas' {FormCustomQuad};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCustomQuad, FormCustomQuad);
  Application.Run;
end.
