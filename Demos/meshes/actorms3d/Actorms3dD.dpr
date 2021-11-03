{: This demo shows how easy it is to use milkshape animations in GLScene. The
    animations are courtesy of Carnegie-Mellon's motion capture project. 

    Animations also make use of MS3D's weighted vertexes. This was really needed as
    it makes animations much more realistic, as you will see.

    The demo also shows the use of double sided textures (her hair), specular lighting,
    and transparency. To make a texture doublesided, just give it a tiny bit of transparency
    in Milkshape. This will cause the loader to turn off backface culling for any group that
    uses that material. 

    I have also utilized one of Yar's shader demos modified a little so the spotlight
    will always follow the actor during the animation sequence.

    Model was made with MS3D, UVMapping and texturing were done with Paintshop and UVMapper Pro

    Note on the animations: There is a flaw in the MakeSkeletalTranslationStatic routine if you
    want to stop all root node translations. I'll be adding an overloaded method in the real soon.
    For now,  if you do not want root node transformations (i.e. you want her to stay in one spot)
    uncomment the line: //pos:=ms3d_joints^[i].Base.Position.V; where the animations are loaded
    in GLFileMS3D.

    TL
}

program Actorms3dD;

uses
  Forms,
  fActorms3dD in 'fActorms3dD.pas' {FormActorms3d};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormActorms3d, FormActorms3d);
  Application.Run;
end.
