{:
 Example using GLSkybox. Use the W,A,S,D keys (or Z,Q,S,D) to move
 and the mouse to look around.
 The scene contains two GLSkyBox objects: GLSkyBox1 and GLSkyBox2 :
 GLSkyBox1 renders the 6 faces of the skybox cube showing
 the landscape, plus one cloudy-textured plane over our head;
 GLSkyBox2 renders only an extra cloudy-textured plane over our head, to
 make a parallattic effect with the other cloudy plane.
 The parallattic effect is achieved setting the "CloudsPlaneSize" and ""CloudsPlaneOffset"
 differently in GLSkyBox1 and GLSkyBox2, and by shifting
 the clouds texture gradually in the cadencer event.
 CloudsPlaneSize is the size of the clouds plane, while
 CloudsPlaneOffset is the distance from the center of the skybox cube (range : 0 to 0.5)
 Note that the moons are children of a GLSkyBox object, and therefore
 they are always rendered relatively to the camera
}
program SkyBox;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
