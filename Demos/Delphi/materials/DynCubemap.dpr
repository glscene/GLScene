{: Dynamic cube map generation demo.

   This a more advanced sample in which the cube map is dynamically generated.
   To generate a cube map, you need three items:<ul>
   <li>a destination texture (where to place the cube map!), the requirement is
      that the TextureImage class must be... a CubeMapImage
   <li>a camera, to specify from where the cubemap will be rendered
      (the orientation and parameters of the camera do not matter, only its
      position is relevant)
   <li>a memory viewer, this is what we'll use to render the 6 images of the
      cube map, it also determines the size of the texture (and must be a square)
   </ul>
   Generating the cube map can then be performed with a single call to the
   memoryviewer's RenderCubeMapTextures method.

   Note: cube map can be used for "cool" looking reflections, but as you'll
   see in this demo it is not perfect, especially if the reflected objects
   are close to the reflecting object (cube map is generated without "knowledge"
   of the object that'll be used for reflecting), so use when appropriate ;)
}
program DynCubemap;

uses
  Forms,
  DynCubemapFm in 'DynCubemapFm.pas' {FormDynCubeMap};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDynCubeMap, FormDynCubeMap);
  Application.Run;
end.
