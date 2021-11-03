unit fBasicSDL_D;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Dialogs,

  GLS.Scene,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Color,
  GLS.Context,
  GLS.Texture,
  GLS.Utils,

  GLS.SDLContext,
  SDL2.Import;

type
  TDataModule1 = class(TDataModule)
    GLScene1: TGLScene;
    GLSDLViewer1: TSDLViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Teapot1: TGLTeapot;
    procedure DataModuleCreate(Sender: TObject);
    procedure GLSDLViewer1EventPollDone(Sender: TObject);
    procedure GLSDLViewer1Resize(Sender: TObject);
  public
    firstPassDone: Boolean;
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.dfm}

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  // When using SDL2, the standard VCL message queue is no longer operational,
  // so you must have/make your own loop to prevent the application from
  // terminating immediately
  GLSDLViewer1.Render;
  while GLSDLViewer1.Active do
  begin
    // Message queue is not operational, but there may still be some messages
    Application.ProcessMessages;
    // Relinquish some of that CPU time
    SDL_Delay(1);
    // Slowly rotate the teapot
    Teapot1.RollAngle := 4 * Frac(Now * 24) * 3600;
  end;
end;

procedure TDataModule1.GLSDLViewer1EventPollDone(Sender: TObject);
begin
  SetGLSceneMediaDir();
  if not firstPassDone then
  begin
    // Loads a texture map for the teapot
    // (see materials/cubemap for details on that)
    // The odd bit is that it must be done upon first render, otherwise
    // SDL OpenGL support has not been initialized and things like checking
    // an extension support (cube maps here) would fail...
    // Something less clunky will be introduced, someday...
    firstPassDone := True;
    GLSDLViewer1.Buffer.RenderingContext.Activate;
    try
      if not GL.ARB_texture_cube_map then
        ShowMessage('Your graphics board does not support cube maps...'#13#10 +
          'So, no cube maps for ya...')
      else
      begin
        with Teapot1.Material.Texture do
        begin
          ImageClassName := TGLCubeMapImage.ClassName;
          with Image as TGLCubeMapImage do
          begin
            Picture[cmtPX].LoadFromFile('cm_left.jpg');
            Picture[cmtNX].LoadFromFile('cm_right.jpg');
            Picture[cmtPY].LoadFromFile('cm_top.jpg');
            Picture[cmtNY].LoadFromFile('cm_bottom.jpg');
            Picture[cmtPZ].LoadFromFile('cm_back.jpg');
            Picture[cmtNZ].LoadFromFile('cm_front.jpg');
          end;
          MappingMode := tmmCubeMapReflection;
          Disabled := False;
        end;
      end;
    finally
      GLSDLViewer1.Buffer.RenderingContext.Deactivate;
    end;
  end;

  GLSDLViewer1.Render;
end;

procedure TDataModule1.GLSDLViewer1Resize(Sender: TObject);
begin
  // Zoom if SDL window gets smaller/bigger
  GLCamera1.SceneScale := GLSDLViewer1.Width / 160;
end;

end.
