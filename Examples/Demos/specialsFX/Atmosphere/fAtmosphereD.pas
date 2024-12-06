unit fAtmosphereD;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  Stage.VectorGeometry,
  GLS.BaseClasses,
  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  GLS.LensFlare,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.SkyDome,
 
  GLS.Atmosphere,
  GLS.SimpleNavigation,
  GLS.Behaviours,
  GLS.Coordinates,
  GLS.Color;

type
  TFormAtmosphere = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1:   TGLScene;
    GLCamera1:  TGLCamera;
    GLLensFlare1: TGLLensFlare;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    Planet: TGLSphere;
    dcPlanet: TGLDummyCube;
    sfPlanetoid: TGLSphere;
    CameraTarget: TGLDummyCube;
    GLSkyDome1: TGLSkyDome;
    GLSimpleNavigation1: TGLSimpleNavigation;
    World: TGLDummyCube;
    Panel1: TPanel;
    ButtonRotate: TButton;
    Button4: TButton;
    Button5: TButton;
    Button2: TButton;
    Button3: TButton;
    Button9: TButton;
    Button10: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button8: TButton;
    AtmosphereUpper: TGLAtmosphere;
    rgAtmosphere: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure rgAtmosphereClick(Sender: TObject);
    procedure ButtonRotateClick(Sender: TObject);
  private
    AtmosphereLower:  TGLAtmosphere;
  end;

var
  FormAtmosphere:  TFormAtmosphere;


implementation

{$R *.dfm}

procedure TFormAtmosphere.FormCreate(Sender: TObject);
begin
  AtmosphereLower := TGLAtmosphere.CreateAsChild(dcPlanet);
  AtmosphereLower.Sun := GLLensFlare1;
  AtmosphereLower.SetOptimalAtmosphere(Planet.Radius);

  GLSkyDome1.Bands.Clear;
  GLSkyDome1.Stars.AddRandomStars(5000, ConvertColorVector(clrWhite), False);
end;

procedure TFormAtmosphere.rgAtmosphereClick(Sender: TObject);
begin
  case rgAtmosphere.ItemIndex of
    0:
    begin
      AtmosphereUpper.Visible := True;
      AtmosphereLower.Visible := True;
    end;
    1:
    begin
      AtmosphereUpper.Visible := False;
      AtmosphereLower.Visible := True;
    end;
    2:
    begin
      AtmosphereUpper.Visible := True;
      AtmosphereLower.Visible := False;
    end
    else
    begin
      AtmosphereUpper.Visible := False;
      AtmosphereLower.Visible := False;
    end;
   end;
end;

procedure TFormAtmosphere.ButtonRotateClick(Sender: TObject);
begin
  GLCadencer1.Enabled := not GLCadencer1.Enabled;
end;

procedure TFormAtmosphere.Button2Click(Sender: TObject);
begin
  GLLensFlare1.Slide(0.8);
end;

procedure TFormAtmosphere.Button3Click(Sender: TObject);
begin
  GLLensFlare1.Slide(-0.8);
end;

procedure TFormAtmosphere.Button4Click(Sender: TObject);
begin
  dcPlanet.Slide(-0.5);
end;

procedure TFormAtmosphere.Button5Click(Sender: TObject);
begin
  dcPlanet.Slide(0.5);
end;

procedure TFormAtmosphere.Button6Click(Sender: TObject);
begin
  AtmosphereLower.Visible := not AtmosphereLower.Visible;
  AtmosphereUpper.Visible := not AtmosphereUpper.Visible;
end;

procedure TFormAtmosphere.Button8Click(Sender: TObject);
begin
  AtmosphereLower.TogleBlendingMode;
end;

procedure TFormAtmosphere.Button10Click(Sender: TObject);
begin
  GLCamera1.AdjustDistanceToTarget(1.1);
end;

procedure TFormAtmosphere.Button9Click(Sender: TObject);
begin
  GLCamera1.AdjustDistanceToTarget(1 / 1.1);
end;

procedure TFormAtmosphere.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  Planet.Turn(deltaTime *20);
  GLSceneViewer1.Invalidate;
end;

procedure TFormAtmosphere.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AtmosphereLower.Free;
end;

end.
