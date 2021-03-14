unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Imaging.JPeg,
  Vcl.Dialogs,
   
  GLS.Cadencer,
  GLS.Scene,
  GLS.Objects,
  GLS.VectorTypes,
  GLS.SceneViewer,
  GLS.VectorFileObjects,
  GLS.PersistentClasses,
  GLS.ShadowPlane,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.ParticleFX,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,

  GLS.FileMD3, // MD3 loading into GLScene
  GLS.FileQ3MD3; // Quake3 helper structures and functions

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    ModelCube: TGLDummyCube;
    MatLib: TGLMaterialLibrary;
    Panel1: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    GLShadowPlane1: TGLShadowPlane;
    Legs: TGLActor;
    Torso: TGLActor;
    Head: TGLActor;
    Weapon: TGLActor;
    GunSmoke: TGLDummyCube;
    GLParticleFXRenderer1: TGLParticleFXRenderer;
    GLPointLightPFXManager1: TGLPointLightPFXManager;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label3: TLabel;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    Label4: TLabel;
    ComboSkin: TComboBox;
    Label5: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboSkinChange(Sender: TObject);
  private
    mx, my: Integer;
    procedure LoadSkin(SkinFilePath, SkinShortName: string;
      Actor1, Actor2, Actor3: TGLActor; GraphicFileExt: string);
  public
    LegsTags, TorsoTags, WeaponTags: TMD3TagList;
    procedure BuildModel;
    function InterpolateMatrix(m1, m2: TGLMatrix; delta: single): TGLMatrix;
  end;

var
  Form1: TForm1;
  i: Integer;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Build the model
  BuildModel;

  ModelCube.Scale.SetVector(0.044, 0.044, 0.044);

  Legs.AnimationMode := aamLoop;
  Torso.AnimationMode := aamLoop;

  // Populate the combo boxes with the names of the
  // loaded animations
  Legs.Animations.SetToStrings(ComboBox1.Items);
  Torso.Animations.SetToStrings(ComboBox2.Items);

  // Set up some initial animations
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf('LEGS_IDLE');
  ComboBox2.ItemIndex := ComboBox2.Items.IndexOf('TORSO_STAND');

  // And trigger them
  ComboBox1Change(nil);
  ComboBox2Change(nil);
end;

// MRQZZZ
// In the MaterialLibrary associated with an actor, after the MD3 actor is loaded, we find
// the Materials associated to each FaceGroup of the meshobjects of the Actor.
// So we parse these Materials and if we find a corrispondence in the SkinFile, we load
// the corresponding JPG in the Material
procedure TForm1.LoadSkin(SkinFilePath, SkinShortName: string;
  { Ex: "default" or "red" or "blue" }
  Actor1, Actor2, Actor3: TGLActor; GraphicFileExt: string { Ex : ".JPG" } );
var
  Idx: Integer;
  MatName: string;
  stl, stlBuf, stlPics: TStringList;
  MatLib: TGLMaterialLibrary;
  PicFileName: string;

  procedure FetchStlBuf(Prefix: string);
  var
    stFileName: string;
  begin
    stFileName := SkinFilePath + Prefix + SkinShortName;
    if FileExists(stFileName) then
      stl.LoadFromFile(stFileName);
    stlBuf.AddStrings(stl);
  end;

  function GetMaterialPicFilename(MatName: string): string;
  var
    n: Integer;
  begin
    MatName := Uppercase(MatName);
    for n := 0 to stlBuf.Count - 1 do
    begin
      if pos(MatName, Uppercase(stlBuf[n])) = 1 then
      begin
        Result := ExtractFileName(StringReplace(stlBuf[n], '/', '\',
          [rfReplaceAll]));
        Break;
      end;
     end;
   end;

  procedure DoActorMaterials(Actor: TGLActor);
  var
    t, u: Integer;
  begin
    for t := 0 to Actor.MeshObjects.Count - 1 do
    begin
      for u := 0 to Actor.MeshObjects[t].FaceGroups.Count - 1 do
      begin
        MatName := Actor.MeshObjects[t].FaceGroups[u].MaterialName;
        PicFileName := GetMaterialPicFilename(MatName);
        Idx := stlPics.IndexOf(PicFileName);
        if Idx = -1 then
        begin
          stlPics.AddObject(PicFileName, Actor.MeshObjects[t].FaceGroups[u]);
          PicFileName := SkinFilePath + ChangeFileExt(PicFileName,
            GraphicFileExt);
          if FileExists(PicFileName) then
            MatLib.Materials.GetLibMaterialByName(MatName)
              .Material.Texture.Image.LoadFromFile(PicFileName);
        end
        else
        begin
          Actor.MeshObjects[t].FaceGroups[u].MaterialName :=
            TGLFaceGroup(stlPics.Objects[Idx]).MaterialName;
        end;
      end;
    end;
  end;

begin
  MatLib := Actor1.MaterialLibrary;
  if MatLib = nil then
    Exit;

  stl := TStringList.create;
  stlBuf := TStringList.create;
  stlPics := TStringList.create;

  SkinFilePath := IncludeTrailingBackslash(SkinFilePath);
  SkinShortName := ChangeFileExt(SkinShortName, '.skin');

  FetchStlBuf('Head_');
  FetchStlBuf('Upper_');
  FetchStlBuf('Lower_');

  DoActorMaterials(Actor1);
  DoActorMaterials(Actor2);
  DoActorMaterials(Actor3);

  stl.Free;
  stlBuf.Free;
  stlPics.Free;
end;

procedure TForm1.BuildModel;
// var
// t: integer;
begin
  // Load model data from MD3 files into the actor
  //
  Legs.LoadFromFile('.\model\lower.md3');
  Torso.LoadFromFile('.\model\upper.md3');
  Head.LoadFromFile('.\model\head.md3');
  Weapon.LoadFromFile('.\model\plasma.md3');

  // Load the required tag lists
  // These are used to loacally transform the separate
  // parts of the model into the correct places
  //
  LegsTags := TMD3TagList.Create;
  LegsTags.LoadFromFile('.\model\lower.md3');
  TorsoTags := TMD3TagList.Create;
  TorsoTags.LoadFromFile('.\model\upper.md3');

  // The tag_flash tag in the railgun model gives the
  // transform offset for the nozzle of the gun. I've
  // added a GunSmoke dummycube there to demonstrate with
  // a smoke like effect
  //
  WeaponTags := TMD3TagList.Create;
  WeaponTags.LoadFromFile('.\model\plasma.md3');
  GunSmoke.Matrix^ := WeaponTags.GetTransform('tag_flash', 0);

  // Apply textures to preloaded materials
  // The md3 file loader puts a material into the actors
  // assigned material library (if there is one) with
  // the names of the mesh objects. The skin and/or shader
  // files can tell you which objects need which textures
  // loaded
  //

  // Mrqzzz's quick procedure for loading skins
  LoadSkin('.\model\', 'default', Head, Torso, Legs, '.jpg');

  // Alternative method
  // LoadQ3Skin('.\model\lower_default.skin',Legs);
  // LoadQ3Skin('.\model\upper_default.skin',Torso);
  // LoadQ3Skin('.\model\head_default.skin',Head);

  // Load the weapon textures
  //
  with MatLib.Materials do
  begin
    with GetLibMaterialByName('plasma2').Material.Texture do
      Image.LoadFromFile('.\model\plasma2.jpg');
  end;

  // Load the animation data from the cfg file
  // This procedure populates an animation list from a
  // file or TStrings object. The last parameter tells
  // it which class of animation is to be loaded.
  //
  LoadQ3Anims(Legs.Animations, '.\model\animation.cfg', 'BOTH');
  LoadQ3Anims(Legs.Animations, '.\model\animation.cfg', 'LEGS');
  LoadQ3Anims(Torso.Animations, '.\model\animation.cfg', 'BOTH');
  LoadQ3Anims(Torso.Animations, '.\model\animation.cfg', 'TORSO');
end;

function TForm1.InterpolateMatrix(m1, m2: TGLMatrix; delta: single): TGLMatrix;
var
  i, j: Integer;
begin
  // This is used for interpolating between 2 matrices. The result
  // is used to reposition the model parts each frame.
  //
  for j := 0 to 3 do
    for i := 0 to 3 do
      Result.V[i].V[j] := m1.V[i].V[j] + (m2.V[i].V[j] - m1.V[i].V[j]) * delta;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  m1, m2: TGLMatrix;
begin
  // Set the transform for the torso
  m1 := LegsTags.GetTransform('tag_torso', Legs.CurrentFrame);
  m2 := LegsTags.GetTransform('tag_torso', Legs.NextFrameIndex);
  Torso.Matrix^ := InterpolateMatrix(m1, m2, Legs.CurrentFrameDelta);
  Torso.Roll(-TrackBar1.Position);
  Torso.Turn(-TrackBar2.Position);

  // Set the transform for the head
  m1 := TorsoTags.GetTransform('tag_head', Torso.CurrentFrame);
  m2 := TorsoTags.GetTransform('tag_head', Torso.NextFrameIndex);
  Head.Matrix^ := InterpolateMatrix(m1, m2, Torso.CurrentFrameDelta);
  Head.Roll(-TrackBar3.Position);
  Head.Turn(-TrackBar4.Position);

  // Set the transform for the weapon
  m1 := TorsoTags.GetTransform('tag_weapon', Torso.CurrentFrame);
  m2 := TorsoTags.GetTransform('tag_weapon', Torso.NextFrameIndex);
  Weapon.Matrix^ := InterpolateMatrix(m1, m2, Torso.CurrentFrameDelta);

  GLSceneViewer1.Invalidate;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Legs.SwitchToAnimation(ComboBox1.Text, False);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  Torso.SwitchToAnimation(ComboBox2.Text, False);
end;


procedure TForm1.ComboSkinChange(Sender: TObject);
begin
  LoadSkin('.\model\', ComboSkin.Text, Head, Torso, Legs, '.jpg');
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  if ssRight in Shift then
    GLCamera1.AdjustDistanceToTarget(Power(1.05, my - Y));
  mx := X;
  my := Y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LegsTags.Free;
  TorsoTags.Free;
  WeaponTags.Free;
end;

end.
