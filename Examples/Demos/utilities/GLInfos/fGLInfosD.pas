unit fGLInfosD;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,

  Stage.TextureFormat,
  Stage.VectorGeometry,
  Stage.Keyboard,
  Stage.VectorTypes,
  Stage.Utils,

  GLS.BaseClasses,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.OpenGLAdapter,
  GLS.Context,
  GLS.Material,
  GLS.Cadencer,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.HUDObjects,
  GLS.Coordinates,
  GLS.Objects,
  GLS.RenderContextInfo,
  GLSL.PostShaders,
  GLSL.CustomShader,
  GLSL.Shader,
  GLS.XCollection,
  GLS.SoundManager,
  GLS.Sounds.BASS,
  GLS.FileWAV,
  GLS.FileMP3,
  GLS.Sounds.FMOD,
  GLS.Canvas,
  GLS.SpaceText,
  GLSL.AsmShader,
  GLSL.PhongShader;

type
  TMainForm = class(TForm)
    SoundLibrary: TGLSoundLibrary;
    FMODPlayer: TGLSMFMOD;
    Viewer: TGLSceneViewer;
    Scene: TGLScene;
    Cadencer: TGLCadencer;
    MaterialLibrary: TGLMaterialLibrary;
    DCCameras: TGLDummyCube;
    Camera1: TGLCamera;
    DriverInfo: TGLDummyCube;
    HUDText_Vendor: TGLResolutionIndependantHUDText;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    HUDText_Renderer: TGLResolutionIndependantHUDText;
    HUDText_Version: TGLResolutionIndependantHUDText;
    HUDText_ExtVersion: TGLResolutionIndependantHUDText;
    HUDText_GLSLVersion: TGLResolutionIndependantHUDText;
    HUDText_NBExt: TGLResolutionIndependantHUDText;
    Background: TGLDummyCube;
    GLSLShader1: TGLSLShader;
    Timer1: TTimer;
    HUDText_Counter: TGLResolutionIndependantHUDText;
    ForeGround: TGLDummyCube;
    HUDLogo: TGLHUDSprite;
    HUDExtensions: TGLDirectOpenGL;
    Extra: TGLDummyCube;
    HUDHelp: TGLResolutionIndependantHUDText;
    GLSpaceText1: TGLSpaceText;
    GLPhongShader1: TGLPhongShader;
    GLLightSource1: TGLLightSource;
    WindowsBitmapFont2: TGLWindowsBitmapFont;
    GLPlane1: TGLPlane;
    SphereSound: TGLSphere;
    procedure FormShow(Sender: TObject);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HUDExtensionsRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLSpaceText1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    procedure HandleKeys(const deltaTime: Double);
  public
    Path: TFileName;
    VendorName: String;
    RendererName: String;
    RendererVersion: String;
    ExtensionVersion: String;
    GLSLVersion: String;
    NBExtensions: String;
    ExtensionsList: TStrings;
    StartLine, MaxLines: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

//====================================================

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Path := ExtractFilePath(ParamStr(0)) + '\audio';
	Path := GetCurrentAssetPath() + '\audio';
  SetCurrentDir(Path);
  SoundLibrary.Samples.AddFile('intro.mp3', 'Music');
  if FMODPlayer <> ActiveSoundManager then
  begin
    // shut down current one, and activate the new one
    if ActiveSoundManager <> nil then
      ActiveSoundManager.Active := False;
    if FMODPlayer <> nil then
      FMODPlayer.Active := True;
    // restart sound
    GetOrCreateSoundEmitter(SphereSound).Playing := True;
  end;
end;

//-------------------------------------------------

procedure TMainForm.FormShow(Sender: TObject);
Var
  ExtStr: String;
  i: Integer;
  dc: HDC;
begin
  HUDLogo.Position.X := Viewer.Width - 150;
  HUDLogo.Position.Y := Viewer.Height - 100;
  Viewer.Buffer.RenderingContext.Activate;
  try
    with Viewer.Buffer do
    begin
      VendorName := String(GL.GetString(GL_VENDOR));
      RendererName := String(GL.GetString(GL_RENDERER));
      RendererVersion := String(GL.GetString(GL_VERSION));
      ExtensionVersion := String(GL.GetString(GL_SHADING_LANGUAGE_VERSION_ARB));
      GLSLVersion := '#' + IntToStr(GL_VERSION);
      ExtensionsList := TStringList.Create;
      ExtStr := String(GL.GetString(GL_EXTENSIONS));
      ExtensionsList.Clear;
      while Length(ExtStr) > 0 do
      begin
        i := Pos(' ', ExtStr);
        if i = 0 then
          i := 255;
        ExtensionsList.Add(Copy(ExtStr, 1, i - 1));
        Delete(ExtStr, 1, i);
      end;
      // Include WGL extensions
      if GL.W_ARB_extensions_string then
      begin
        dc := wglGetCurrentDC();
        ExtStr := String(GL.WGetExtensionsStringARB(dc));
        while Length(ExtStr) > 0 do
        begin
          i := Pos(' ', ExtStr);
          if i = 0 then
            i := 255;
          ExtensionsList.Add(Copy(ExtStr, 1, i - 1));
          Delete(ExtStr, 1, i);
        end;
      end;
      NBExtensions := IntToStr(ExtensionsList.Count);
    end;
  finally
    HUDText_Vendor.Text := 'Vendor             : ' + VendorName;
    HUDText_Renderer.Text := 'Renderer           : ' + RendererName;
    HUDText_Version.Text := 'Renderer Version   : ' + RendererVersion;
    HUDText_ExtVersion.Text := 'OPENGL Version     : ' + ExtensionVersion;
    HUDText_GLSLVersion.Text := 'GLSL Version       : ' + GLSLVersion;
    HUDText_NBExt.Text := 'Extensions supported : ' + NBExtensions;
  end;
  MaxLines := round(((Viewer.Height - 30) - (Viewer.Height / 3)) / 12);
  StartLine := 0;
  Viewer.Invalidate;
end;

//---------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  HUDLogo.Position.X := Viewer.Width - 150;
  HUDLogo.Position.Y := Viewer.Height - 100;
  MaxLines := round(((Viewer.Height - 30) - (Viewer.Height / 3)) / 12);
end;


//-------------------------------------------------------

procedure TMainForm.HandleKeys(const deltaTime: Double);
begin
  if IsKeyDown(VK_UP) then
  begin
    if StartLine > 0 then
      StartLine := StartLine - 1;
  end
  else if IsKeyDown(VK_DOWN) then
  begin
    if StartLine < (ExtensionsList.Count - 1 - MaxLines) then
      StartLine := StartLine + 1;
  end;

  if IsKeyDown(VK_SPACE) then
  begin
    if FMODPlayer.Sources.Items[0].Pause then
      FMODPlayer.Sources.Items[0].Pause := False
    else
      FMODPlayer.Sources.Items[0].Pause := True;
  end;

  if IsKeyDown(VK_ESCAPE) then
  begin
    Application.Terminate;
  end

end;

procedure TMainForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  HandleKeys(deltaTime);
  Viewer.Invalidate;
end;


procedure TMainForm.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  with Shader do
  begin
    param['resolution'].AsVector2f := Vector2fMake(Viewer.Width, Viewer.Height);
    param['time'].AsVector1f := Cadencer.CurrentTime;
  end;
end;

procedure TMainForm.GLSpaceText1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  GLSpaceText1.TurnAngle := 15 * newTime;
end;

procedure TMainForm.HUDExtensionsRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  i, X, ys, Y: Integer;
  Canvas: TGLCanvas;
  r: TRect;
  Color: TColor;
  ext: string;
begin
  ys := round(Viewer.Height / 3) - 20;
  Canvas := TGLCanvas.Create(Viewer.Width, Viewer.Height);

  with Canvas do
  begin
    PenWidth := 2;
    PenColor := $00333333;
    PenAlpha := 0.7;
    r := Rect(10, ys, Viewer.Width - 10, Viewer.Height - 30);
    FillRect(r.Left, r.Top, r.Right, r.Bottom);

    for i := 0 to MaxLines do
    begin
      Color := clWhite;
      X := 15;
      Y := ys + (12 * i);
      ext := ExtensionsList.Strings[StartLine + i];
      if Pos('EXT', ext) > 0 then
        Color := clSkyBlue;
      if Pos('ARB', ext) > 0 then
        Color := clYellow;
      if Pos('NV', ext) > 0 then
        Color := clMoneyGreen;
      if Pos('WGL', ext) > 0 then
        Color := clAqua;

      WindowsBitmapFont2.Font.Size := 8;
      WindowsBitmapFont2.TextOut(rci, X, Y, ext, Color);
    end;
  end;
  Canvas.Free;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  ManagerName: String;
begin
  // some stats
  ManagerName := 'FMOD';
  if ActiveSoundManager <> nil then
    HUDText_Counter.Text := Format('%.2f FPS, %s CPU use : %.2f%%',
      [Viewer.FramesPerSecond, ManagerName, ActiveSoundManager.CPUUsagePercent])
  else
    HUDText_Counter.Text := Format('%.2f FPS', [Viewer.FramesPerSecond]);
  Viewer.ResetPerformanceMonitor;
end;

end.
