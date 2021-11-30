//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.SceneRegister;
(*
  Registration unit for library components, property editors and
  IDE experts.
*)
interface

{$I GLScene.inc}

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  VCL.Forms,
  VCL.Dialogs,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.Graphics,

  ToolsAPI,
  DesignIntf,
  DesignEditors,
  VCLEditors,

  GLS.Context,
  GLS.Scene,
  GLS.Color,
  GLS.ObjectManager,
  GLS.PluginManager,
  GLS.Strings;

type
  TGLLibMaterialNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLSceneViewerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLSceneEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLResolutionProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TGLTextureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TGLTextureImageProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLImageClassProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TGLColorProperty = class(TClassProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  protected
    function ColorToBorderColor(aColor: TGLColorVector; selected: Boolean): TColor;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;
    // ICustomPropertyListDrawing  stuff
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    // CustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TGLSoundFileProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TGLSoundNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLCoordinatesProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLMaterialProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLGUILayoutEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  (* Editor copied from DsgnIntf.
    Could have been avoided, if only that guy at Borland didn't chose to
    publish only half of the stuff (and that's not the only class with
    that problem, most of the subitems handling code in TGLSceneBaseObject is
    here for the same reason...), the "protected" wasn't meant just to lure
    programmers into code they can't reuse... Arrr! and he did that again
    in D6! Grrr... *)
  TGLReuseableDefaultEditor = class(TComponentEditor, IDefaultEditor)
  protected
    FFirst: IProperty;
    FBest: IProperty;
    FContinue: Boolean;
    procedure CheckEdit(const Prop: IProperty);
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); virtual;
  public
    procedure Edit; override;
  end;

  //  Editor for material library.
  TGLMaterialLibraryEditor = class(TGLReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLAnimationNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  (*  Selection editor for TGLSoundLibrary 
    Allows units to be added to the uses clause automatically when
    sound files are loaded into a TGLSoundLibrary at design-time. *)
  TGLSoundLibrarySelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  (*  Selection editor for TGLBaseSceneObject.
    Allows units to be added to the uses clause automatically when
    behaviours/effects are added to a TGLBaseSceneObject at design-time. *)
  TGLBaseSceneObjectSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // Editor for Archive Manager
  TGLSArchiveManagerEditor = class(TGLReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLMaterialComponentNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLLibTextureNameProperty = class(TGLMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibSamplerNameProperty = class(TGLMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibCombinerNameProperty = class(TGLMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibShaderNameProperty = class(TGLMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAttachmentNameProperty = class(TGLMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAsmProgNameProperty = class(TGLMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TPictureFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TShaderFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TAsmProgFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TUniformAutoSetProperty = class(TPropertyEditor)
  private
    procedure PassUniform(const S: string);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLShaderEditorProperty = class(TClassProperty)
  protected
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure OnShaderCheck(Sender: TObject);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

// Register Components
procedure Register;

// Auto-create for object manager
function ObjectManager: TGLObjectManager;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  FmLibMaterialPicker,
  FmGUILayoutEditor,
  FmMaterialEditor,
  FmShaderMemo,
  FmShaderUniformEditor,
  FmVectorEditor,
  FmSceneEditor,

  GLS.ApplicationFileIO,
  GLS.VectorGeometry,
  GLS.ScriptBase,

  GLS.AnimatedSprite,
  GLS.AsyncHDS,
  GLS.AsyncTimer,
  GLS.Atmosphere,
  GLS.AVIRecorder,
  GLS.BaseClasses,
  GLS.BitmapFont,
  GLS.Blur,
  GLS.BumpMapHDS,
  GLS.Cadencer,
  GLS.CameraController,
  GLS.Collision,
  GLS.CompositeImage,
  GLS.Console,
  GLS.Coordinates,
  GLS.DCE,
  GLS.DynamicTexture,
  GLS.EParticleMasksManager,
  GLS.ExplosionFx,
  GLS.Extrusion,
  GLS.FBORenderer,
  GLS.Feedback,
  GLS.FireFX,
  GLS.FPSMovement,
  GLS.GameMenu,
  GLS.GeomObjects,
  GLS.Gizmo,
  GLS.Graph,
  GLS.Graphics,
  GLS.Gui,
  GLS.HeightData,
  GLS.HeightTileFileHDS,
  GLS.HudObjects,
  GLS.Imposter,
  GLS.LensFlare,
  GLS.LinePFX,
  GLS.Material,
  GLS.MaterialEx,
  GLS.MaterialMultiProxy,
  GLS.MaterialScript,
  GLS.Mesh,
  GLS.Mirror,
  GLS.MultiPolygon,
  GLS.MultiProxy,
  GLS.Navigator,
  GLS.Nodes,
  GLS.Objects,
  GLS.ParticleFX,
  GLS.Particles,
  GLS.Perlin,
  GLS.PerlinPFX,
  GLS.Portal,
  GLS.Screen,
  GLS.ShadowHDS,
  GLS.ShadowPlane,
  GLS.ShadowVolume,
  GLS.SimpleNavigation,
  GLS.SkyDome,
  GLS.ProxyObjects,
  GLS.RenderContextInfo,
  GLS.ArchiveManager,
  GLS.Language,
  GLS.Memo,
  GLS.SmoothNavigator,
  GLS.Utils,
  GLSL.AsmShader,
  GLSL.BumpShaders,
  GLSL.ShapeShaders,
  GLSL.LineShaders,
  GLSL.ShaderCombiner,
  GLSL.PhongShader,
  GLSL.PostEffects,
  GLSL.ProjectedTextures,
  GLSL.DiffuseSpecularShader,
  GLSL.MultiMaterialShader,
  GLSL.PostShaders,
  GLSL.Shader,
  GLSL.TextureShaders,
  GLSL.UserShader,

  GLS.ProjectedTextures,
  GLS.State,
  GLS.TerrainRenderer,
  GLS.TexLensFlare,
  GLS.Texture,
  GLS.TexturedHDS,
  GLS.TextureImageEditors,
  GLS.ThorFX,
  GLS.TilePlane,
  GLS.TimeEventsMgr,
  GLS.Trail,
  GLS.Tree,
  GLS.VectorFileObjects,
  GLS.SceneViewer,
  GLS.WaterPlane,
  GLS.Windows,
  GLS.WindowsFont,
  GLS.zBuffer,

//----------------- File formats -------------------------------
  GLS.FileVfsPAK,
//------------ Vector file formats
  GLS.File3DS,
  GLS.FileASE,
  GLS.FileB3D,
  GLS.FileGL2,
  GLS.FileGTS,
  GLS.FileLMTS,
  GLS.FileLWO,
  GLS.FileMD2,
  GLS.FileMD3,
  GLS.FileMD5,
  GLS.FileMDC,
  GLS.FileMS3D,
  GLS.FileNMF,
  GLS.FileNurbs,
  GLS.FileObj,
  GLS.FileOCT,
  GLS.FilePLY,
  GLS.FileQ3BSP,
  GLS.FileSMD,
  GLS.FileSTL,
  GLS.FileVRML,

//----------------- Sound file formats
  GLS.FileWAV,
  GLS.FileMP3,

//----------------- Raster file format
  GLS.FileDDS,
  GLS.FileO3TC,
  GLS.FileHDR,
  GLS.FileJPEG,
  GLS.FilePNG,
  GLS.FileBMP,
  GLS.FileTGA,

  GLS.Sound,
  GLS.SoundFileObjects,
  GLS.SpaceText,
  GLS.Joystick,
  GLS.ScreenSaver,
  GLS.FullScreenViewer,
  GLS.Logger;

var
  vObjectManager: TGLObjectManager;

function ObjectManager: TGLObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TGLObjectManager.Create(nil);
  Result := vObjectManager;
end;

//----------------- TGLSceneViewerEditor ---------------------------------------

procedure TGLSceneViewerEditor.ExecuteVerb(Index: Integer);
var
  source: TGLSceneViewer;
begin
  source := Component as TGLSceneViewer;
  case Index of
    0: source.Buffer.ShowInfo;
  end;
end;

function TGLSceneViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show context info';
  end;
end;

function TGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//----------------- TGLSceneEditor ---------------------------------------------

procedure TGLSceneEditor.Edit;
begin
  with GLSceneEditorForm do
  begin
    SetScene(Self.Component as TGLScene, Self.Designer);
    Show;
  end;
end;

procedure TGLSceneEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLSceneEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Scene Editor';
  end;
end;

function TGLSceneEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//----------------- TGLResolutionProperty ----------------------------------------

function TGLResolutionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TGLResolutionProperty.GetValue: string;
begin
  Result := vVideoModes[GetOrdValue].Description;
end;

procedure TGLResolutionProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to vNumberVideoModes - 1 do
    Proc(vVideoModes[i].Description);
end;

procedure TGLResolutionProperty.SetValue(const Value: string);

const
  Nums = ['0' .. '9'];

var
  XRes, YRes, BPP: Integer;
  Pos, SLength: Integer;
  TempStr: string;

begin
  if CompareText(Value, 'default') <> 0 then
  begin
    // initialize scanning
    TempStr := Trim(Value) + '|'; // ensure at least one delimiter
    SLength := Length(TempStr);
    XRes := 0;
    YRes := 0;
    BPP := 0;
    // contains the string something?
    if SLength > 1 then
    begin
      // determine first number
      for Pos := 1 to SLength do
        if not(AnsiChar(TempStr[Pos]) in Nums) then
          Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes := StrToInt(Copy(TempStr, 1, Pos - 1));
        // search for following non-numerics
        for Pos := Pos to SLength do
          if AnsiChar(TempStr[Pos]) in Nums then
            Break;
        Delete(TempStr, 1, Pos - 1); // take it out of the String
        SLength := Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos := 1 to SLength do
            if not(AnsiChar(TempStr[Pos]) in Nums) then
              Break;
          if Pos <= SLength then
          begin
            YRes := StrToInt(Copy(TempStr, 1, Pos - 1));
            // search for following non-numerics
            for Pos := Pos to SLength do
              if AnsiChar(TempStr[Pos]) in Nums then
                Break;
            Delete(TempStr, 1, Pos - 1); // take it out of the String
            SLength := Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos := 1 to SLength do
                if not(AnsiChar(TempStr[Pos]) in Nums) then
                  Break;
              if Pos <= SLength then
                BPP := StrToInt(Copy(TempStr, 1, Pos - 1));
            end;
          end;
        end;
      end;
    end;
    SetOrdValue(GetIndexFromResolution(XRes, YRes, BPP));
  end
  else
    SetOrdValue(0);
end;

//----------------- TGLTextureProperty -----------------------------------------

function TGLTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;

//----------------- TGLTextureImageProperty ------------------------------------

function TGLTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TGLTextureImageProperty.Edit;
begin
  if EditGLTextureImage(TGLTextureImage(GetOrdValue)) then
    Designer.Modified;
end;

//----------------- TGLImageClassProperty --------------------------------------

function TGLImageClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLImageClassProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  sl: TStrings;
begin
  sl := GetGLTextureImageClassesAsStrings;
  try
    for i := 0 to sl.Count - 1 do
      Proc(sl[i]);
  finally
    sl.Free;
  end;
end;

function TGLImageClassProperty.GetValue: string;
begin
  Result := FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

procedure TGLImageClassProperty.SetValue(const Value: string);
var
  tic: TGLTextureImageClass;
begin
  tic := FindGLTextureImageClassByFriendlyName(Value);
  if Assigned(tic) then
    SetStrValue(tic.ClassName)
  else
    SetStrValue('');
  Modified;
end;

//----------------- TGLColorproperty -----------------------------------------------------------------------------------

procedure TGLColorProperty.Edit;
var
  colorDialog: TColorDialog;
  GLColor: TGLColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    GLColor := TGLColor(GetOrdValue);
    colorDialog.Options := [cdFullOpen];
    colorDialog.Color := ConvertColorVector(GLColor.Color);
    if colorDialog.Execute then
    begin
      GLColor.Color := ConvertWinColor(colorDialog.Color);
      Modified;
    end;
  finally
    colorDialog.Free;
  end;
end;

function TGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paDialog];
end;

procedure TGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TGLColorProperty.GetValue: string;
begin
  Result := ColorManager.GetColorName(TGLColor(GetOrdValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetOrdValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

function TGLColorProperty.ColorToBorderColor(aColor: TGLColorVector; selected: Boolean): TColor;
begin
  if (aColor.X > 0.75) or (aColor.Y > 0.75) or (aColor.Z > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(aColor);
end;

procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  vRight: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  Color: TGLColorVector;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
    try
      vOldPenColor := Pen.Color;
      vOldBrushColor := Brush.Color;

      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

      Color := ColorManager.GetColor(Value);
      Brush.Color := ConvertColorVector(Color);
      Pen.Color := ColorToBorderColor(Color, ASelected);

      Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

      Brush.Color := vOldBrushColor;
      Pen.Color := vOldPenColor;
    finally
      DefaultPropertyListDrawValue(Value, ACanvas,
        Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
    end;
end;

procedure TGLColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TGLColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // Nothing
end;

procedure TGLColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

//----------------- TGLSoundFileProperty -----------------------------------------
function TGLSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TGLSoundFileProperty.GetValue: string;
var
  sample: TGLSoundSample;
begin
  sample := GetComponent(0) as TGLSoundSample;
  if sample.Data <> nil then
    Result := '(' + sample.Data.ClassName + ')'
  else
    Result := '(empty)';
end;

procedure TGLSoundFileProperty.Edit;
var
  ODialog: TOpenDialog;
  sample: TGLSoundSample;
  Desc, F: string;
begin
  sample := GetComponent(0) as TGLSoundSample;
  ODialog := TOpenDialog.Create(nil);
  try
    GetGLSoundFileFormats.BuildFilterStrings(TGLSoundFile, Desc, F);
    ODialog.Filter := Desc;
    if ODialog.Execute then
    begin
      sample.LoadFromFile(ODialog.FileName);
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

//----------------- TGLSoundNameProperty -----------------------------------------

function TGLSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLSoundNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  source: TGLBaseSoundSource;
begin
  source := (GetComponent(0) as TGLBaseSoundSource);
  if Assigned(source.SoundLibrary) then
    with source.SoundLibrary do
      for i := 0 to Samples.Count - 1 do
        Proc(Samples[i].Name);
end;

//----------------- TGLCoordinatesProperty -------------------------------------

function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TGLCoordinatesProperty.Edit;
var
  glc: TGLCoordinates;
  x, y, z: Single;
begin
  glc := TGLCoordinates(GetOrdValue);
  x := glc.x;
  y := glc.y;
  z := glc.z;
  if GLVectorEditorForm.Execute(x, y, z) then
  begin
    glc.AsVector := VectorMake(x, y, z);
    Modified;
  end;
end;

//----------------- TGLGUILayoutEditor -------------------------------

procedure TGLGUILayoutEditor.Edit;
begin
  GUILayoutEditorForm.Execute(TGLGuiLayout(Self.Component));
end;

procedure TGLGUILayoutEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLGUILayoutEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Layout Editor';
  end;
end;

function TGLGUILayoutEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//----------------- TGLReuseableDefaultEditor --------------------------

procedure TGLReuseableDefaultEditor.CheckEdit(const Prop: IProperty);
begin
  if FContinue then
    EditProperty(Prop, FContinue);
end;

procedure TGLReuseableDefaultEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
  BestName: string;
  MethodProperty: IMethodProperty;

  procedure ReplaceBest;
  begin
    FBest := Prop;
    if FFirst = FBest then
      FFirst := nil;
  end;

begin
  if not Assigned(FFirst) and Supports(Prop, IMethodProperty, MethodProperty)
  then
    FFirst := Prop;
  PropName := Prop.GetName;
  BestName := '';
  if Assigned(FBest) then
    BestName := FBest.GetName;
  if CompareText(PropName, 'ONCREATE') = 0 then
    ReplaceBest
  else if CompareText(BestName, 'ONCREATE') <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
      ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;

procedure TGLReuseableDefaultEditor.Edit;
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  FContinue := True;
  Components.Add(Component);
  FFirst := nil;
  FBest := nil;
  try
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
    if FContinue then
      if Assigned(FBest) then
        FBest.Edit
      else if Assigned(FFirst) then
        FFirst.Edit;
  finally
    FFirst := nil;
    FBest := nil;
  end;
end;

//----------------- TGLMaterialLibraryEditor ----------------------------------

procedure TGLMaterialLibraryEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'MATERIALS') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TGLMaterialLibraryEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLMaterialLibraryEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Material Library Editor';
  end;
end;

function TGLMaterialLibraryEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

//----------------- TGLMaterialProperty -------------------------------

function TGLMaterialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TGLMaterialProperty.Edit;
var
  buf: string;
  ml: TGLAbstractMaterialLibrary;
  obj: TPersistent;
  Int: IGLMaterialLibrarySupported;
begin

  if FmMaterialEditor.GLMaterialEditorForm.Execute(
    TGLMaterial(GetOrdValue))
  then
    Modified;
end;


//----------------- TGLLibMaterialNameProperty ---------------------------------

function TGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TGLLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TGLAbstractMaterialLibrary;
  obj: TPersistent;
  Int: IGLMaterialLibrarySupported;
begin
  buf := GetStrValue;
  obj := GetComponent(0);
  if Supports(obj, IGLMaterialLibrarySupported, Int) then
    ml := Int.GetMaterialLibrary
  else
  begin
    ml := nil;
    Assert(False, 'oops, unsupported...');
  end;
  if not Assigned(ml) then
    ShowMessage('Select the material library first.')
  else
//  if FmMaterialEditor.GLMaterialEditorForm.ExecutePicker(buf, ml) then
  if GLLibMaterialPickerForm.Execute(buf, ml) then
    SetStrValue(buf);
end;

//----------------- TGLAnimationNameProperty -----------------------------------

function TGLAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLAnimationNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  animControler: TGLAnimationControler;
  actor: TGLActor;
begin
  animControler := (GetComponent(0) as TGLAnimationControler);
  if Assigned(animControler) then
  begin
    actor := animControler.actor;
    if Assigned(actor) then
      with actor.Animations do
      begin
        for i := 0 to Count - 1 do
          Proc(Items[i].Name);
      end;
  end;
end;

//---------------- TGLBaseSceneObjectSelectionEditor -----------------------

procedure TGLBaseSceneObjectSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TGLBaseSceneObject;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TGLBaseSceneObject) then
    begin
      comp := TGLBaseSceneObject(Designer.Root.Components[i]);
      for j := 0 to comp.Behaviours.Count - 1 do
        Proc(FindUnitName(comp.Behaviours[j]));
      for j := 0 to comp.Effects.Count - 1 do
        Proc(FindUnitName(comp.Effects[j]));
    end;
  end;
end;

//---------------------------- TGLSoundLibrarySelectionEditor -----------------------

procedure TGLSoundLibrarySelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TGLSoundLibrary;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TGLSoundLibrary) then
    begin
      comp := TGLSoundLibrary(Designer.Root.Components[i]);
      for j := 0 to comp.Samples.Count - 1 do
        if Assigned(comp.Samples[j].Data) then
          Proc(FindUnitName(comp.Samples[j].Data));
    end;
  end;
end;

//-------------------- TGLSArchiveManagerEditor -----------------------

procedure TGLSArchiveManagerEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'ARCHIVES') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TGLSArchiveManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLSArchiveManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Archive Manager Editor';
  end;
end;

function TGLSArchiveManagerEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

procedure TGLMaterialComponentNameProperty.Edit;
var
  LOwner: IGLMaterialLibrarySupported;
  LItem: TGLBaseMaterialCollectionItem;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    LItem := TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .Components.GetItemByName(GetStrValue);
    if Assigned(LItem) then
      Designer.SelectComponent(LItem);
    Modified;
  end;
end;

function TGLMaterialComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLLibTextureNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLTextureImageEx);
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLFrameBufferAttachment);
  end;
end;

procedure TGLLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLTextureSampler);
end;

procedure TGLLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLTextureCombiner);
end;

procedure TGLLibShaderNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLShaderEx);
end;

procedure TGLLibAttachmentNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLFrameBufferAttachment);
end;

procedure TGLLibAsmProgNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLASMVertexProgram);
end;

function TPictureFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPictureFileProperty.Edit;
var
  LFileName: string;
begin
  if OpenPictureDialog(LFileName) then
  begin
    SetStrValue(RelativePath(LFileName));
  end;
  Modified;
end;

procedure TShaderFileProperty.Edit;
var
  ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(nil);
  try
    ODialog.Filter := '*.glsl';
    if ODialog.Execute then
    begin
      SetStrValue(RelativePath(ODialog.FileName));
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

function TShaderFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TAsmProgFileProperty.Edit;
var
  ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(nil);
  try
    ODialog.Filter := '*.asm';
    if ODialog.Execute then
    begin
      SetStrValue(RelativePath(ODialog.FileName));
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

function TAsmProgFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TUniformAutoSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

procedure TUniformAutoSetProperty.PassUniform(const S: string);
begin
  GLShaderUniformEditor.AddUniform(TGLBaseShaderModel(GetComponent(0))
    .Uniforms[S]);
end;

procedure TUniformAutoSetProperty.Edit;
var
  LOwner: TGLBaseShaderModel;
begin
  LOwner := TGLBaseShaderModel(GetComponent(0));
  if LOwner.Enabled and LOwner.IsValid then
  begin
    with GLShaderUniformEditor do
    begin
      Clear;
      LOwner.MaterialLibrary.GetNames(AddTextureName, TGLTextureImageEx);
      LOwner.MaterialLibrary.GetNames(AddTextureName, TGLFrameBufferAttachment);
      LOwner.MaterialLibrary.GetNames(AddSamplerName, TGLTextureSampler);
      LOwner.GetUniformNames(PassUniform);
      Execute;
    end;
  end;
end;

function TGLShaderEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TGLShaderEditorProperty.GetStrings: TStrings;
begin
  Result := TStrings(GetOrdValue);
end;

procedure TGLShaderEditorProperty.OnShaderCheck(Sender: TObject);
var
  LShader: TGLShaderEx;
  LContext: TGLContext;
begin
  SetStrings(GLShaderEditorForm.GLSLMemo.Lines);
  LShader := TGLShaderEx(GetComponent(0));
  LContext := LShader.Handle.RenderingContext;
  if Assigned(LContext) then
  begin
    LContext.Activate;
    try
      LShader.DoOnPrepare(LContext);
      GLShaderEditorForm.CompilatorLog.Lines.Add(LShader.InfoLog);
    finally
      LContext.Deactivate;
    end;
  end
  else
    GLShaderEditorForm.CompilatorLog.Lines.Add
      ('There is no any rendering context for work with OpenGL');
end;

procedure TGLShaderEditorProperty.SetStrings(const Value: TStrings);
begin
  SetOrdValue(Longint(Value));
end;

procedure TGLShaderEditorProperty.Edit;
begin
  with GLShaderEditorForm do
  begin
    OnCheck := OnShaderCheck;
    GLSLMemo.Lines.Assign(GetStrings);
    GLSLMemo.CurX := 0;
    GLSLMemo.CurY := 0;
    if ShowModal = mrOk then
    begin
      SetStrings(GLSLMemo.Lines);
      Modified;
    end;
  end;
end;

// ******************************************************
// Register Properties
//*******************************************************
procedure GLSceneRegisterPropertiesInCategories;
begin
  // property types
  // ScreenDepth in Win32FullScreenViewer
  RegisterPropertiesInCategory(strOpenGLCategoryName,
     [TypeInfo(TGLCamera), TypeInfo(TGLSceneBuffer),
     TypeInfo(TGLVSyncMode), TypeInfo(TGLScreenDepth)]);
  // TGLSceneViewer
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLSceneViewer, ['*Render']);
  // GLScene
  RegisterPropertiesInCategory(strOpenGLCategoryName,
    [TypeInfo(TGLObjectsSorting), TypeInfo(TGLProgressEvent),
    TypeInfo(TGLBehaviours), TypeInfo(TGLEffects),
    TypeInfo(TGLDirectRenderEvent), TypeInfo(TGLCameraStyle),
    TypeInfo(TOnCustomPerspective), TypeInfo(TGLScene)]);
  RegisterPropertiesInCategory(strLayoutCategoryName,
    [TypeInfo(TGLObjectsSorting), TypeInfo(TGLNormalDirection)]);
  RegisterPropertiesInCategory(strVisualCategoryName,
    [TypeInfo(TGLVisibilityCulling), TypeInfo(TGLLightStyle), TypeInfo(TGLColor),
    TypeInfo(TGLNormalDirection), TypeInfo(TGLCameraStyle)]);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLBaseSceneObject,
    ['Rotation', 'Direction', 'Position', 'Up', 'Scale', '*Angle', 'ShowAxes', 'FocalLength']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLSceneObject, ['Parts']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLDirectOpenGL, ['UseBuildList']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLProxyObjectOptions)]);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLLightSource, ['*Attenuation', 'Shining', 'Spot*']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLCamera, ['TargetObject']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLCamera, ['DepthOfView', 'SceneScale']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLNonVisualViewer, ['*Render']);

  // GLObjects
  RegisterPropertiesInCategory(strOpenGLCategoryName,
    [TypeInfo(TGLLinesNodes), TypeInfo(TGLLineNodesAspect),
    TypeInfo(TGLLineSplineMode), TypeInfo(TGLLinesOptions)]);
  // GLDummyCube
  RegisterPropertiesInCategory(strLayoutCategoryName, TGLDummyCube, ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLDummyCube, ['CubeSize', 'VisibleAtRunTime']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLPlane, ['*Offset', '*Tiles']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(strLayoutCategoryName, TGLSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLSprite, ['AlphaChannel', 'Rotation']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLNode, ['X', 'Y', 'Z']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLLines,
    ['Antialiased', 'Division', 'Line*', 'NodeSize']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLCube, ['Cube*']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLFrustrum, ['ApexHeight', 'Base*']);
  // GLSpaceText
  RegisterPropertiesInCategory(strLayoutCategoryName, [TypeInfo(TGLTextAdjust)]);
  RegisterPropertiesInCategory(strLocalizableCategoryName, [TypeInfo(TGLSpaceTextCharRange)]);
  RegisterPropertiesInCategory(strVisualCategoryName, [TypeInfo(TGLLineSplineMode),
    TypeInfo(TGLCapType), TypeInfo(TGLNormalSmoothing),
    TypeInfo(TGLArrowHeadStyle), TypeInfo(TGLTextAdjust)]);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLSpaceText,
    ['AllowedDeviation', 'AspectRatio', 'Extrusion', 'Oblique', 'TextHeight']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLSphere,
    ['Bottom', 'Radius', 'Slices', 'Stacks', 'Start', 'Stop']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLDisk,
    ['*Radius', 'Loops', 'Slices']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLCone,
    ['BottomRadius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLCylinder,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLCapsule,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLAnnulus,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLTorus,
    ['*Radius', 'Rings', 'Sides']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLArrowLine,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLPolygon, ['Division']);

  // GLMultiPolygon
  RegisterPropertiesInCategory(strVisualCategoryName, TGLContour, ['Division']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLContourNodes), TypeInfo(TGLContours)]);

  // GLExtrusion
  RegisterPropertiesInCategory(strVisualCategoryName, TGLExtrusionSolid, ['Stacks']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLPipeNode, ['RadiusFactor']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLPipe, ['Division', 'Radius', 'Slices']);
  RegisterPropertiesInCategory(strVisualCategoryName, [TypeInfo(TGLNodes), TypeInfo(TPipeNodesColorMode)]);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLRevolutionSolid, ['Division', 'Slices', 'YOffsetPerTurn']);

  // GLVectorFileObjects
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLActorAnimationMode), TypeInfo(TGLActorAnimations),
    TypeInfo(TGLMeshAutoCenterings), TypeInfo(TGLActorFrameInterpolation),
    TypeInfo(TGLActorAnimationReference), TypeInfo(TGLActor)]);
  RegisterPropertiesInCategory(strLayoutCategoryName, [TypeInfo(TGLMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(strVisualCategoryName, [TypeInfo(TGLMeshAutoCenterings), TypeInfo(TGLActorAnimationReference),
    TypeInfo(TGLMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLFreeForm, ['UseMeshmaterials']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLAnimationControler, ['AnimationName']);
  RegisterPropertiesInCategory(sLinkageCategoryName, TGLAnimationControler, ['AnimationName']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLActorAnimation, ['*Frame']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLActor,
    ['*Frame*', 'Interval', 'OverlaySkeleton', 'UseMeshmaterials']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLActor,  ['OverlaySkeleton']);

  // GLMesh 
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLMeshMode), TypeInfo(TGLVertexMode)]);

  // GLGraph 
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLHeightFieldOptions)]);
  RegisterPropertiesInCategory(strVisualCategoryName, [TypeInfo(TGLHeightFieldColorMode), TypeInfo(TGLSamplingScale),
    TypeInfo(TGLXYZGridLinesStyle), TypeInfo(TGLXYZGridParts)]);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLXYZGrid, ['Antialiased']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLXYZGrid, ['Antialiased', 'Line*']);

  // GLParticles
  RegisterPropertiesInCategory(strLayoutCategoryName, TGLParticles, ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLParticles, ['*Size', 'VisibleAtRunTime']);

  // GLSkydome
  RegisterPropertiesInCategory(strOpenGLCategoryName,
    [TypeInfo(TGLSkyDomeBands), TypeInfo(TGLSkyDomeOptions), TypeInfo(TGLSkyDomeStars)]);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLSkyDomeBand, ['Slices', 'Stacks', '*Angle']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLSkyDomeStar, ['Dec', 'Magnitude', 'RA']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLEarthSkyDome,
    ['Slices', 'Stacks', 'SunElevation', 'Turbidity']);

  // GLMirror
  RegisterPropertiesInCategory(strOpenGLCategoryName,
    [TypeInfo(TGLMirrorOptions), TypeInfo(TGLBaseSceneObject)]);

  // GLParticleFX 
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLBlendingMode)]);
  RegisterPropertiesInCategory(strVisualCategoryName,
    [TypeInfo(TGLBlendingMode), TypeInfo(TPFXLifeColors), TypeInfo(TSpriteColorMode)]);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLParticleFXRenderer, ['ZWrite']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLParticleFXRenderer, ['ZWrite']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TPFXLifeColor, ['LifeTime']);
  RegisterPropertiesInCategory(strVisualCategoryName, TPFXLifeColor, ['LifeTime']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLLifeColoredPFXManager, ['Acceleration', 'ParticleSize']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLPolygonPFXManager, ['NbSides']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLPointLightPFXManager, ['TexMapSize']);

  // GLTerrainRenderer 
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLHeightDataSource)]);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLTerrainRenderer, ['*CLOD*', 'QualityDistance', 'Tile*']);

  // GLzBuffer 
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLMemoryViewer),
    TypeInfo(TGLSceneViewer), TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(strVisualCategoryName, [TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLZShadows, ['DepthFade', '*Shadow', 'Soft', 'Tolerance']);

  // GLHUDObjects
  RegisterPropertiesInCategory(strLayoutCategoryName, [TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(strVisualCategoryName, [TypeInfo(TGLBitmapFont), TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(strLocalizableCategoryName,[TypeInfo(TGLBitmapFont)]);

  // GLTexture
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLMaterial), TypeInfo(TGLMaterialLibrary),
    TypeInfo(TGLLibMaterials), TypeInfo(TGLTextureNeededEvent)]);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLLibMaterial, ['Texture2Name']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLLibMaterial, ['TextureOffset', 'TextureScale']);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLMaterialLibrary, ['TexturePaths']);

  // GLCadencer
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLCadencer)]);

  // GLCollision
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TObjectCollisionEvent)]);

  // GLFireFX
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLFireFXManager,
    ['MaxParticles', 'NoZWrite', 'Paused', 'UseInterval']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLFireFXManager,
    ['Fire*', 'InitialDir', 'NoZWrite', 'Particle*', 'Paused']);

  // GLThorFX
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TCalcPointEvent)]);
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLThorFXManager,
    ['Maxpoints', 'Paused']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLThorFXManager,
    ['Core', 'Glow*', 'Paused', 'Target', 'Vibrate', 'Wildness']);

  // GLBitmapFont 
  RegisterPropertiesInCategory(strOpenGLCategoryName, [TypeInfo(TGLMagFilter), TypeInfo(TGLMinFilter)]);
  RegisterPropertiesInCategory(strLocalizableCategoryName, [TypeInfo(TGLBitmapFontRanges)]);
  RegisterPropertiesInCategory(strLocalizableCategoryName, TGLBitmapFontRange, ['*ASCII']);
  RegisterPropertiesInCategory(strLayoutCategoryName, TGLBitmapFont, ['Char*', '*Interval*', '*Space']);
  RegisterPropertiesInCategory(strLocalizableCategoryName, TGLBitmapFont, ['Glyphs']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLBitmapFont,
    ['Char*', '*Interval*', '*Space', 'Glyphs']);

  // GLHeightData
  RegisterPropertiesInCategory(strOpenGLCategoryName, TGLBitmapHDS, ['MaxPoolSize']);
  RegisterPropertiesInCategory(strVisualCategoryName, TGLBitmapHDS, ['Picture']);
end;

// ******************************************************
// Register Components
//*******************************************************

procedure Register;
begin
  RegisterComponents('GLScene', [TGLScene, TGLSceneViewer, TGLMemoryViewer,
  TGLMaterialLibrary, TGLMaterialLibraryEx, TGLCadencer, TGLGuiLayout,
  TGLBitmapFont, TGLWindowsBitmapFont, TGLScriptLibrary, TGLSoundLibrary,
  TGLFullScreenViewer]);

  RegisterComponents('GLScene PFX',
    [TGLCustomPFXManager, TGLPolygonPFXManager,
    TGLPointLightPFXManager, TGLCustomSpritePFXManager, TGLPerlinPFXManager,
    TGLLinePFXManager, TGLFireFXManager, TGLThorFXManager,
    TGLEParticleMasksManager]);

  RegisterComponents('GLScene Utils', [TGLAsyncTimer, TGLStaticImposterBuilder,
	TGLCollisionManager, TGLAnimationControler, TGLAVIRecorder, TGLDCEManager,
	TGLFPSMovementManager, TGLMaterialScripter, TGLUserInterface, TGLNavigator,
	TGLSmoothNavigator, TGLSmoothUserInterface, TGLTimeEventsMGR,
	TGLApplicationFileIO, TGLVfsPAK, TGLSimpleNavigation, TGLGizmo,
	TGLCameraController, TGLSLanguage, TGLSLogger, TGLSArchiveManager,
	TGLJoystick, TGLScreenSaver, TGLSSynHiMemo]);

  RegisterComponents('GLScene Terrain', [TGLBitmapHDS, TGLCustomHDS,
    TGLHeightTileFileHDS, TGLBumpmapHDS, TGLPerlinHDS, TGLTexturedHDS,
    TGLAsyncHDS, TGLShadowHDS]);

  RegisterComponents('GLScene Shaders', [TGLTexCombineShader, TGLPhongShader,
    TGLUserShader, TGLHiddenLineShader, TGLCelShader, TGLOutlineShader,
    TGLMultiMaterialShader, TGLBumpShader, TGLSLShader,
    TGLSLDiffuseSpecularShader, TGLSLBumpShader, TGLAsmShader,
    TGLShaderCombiner, TGLTextureSharingShader, TGLSLPostBlurShader,
    TGLSLPostThermalVisionShader, TGLSLPostDreamVisionShader, TGLSLPostNightVisionShader,
    TGLSLPostPixelateShader, TGLSLPostPosterizeShader, TGLSLPostFrostShader,
    TGLSLPostTroubleShader]);

  RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
  RegisterComponentEditor(TGLScene, TGLSceneEditor);
  RegisterComponentEditor(TGLMaterialLibrary, TGLMaterialLibraryEditor);
  RegisterComponentEditor(TGLMaterialLibraryEx, TGLMaterialLibraryEditor);
  RegisterComponentEditor(TGLSArchiveManager, TGLSArchiveManagerEditor);

  GLSceneRegisterPropertiesInCategories;

  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TGLResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TGLTexture), TGLMaterial, '', TGLTextureProperty);
  RegisterPropertyEditor(TypeInfo(TGLTextureImage), TGLTexture, '', TGLTextureImageProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLTexture, 'ImageClassName', TGLImageClassProperty);
  RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '', TGLSoundFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLBaseSoundSource, 'SoundName', TGLSoundNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);
  RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterial), nil, '', TGLMaterialProperty);
  RegisterComponentEditor(TGLGuiLayout, TGLGUILayoutEditor);

  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterial, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterial, 'Texture2Name', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSkyBox, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLEParticleMask, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLGameMenu, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialMultiProxyMaster, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSLBumpShader, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSpriteAnimation, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialProxy, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLActorProxy, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLFBORenderer, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLActorAnimationName), TGLAnimationControler, '', TGLAnimationNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLTextureSharingShaderMaterial, 'LibMaterialName',
    TGLLibMaterialNameProperty);
  RegisterSelectionEditor(TGLBaseSceneObject, TGLBaseSceneObjectSelectionEditor);
  RegisterSelectionEditor(TGLSoundLibrary, TGLSoundLibrarySelectionEditor);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterialProperty,
    'NextPass', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLTextureProperties, 'LibTextureName', TGLLibTextureNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLTextureProperties, 'LibSamplerName', TGLLibSamplerNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLMultitexturingProperties, 'LibCombinerName', TGLLibCombinerNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLMultitexturingProperties, 'LibAsmProgName', TGLLibAsmProgNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel3,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel3,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibGeometryShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibGeometryShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibTessControlShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibTessEvalShaderName', TGLLibShaderNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TGLTextureImageEx, 'SourceFile', TPictureFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLShaderEx, 'SourceFile', TShaderFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLASMVertexProgram, 'SourceFile', TAsmProgFileProperty);
  RegisterPropertyEditor(TypeInfo(Boolean), TGLBaseShaderModel, 'AutoFillOfUniforms', TUniformAutoSetProperty);
  RegisterPropertyEditor(TypeInfo(TStringList), TGLShaderEx, 'Source', TGLShaderEditorProperty);
end;

function GetGLSceneVersion: string;
var
  LProject: IOTAProject;
  LExePath, LProjectPath, LSVN, LRevision: string;
begin
  LRevision := Copy(GLSCENE_REVISION, 20, 5);

  // will be assigned after project compilation
  // after each compilation get it from file \.svn\entries in 4-th line
  // and write to file GLSceneRevision
  // in both fail (no \.svn\entries or GLSceneRevision file) get a version value from GLScene.pas
  LProject := GetActiveProject;
  LExePath := ExtractFilePath(ParamStr(0));
  if Assigned(LProject) then
  begin
    LProjectPath := ExtractFilePath(LProject.FileName);
    LSVN := LProjectPath + '.svn\entries';
    if FileExists(LSVN) then
      with TStringList.Create do
        try
          // Load
          LoadFromFile(LSVN);
          if (Count >= 4) and (Trim(Strings[3]) <> '') and
            IsDirectoryWriteable(LExePath) then
          begin
            LRevision := Trim(Strings[3]);
            // Save
            Clear;
            Add(LRevision);
            SaveToFile(LExePath + 'GLSceneRevision');
          end;
        finally
          Free;
        end;
  end
  else if FileExists(LExePath + 'GLSceneRevision') then
    try
      with TStringList.Create do
        try
          LoadFromFile(LExePath + 'GLSceneRevision');
          if (Count >= 1) and (Trim(Strings[0]) <> '') then
            LRevision := Trim(Strings[0]);
        finally
          Free;
        end;
    except
    end;

  // Finally
  Result := Format(GLSCENE_VERSION, [LRevision]);
end;

function GetProjectTargetName: string;
var
  Project: IOTAProject;
begin
  Result := '';
  Project := GetActiveProject;
  if Assigned(Project) then
  begin
    Result := Project.ProjectOptions.TargetName;
    if Length(Result) > 0 then
      ForceDirectories(ExtractFilePath(Result));
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  SplashScreenServices.AddPluginBitmap(GetGLSceneVersion, LoadBitmap(HInstance, 'TGLScene'),
    False, 'MPL 2.0 license', 'VCL version');
  GLS.Utils.IsDesignTime := True;
  GLS.Utils.vProjectTargetName := GetProjectTargetName;
  GLS.Color.vUseDefaultColorSets := True;
  GLS.Coordinates.vUseDefaultCoordinateSets := True;
  ReadVideoModes;

with ObjectManager do
begin
  CreateDefaultObjectIcons(HInstance);
  RegisterSceneObject(TGLCamera, 'Camera', '', HInstance);
  RegisterSceneObject(TGLLightSource, 'LightSource', '', HInstance);
  RegisterSceneObject(TGLDummyCube, 'DummyCube', '', HInstance);

  // Basic geometry
  RegisterSceneObject(TGLSprite, 'Sprite', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLPoints, 'Points', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLLines, 'Lines', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLPlane, 'Plane', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLPolygon, 'Polygon', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCube, 'Cube', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLFrustrum, 'Frustrum', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLSphere, 'Sphere', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLDisk, 'Disk', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCone, 'Cone', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCylinder, 'Cylinder', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCapsule, 'Capsule', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLIcosahedron, 'Icosahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLHexahedron, 'Hexahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLOctahedron, 'Octahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLTetrahedron, 'Tetrahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLSuperellipsoid, 'Superellipsoid', strOCBasicGeometry, HInstance);

  // Advanced geometry
  RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLArrowLine, 'ArrowLine', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLArrowArc, 'ArrowArc', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLAnnulus, 'Annulus', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLPipe, 'Pipe', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLTorus, 'Torus', strOCAdvancedGeometry, HInstance);

  // Mesh objects
  RegisterSceneObject(TGLActor, 'Actor', strOCMeshObjects, HInstance);
  RegisterSceneObject(TGLFreeForm, 'FreeForm', strOCMeshObjects, HInstance);
  RegisterSceneObject(TGLMesh, 'Mesh', strOCMeshObjects, HInstance);
  RegisterSceneObject(TGLTilePlane, 'TilePlane', strOCMeshObjects, HInstance);
  RegisterSceneObject(TGLPortal, 'Portal', strOCMeshObjects, HInstance);
  RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', strOCMeshObjects, HInstance);

  // Graph-plotting objects
  RegisterSceneObject(TGLFlatText, 'FlatText', strOCGraphPlottingObjects, HInstance);
  RegisterSceneObject(TGLHeightField, 'HeightField', strOCGraphPlottingObjects, HInstance);
  RegisterSceneObject(TGLXYZGrid, 'XYZGrid', strOCGraphPlottingObjects, HInstance);

  // Particle systems
  RegisterSceneObject(TGLParticles, 'Particles', strOCParticleSystems, HInstance);
  RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer', strOCParticleSystems, HInstance);

  // Environment objects
  RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome', strOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TGLSkyDome, 'SkyDome', strOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TGLSkyBox, 'SkyBox', strOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TGLAtmosphere, 'Atmosphere', strOCEnvironmentObjects, HInstance);

  // HUD objects.
  RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', strOCHUDObjects, HInstance);
  RegisterSceneObject(TGLHUDText, 'HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TGLResolutionIndependantHUDText,
    'Resolution Independant HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TGLGameMenu, 'GameMenu', strOCHUDObjects, HInstance);
  RegisterSceneObject(TGLConsole, 'Console', strOCHUDObjects, HInstance);

  // GUI objects.
  RegisterSceneObject(TGLBaseControl, 'Root Control', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLForm, 'GLForm', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLPanel, 'GLPanel', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLButton, 'GLButton', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLCheckBox, 'GLCheckBox', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLEdit, 'GLEdit', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLLabel, 'GLLabel', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLScrollbar, 'GLScrollbar', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLStringGrid, 'GLStringGrid', strOCGuiObjects, HInstance);
  RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', strOCGuiObjects, HInstance);

  // Special objects
  RegisterSceneObject(TGLLensFlare, 'LensFlare', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLMirror, 'Mirror', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLZShadows, 'ZShadows', strOCSpecialObjects, HInstance);

  // Texture objects
  RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLProjectedTextures, 'Projected Textures', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLBlur, 'Blur', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLMotionBlur, 'MotionBlur', strOCSpecialObjects, HInstance);

  RegisterSceneObject(TGLSpaceText, 'SpaceText', strOCDoodad, HInstance);

  RegisterSceneObject(TGLTrail, 'GLTrail', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLPostEffect, 'PostEffect', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder', strOCSpecialObjects, HInstance);

  // Doodad objects.
  RegisterSceneObject(TGLTeapot, 'Teapot', strOCDoodad, HInstance);
  RegisterSceneObject(TGLTree, 'Tree', strOCDoodad, HInstance);
  RegisterSceneObject(TGLWaterPlane, 'WaterPlane', strOCDoodad, HInstance);

  // Proxy objects.
  RegisterSceneObject(TGLProxyObject, 'ProxyObject', strOCProxyObjects, HInstance);
  RegisterSceneObject(TGLColorProxy, 'ColorProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TGLActorProxy, 'ActorProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TGLMultiProxy, 'MultiProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy', strOCProxyObjects, HInstance);

  // Other objects.
  RegisterSceneObject(TGLDirectOpenGL, 'Direct OpenGL', '', HInstance);
  RegisterSceneObject(TGLRenderPoint, 'Render Point', '', HInstance);
  RegisterSceneObject(TGLImposter, 'Imposter Sprite', '', HInstance);
  RegisterSceneObject(TGLFeedback, 'OpenGL Feedback', '', HInstance);
  RegisterSceneObject(TGLFBORenderer, 'OpenGL FrameBuffer', '', HInstance);
end;

//------------------------------------------------------
finalization
//------------------------------------------------------

ObjectManager.Free;

end.
