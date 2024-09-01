//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.SceneRegister;
(*
  Registration unit for library components, property editors and
  IDE experts.
*)
interface

{$I GXS.Scene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Graphics,

  { TODO : F1026 Files not found: 'VCLEditors' etc.}
  (*need to create instead a custom PropertyEditor like it described in -> *)
  (*ms-help://embarcadero.rs_xe7/rad/Creating_a_Component_Editor_and_a_Property_Editor_for_FireMonkey_Components.html*)
  (*
  VCLEditors,
  ToolsAPI,
  *)

// ToDo DesignTime
(*
  DesignIntf,
  DesignEditors,
  DesignMenus,
*)

  GXS.Strings,

  GXS.Scene,
  GXS.Context,
  GXS.Color,
  GXS.SceneViewer,
  GXS.Utils,
  GXS.ObjectManager;

type
  TgxLibMaterialNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TgxSceneViewerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TgxSceneEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TgxResolutionProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TgxTextureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TgxTextureImageProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TgxImageClassProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TgxColorProperty = class(TClassProperty
//  , ICustomPropertyDrawing,
//    ICustomPropertyListDrawing
    )
  protected
    function ColorToBorderColor(aColor: TgxColorVector;
      selected: Boolean): TgxColor;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;
    // ICustomPropertyListDrawing  stuff
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
    // CustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TgxSoundFileProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TgxSoundNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TgxCoordinatesProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TgxMaterialProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TgxGUILayoutEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  (* Editor copied from DsgnIntf.
    Could have been avoided, if only that guy at Borland didn't chose to
    publish only half of the stuff (and that's not the only class with
    that problem, most of the subitems handling code in TgxSceneBaseObject is
    here for the same reason...), the "protected" wasn't meant just to lure
    programmers into code they can't reuse... Arrr! Grrr... *)
  TgxReuseableDefaultEditor = class(TComponentEditor, IDefaultEditor)
  protected
    FFirst: IProperty;
    FBest: IProperty;
    FContinue: Boolean;
    procedure CheckEdit(const Prop: IProperty);
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); virtual;
  public
    procedure Edit; override;
  end;

  // Editor for material library.
  TgxMaterialLibraryEditor = class(TgxReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TgxAnimationNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  (* Selection editor for TgxSoundLibrary.
    Allows units to be added to the uses clause automatically when
    sound files are loaded into a TgxSoundLibrary at design-time. *)
  TgxSoundLibrarySelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  (* Selection editor for TgxBaseSceneObject.
    Allows units to be added to the uses clause automatically when
    behaviours/effects are added to a TgxBaseSceneObject at design-time. *)
  TgxBaseSceneObjectSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // Editor for Archive Manager.
  TgxSArchiveManagerEditor = class(TgxReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TgxMaterialComponentNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TgxLibTextureNameProperty = class(TgxMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TgxLibSamplerNameProperty = class(TgxMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TgxLibCombinerNameProperty = class(TgxMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TgxLibShaderNameProperty = class(TgxMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TgxLibAttachmentNameProperty = class(TgxMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TgxLibAsmProgNameProperty = class(TgxMaterialComponentNameProperty)
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

  TgxShaderEditorProperty = class(TClassProperty)
  protected
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure OnShaderCheck(Sender: TObject);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

// Auto-create for object manager
function ObjectManager: TgxObjectManager;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
//  FLibMaterialPicker,
//  FGUILayoutEditor,
//  FMaterialEditorForm,
//  FShaderMemo,
//  FShaderUniformEditor,
//  FVectorEditor,
//  FSceneEditor,

  GXS.BaseClasses,
  GXS.VectorTypesExt,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.ApplicationFileIO,

  GLXL.AsmShader,
  GXS.AsyncHDS,
  GXS.AsyncTimer,
  GXS.Atmosphere,
  GXS.AVIRecorder,
  GXS.AnimatedSprite,

  GXS.BitmapFont,
  GXS.Blur,
  GXS.BumpmapHDS,
  GLXL.BumpShaders,
  GXS.Cadencer,
  GXS.CameraController,
  GXS.CelShader,
  GXS.Collision,
  GXS.CompositeImage,
  GXS.Console,
  GXS.Coordinates,
  GXS.DCE,
  GXS.DynamicTexture,
  GXS.EParticleMasksManager,
  GXS.ExplosionFx,
  GXS.Extrusion,
  GXS.FBORenderer,
  GXS.Feedback,
  GXS.FireFX,
  GXS.FPSMovement,
  GXS.GameMenu,
  GXS.GeomObjects,
  GXS.Gizmo,
  GXS.Graph,
  GXS.Graphics,
  GXS.Gui,
  GXS.HeightData,
  GXS.HeightTileFileHDS,
  GXS.HiddenLineShader,
  GXS.HUDObjects,
  GXS.Imposter,
  GXS.LensFlare,
  GXS.LinePFX,
  GXS.Material,
  GXS.MaterialMultiProxy,
  GXS.MaterialScript,
  GXS.Mesh,
  GXS.Mirror,
  GXS.MultiMaterialShader,
  GXS.MultiPolygon,
  GXS.MultiProxy,
  GXS.Navigator,
  GXS.Nodes,
  GXS.Objects,
  GXS.OutlineShader,
  GXS.ParticleFX,
  GXS.Particles,
  GXS.Perlin,
  GXS.PerlinPFX,
  GLXL.PhongShader,
  GXS.Portal,
  GXS.ProjectedTextures,
  GXS.ProxyObjects,
  GXS.RenderContextInfo,
  GXS.ArchiveManager,
  GXS.Screen,
  GXS.ScriptBase,
  GXS.ShadowHDS,
  GXS.ShadowPlane,
  GXS.ShadowVolume,
  GXS.SimpleNavigation,
  GXS.Skydome,
  GXS.SmoothNavigator,

  GLXL.ShaderCombiner,
  GLXL.DiffuseSpecularShader,
  GLXL.PostEffects,
  GLXL.PostShaders,
  GLXL.ProjectedTextures,
  GLXL.Shader,
  GLXL.TextureShaders,
  GLXL.UserShader,

  Soundx.WaveOut,
  GXS.State,
  GXS.TerrainRenderer,
  GXS.TexCombineShader,
  GXS.TexLensFlare,
  GXS.Texture,
  GXS.TexturedHDS,
  GXS.TextureImageEditors,
  GXS.ThorFX,
  GXS.TilePlane,
  GXS.TimeEventsMgr,
  GXS.Trail,
  GXS.Tree,
  GXS.FileTIN,
  GXS.Utils,
  GXS.VectorFileObjects,
  GXS.WaterPlane,
  GXS.Windows,
  GXS.WindowsFont,
  GXS.zBuffer,
  // Image file formats
  GXS.FileDDS,
  GXS.FileTGA,
  // Vector file formats
  GXS.File3DS,
  GXS.FileASE,
  GXS.FileB3D,
  GXS.FileGL2,
  GXS.FileGTS,
  GXS.FileLMTS,
  GXS.FileLWO,
  GXS.FileMD2,
  GXS.FileMD3,
  GXS.FileMD5,
  GXS.FileMDC,
  GXS.FileMS3D,
  GXS.FileNMF,
  GXS.FileNurbs,
  GXS.FileObj,
  GXS.FileOCT,
  GXS.FilePLY,
  GXS.FileQ3BSP,
  GXS.FileSMD,
  GXS.FileSTL,
  GXS.FileVRML,

  // Sound file formats
  GXS.FileWAV,
  GXS.FileMP3,

  // Raster file format
  GXS.FileDDS,
  GXS.FileO3TC,
  GXS.FileHDR,
  GXS.FileJPEG,
  GXS.FilePNG,
  GXS.FileBMP,
  GXS.FileTGA,

  GXS.Sound,
  GXS.SoundFileObjects,
  GXS.SpaceText,
  GXS.Joystick,
  GXS.ScreenSaver,
  GXS.FullScreenViewer;

var
  vObjectManager: TgxObjectManager;

function ObjectManager: TgxObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TgxObjectManager.Create(nil);
  Result := vObjectManager;
end;

procedure TgxSceneViewerEditor.ExecuteVerb(Index: Integer);
var
  source: TgxSceneViewer;
begin
  source := Component as TgxSceneViewer;
  case Index of
    0:
      source.Buffer.ShowInfo;
  end;
end;

function TgxSceneViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show context info';
  end;
end;

function TgxSceneViewerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TgxSceneEditor.Edit;
begin
  with GLXceneEditorForm do
  begin
    SetScene(Self.Component as TgxScene, Self.Designer);
    Show;
  end;
end;

procedure TgxSceneEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TgxSceneEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Scene Editor';
  end;
end;

function TgxSceneEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TgxResolutionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;


function TgxResolutionProperty.GetValue: string;
begin
  Result := vVideoModes[GetOrdValue].Description;
end;

procedure TgxResolutionProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to vNumberVideoModes - 1 do
    Proc(vVideoModes[i].Description);
end;

procedure TgxResolutionProperty.SetValue(const Value: string);

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

function TgxTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;


function TgxTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TgxTextureImageProperty.Edit;
begin
  if EditTextureImage(TgxTextureImage(GetOrdValue)) then
    Designer.Modified;
end;

//---------------------------
// TgxImageClassProperty 
//---------------------------

function TgxImageClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TgxImageClassProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  sl: TStrings;
begin
  sl := GetTextureImageClassesAsStrings;
  try
    for i := 0 to sl.Count - 1 do
      Proc(sl[i]);
  finally
    sl.Free;
  end;
end;

function TgxImageClassProperty.GetValue: string;
begin
  Result := FindTextureImageClass(GetStrValue).FriendlyName;
end;

procedure TgxImageClassProperty.SetValue(const Value: string);
var
  tic: TgxTextureImageClass;
begin
  tic := FindTextureImageClassByFriendlyName(Value);
  if Assigned(tic) then
    SetStrValue(tic.ClassName)
  else
    SetStrValue('');
  Modified;
end;

procedure TgxColorProperty.Edit;
var
  colorDialog: TColorDialog;
  GXS.Color: TgxColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    GXS.Color := TgxColor(GetOrdValue);
{$IFDEF WIN32}
    colorDialog.Options := [cdFullOpen];
{$ENDIF}
    colorDialog.Color := ConvertColorVector(GXS.Color.Color);
    if colorDialog.Execute then
    begin
      GXS.Color.Color := ConvertWinColor(colorDialog.Color);
      Modified;
    end;
  finally
    colorDialog.Free;
  end;
end;

function TgxColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paDialog];
end;

procedure TgxColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TgxColorProperty.GetValue: string;
begin
  Result := ColorManager.GetColorName(TgxColor(GetOrdValue).Color);
end;

procedure TgxColorProperty.SetValue(const Value: string);
begin
  TgxColor(GetOrdValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

function TgxColorProperty.ColorToBorderColor(aColor: TgxColorVector;
  selected: Boolean): TColor;
begin
  if (aColor.X > 0.75) or (aColor.Y > 0.75) or (aColor.Z > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(aColor);
end;

procedure TgxColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TgxColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  vRight: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  Color: TgxColorVector;
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

procedure TgxColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TgxColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // Nothing
end;

procedure TgxColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TgxSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TgxSoundFileProperty.GetValue: string;
var
  sample: TgxSoundSample;
begin
  sample := GetComponent(0) as TgxSoundSample;
  if sample.Data <> nil then
    Result := '(' + sample.Data.ClassName + ')'
  else
    Result := '(empty)';
end;


procedure TgxSoundFileProperty.Edit;
var
  ODialog: TOpenDialog;
  sample: TgxSoundSample;
  Desc, F: string;
begin
  sample := GetComponent(0) as TgxSoundSample;
  ODialog := TOpenDialog.Create(nil);
  try
    GetGLSoundFileFormats.BuildFilterStrings(TgxSoundFile, Desc, F);
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

//---------------------------------------------------------

function TgxSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TgxSoundNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  source: TgxBaseSoundSource;
begin
  source := (GetComponent(0) as TgxBaseSoundSource);
  if Assigned(source.SoundLibrary) then
    with source.SoundLibrary do
      for i := 0 to Samples.Count - 1 do
        Proc(Samples[i].Name);
end;

//---------------------------------------------------------
{ TgxCoordinatesProperty }
//--------------------------------------------------------
function TgxCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TgxCoordinatesProperty.Edit;
var
  glc: TgxCoordinates;
  x, y, z: Single;
begin
  glc := TgxCoordinates(GetOrdValue);
  x := glc.x;
  y := glc.y;
  z := glc.z;
  if VectorEditorForm.Execute(x, y, z) then
  begin
    glc.AsVector := VectorMake(x, y, z);
    Modified;
  end;
end;

//--------------------------------------------------------

function TgxMaterialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TgxMaterialProperty.Edit;
begin
  if FMaterialEditorForm.MaterialEditorForm.Execute(TgxMaterial(GetOrdValue))
  then
    Modified;
end;

procedure TgxGUILayoutEditor.Edit;
begin
  GUILayoutEditorForm.Execute(TgxGuiLayout(Self.Component));
end;

procedure TgxGUILayoutEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TgxGUILayoutEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Layout Editor';
  end;
end;

function TgxGUILayoutEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TgxReuseableDefaultEditor.CheckEdit(const Prop: IProperty);
begin
  if FContinue then
    EditProperty(Prop, FContinue);
end;

procedure TgxReuseableDefaultEditor.EditProperty(const Prop: IProperty;
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

procedure TgxReuseableDefaultEditor.Edit;
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

procedure TgxMaterialLibraryEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'MATERIALS') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TgxMaterialLibraryEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TgxMaterialLibraryEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Material Library Editor';
  end;
end;

function TgxMaterialLibraryEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

function TgxLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TgxLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TgxAbstractMaterialLibrary;
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
  else if LibMaterialPicker.Execute(buf, ml) then
    SetStrValue(buf);
end;


function TgxAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TgxAnimationNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  animControler: TgxAnimationControler;
  actor: TgxActor;
begin
  animControler := (GetComponent(0) as TgxAnimationControler);
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

procedure TgxBaseSceneObjectSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TgxBaseSceneObject;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TgxBaseSceneObject) then
    begin
      comp := TgxBaseSceneObject(Designer.Root.Components[i]);
      for j := 0 to comp.Behaviours.Count - 1 do
        Proc(FindUnitName(comp.Behaviours[j]));
      for j := 0 to comp.Effects.Count - 1 do
        Proc(FindUnitName(comp.Effects[j]));
    end;
  end;
end;

procedure TgxSoundLibrarySelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TgxSoundLibrary;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TgxSoundLibrary) then
    begin
      comp := TgxSoundLibrary(Designer.Root.Components[i]);
      for j := 0 to comp.Samples.Count - 1 do
        if Assigned(comp.Samples[j].Data) then
          Proc(FindUnitName(comp.Samples[j].Data));
    end;
  end;
end;

procedure TgxSArchiveManagerEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'ARCHIVES') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TgxSArchiveManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TgxSArchiveManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Archive Manager Editor';
  end;
end;

function TgxSArchiveManagerEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

procedure TgxMaterialComponentNameProperty.Edit;
var
  LOwner: IGLMaterialLibrarySupported;
  LItem: TgxBaseMaterialCollectionItem;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    LItem := TgxMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .Components.GetItemByName(GetStrValue);
    if Assigned(LItem) then
      Designer.SelectComponent(LItem);
    Modified;
  end;
end;

function TgxMaterialComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TgxLibTextureNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    TgxMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TgxTextureImageEx);
    TgxMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TgxFrameBufferAttachment);
  end;
end;

procedure TgxLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TgxMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TgxTextureSampler);
end;

procedure TgxLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TgxMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TgxTextureCombiner);
end;

procedure TgxLibShaderNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TgxMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TgxShaderEx);
end;

procedure TgxLibAttachmentNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TgxMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TgxFrameBufferAttachment);
end;

procedure TgxLibAsmProgNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TgxMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TgxASMVertexProgram);
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

{ TUniformAutoSetProperty }

function TUniformAutoSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

procedure TUniformAutoSetProperty.PassUniform(const S: string);
begin
  ShaderUniformEditor.AddUniform(TgxBaseShaderModel(GetComponent(0))
    .Uniforms[S]);
end;

procedure TUniformAutoSetProperty.Edit;
var
  LOwner: TgxBaseShaderModel;
begin
  LOwner := TgxBaseShaderModel(GetComponent(0));
  if LOwner.Enabled and LOwner.IsValid then
  begin
    with ShaderUniformEditor do
    begin
      Clear;
      LOwner.MaterialLibrary.GetNames(AddTextureName, TgxTextureImageEx);
      LOwner.MaterialLibrary.GetNames(AddTextureName, TgxFrameBufferAttachment);
      LOwner.MaterialLibrary.GetNames(AddSamplerName, TgxTextureSampler);
      LOwner.GetUniformNames(PassUniform);
      Execute;
    end;
  end;
end;

function TgxShaderEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TgxShaderEditorProperty.GetStrings: TStrings;
begin
  Result := TStrings(GetOrdValue);
end;

procedure TgxShaderEditorProperty.OnShaderCheck(Sender: TObject);
var
  LShader: TgxShaderEx;
  LContext: TgxContext;
begin
  SetStrings(GLShaderEditorForm.GLSLMemo.Lines);
  LShader := TgxShaderEx(GetComponent(0));
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

procedure TgxShaderEditorProperty.SetStrings(const Value: TStrings);
begin
  SetOrdValue(Longint(Value));
end;

procedure TgxShaderEditorProperty.Edit;
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

// ******************************************************************************
procedure RegisterPropertiesInCategories;
begin
  // GXS.SceneViewer
{$IFDEF WIN32}
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxCamera), TypeInfo(TgxSceneBuffer),
    TypeInfo(TVSyncMode), TypeInfo(TgxScreenDepth)]);
{$ENDIF}
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxSceneViewer, ['*Render']);

  // GXS.Scene
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(Tgxx.ObjectsSorting), TypeInfo(TgxProgressEvent),
    TypeInfo(TgxBehaviours), TypeInfo(TgxEffects), TypeInfo(TDirectRenderEvent), TypeInfo(TgxCameraStyle),
    TypeInfo(TOnCustomPerspective), TypeInfo(TgxScene)]);
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(Tgxx.ObjectsSorting), TypeInfo(TgxNormalDirection)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TgxVisibilityCulling), TypeInfo(TLightStyle),
    TypeInfo(TgxColor), TypeInfo(TgxNormalDirection), TypeInfo(TgxCameraStyle)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxBaseSceneObject,
    ['Rotation', 'Direction', 'Position', 'Up', 'Scale', '*Angle', 'ShowAxes', 'FocalLength']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxSceneObject, ['Parts']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxDirectOpenVX, ['UseBuildList']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxProxyObjectOptions)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxLightSource, ['*Attenuation', 'Shining', 'Spot*']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxCamera, ['TargetObject']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxCamera, ['DepthOfView', 'SceneScale']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxNonVisualViewer, ['*Render']);

  // GXS.Objects 
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxLinesNodes), TypeInfo(TLineNodesAspect),
    TypeInfo(TgxLineSplineMode), TypeInfo(TgxLinesOptions)]);
{$IFDEF WIN32}
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TgxTextAdjust)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName, [TypeInfo(TSpaceTextCharRange)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TgxLineSplineMode),
    TypeInfo(TgxCapType), TypeInfo(TgxNormalSmoothing), TypeInfo(TgxArrowHeadStyle), TypeInfo(TgxTextAdjust)]);
{$ENDIF}
  RegisterPropertiesInCategory(sLayoutCategoryName, TgxDummyCube, ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxDummyCube, ['CubeSize', 'VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxPlane, ['*Offset', '*Tiles']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sLayoutCategoryName, TgxSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxSprite, ['AlphaChannel', 'Rotation']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxNode, ['X', 'Y', 'Z']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxLines, ['Antialiased', 'Division', 'Line*', 'NodeSize']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxCube, ['Cube*']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxFrustrum, ['ApexHeight', 'Base*']);
{$IFDEF WIN32}
  RegisterPropertiesInCategory(sVisualCategoryName, TgxSpaceText, ['AllowedDeviation', 'AspectRatio', 'Extrusion', 'Oblique', 'TextHeight']);
{$ENDIF}
  RegisterPropertiesInCategory(sVisualCategoryName, TgxSphere, ['Bottom', 'Radius', 'Slices', 'Stacks', 'Start', 'Stop']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxDisk, ['*Radius', 'Loops', 'Slices']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxCone, ['BottomRadius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxCylinder, ['*Radius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxCapsule, ['*Radius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxAnnulus, ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxTorus, ['*Radius', 'Rings', 'Sides']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxArrowLine, ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxPolygon, ['Division']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxContourNodes), TypeInfo(TgxContours)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxContour, ['Division']);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TgxNodes), TypeInfo(TPipeNodesColorMode)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxRevolutionSolid, ['Division', 'Slices', 'YOffsetPerTurn']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxExtrusionSolid, ['Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxPipeNode, ['RadiusFactor']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxPipe, ['Division', 'Radius', 'Slices']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxActorAnimationMode), TypeInfo(TgxActorAnimations),
    TypeInfo(TMeshAutoCenterings), TypeInfo(TActorFrameInterpolation),
	TypeInfo(TgxActorAnimationReference), TypeInfo(TgxActor)]);
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TMeshAutoCenterings),
    TypeInfo(TgxActorAnimationReference), TypeInfo(TMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxFreeForm, ['UseMeshmaterials']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxAnimationControler, ['AnimationName']);
  RegisterPropertiesInCategory(sLinkageCategoryName, TgxAnimationControler, ['AnimationName']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxActorAnimation, ['*Frame']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxActor, ['*Frame*', 'Interval', 'OverlaySkeleton', 'UseMeshmaterials']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxActor, ['OverlaySkeleton']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TMeshMode), TypeInfo(TVertexMode)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxHeightFieldOptions)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TgxHeightFieldColorMode),
    TypeInfo(TgxSamplingScale), TypeInfo(TXYZGridLinesStyle), TypeInfo(TXYZGridParts)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxXYZGrid, ['Antialiased']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxXYZGrid, ['Antialiased', 'Line*']);

  RegisterPropertiesInCategory(sLayoutCategoryName, TgxParticles, ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxParticles, ['*Size', 'VisibleAtRunTime']);

  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxSkyDomeBands), TypeInfo(TgxSkyDomeOptions), TypeInfo(TgxSkyDomeStars)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxSkyDomeBand, ['Slices', 'Stacks', '*Angle']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxSkyDomeStar, ['Dec', 'Magnitude', 'RA']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxEarthSkyDome, ['Slices', 'Stacks', 'SunElevation', 'Turbidity']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TMirrorOptions), TypeInfo(TgxBaseSceneObject)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TBlendingMode)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TBlendingMode), TypeInfo(TPFXLifeColors), TypeInfo(TSpriteColorMode)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxParticleFXRenderer, ['ZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxParticleFXRenderer, ['ZWrite']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TPFXLifeColor, ['LifeTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TPFXLifeColor, ['LifeTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxLifeColoredPFXManager, ['Acceleration', 'ParticleSize']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxPolygonPFXManager, ['NbSides']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxPointLightPFXManager, ['TexMapSize']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxHeightDataSource)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxTerrainRenderer, ['*CLOD*', 'QualityDistance', 'Tile*']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxMemoryViewer), TypeInfo(TgxSceneViewer), TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxZShadows, ['DepthFade', '*Shadow', 'Soft', 'Tolerance']);
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TgxBitmapFont), TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName, [TypeInfo(TgxBitmapFont)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxMaterial), TypeInfo(TgxMaterialLibrary),
    TypeInfo(TgxLibMaterials), TypeInfo(TTextureNeededEvent)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxLibMaterial, ['Texture2Name']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxLibMaterial, ['TextureOffset', 'TextureScale']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxMaterialLibrary, ['TexturePaths']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxCadencer)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TObjectCollisionEvent)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxFireFXManager, ['MaxParticles', 'NoZWrite', 'Paused', 'UseInterval']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxFireFXManager, ['Fire*', 'InitialDir', 'NoZWrite', 'Particle*', 'Paused']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TCalcPointEvent)]);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxThorFXManager, ['Maxpoints', 'Paused']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxThorFXManager, ['Core', 'Glow*', 'Paused', 'Target', 'Vibrate', 'Wildness']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TgxMagFilter), TypeInfo(TgxMinFilter)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName, [TypeInfo(TgxBitmapFontRanges)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName, TgxBitmapFontRange, ['*ASCII']);
  RegisterPropertiesInCategory(sLayoutCategoryName, TgxBitmapFont, ['Char*', '*Interval*', '*Space']);
  RegisterPropertiesInCategory(sLocalizableCategoryName, TgxBitmapFont, ['Glyphs']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxBitmapFont, ['Char*', '*Interval*', '*Space', 'Glyphs']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TgxBitmapHDS, ['MaxPoolSize']);
  RegisterPropertiesInCategory(sVisualCategoryName, TgxBitmapHDS, ['Picture']);
end;

procedure Register;
begin
  RegisterComponents('GXScene', [TgxScene, TgxSceneViewer, TgxMemoryViewer,
    TgxMaterialLibrary, TgxMaterialLibraryEx, TgxCadencer, TgxGuiLayout,
    TgxBitmapFont, TgxWindowsBitmapFont, TgxScriptLibrary, TgxSoundLibrary,
    TgxFullScreenViewer]);

  RegisterComponents('GXScene PFX', [TgxCustomPFXManager, TgxPolygonPFXManager,
    TgxPointLightPFXManager, TgxCustomSpritePFXManager, TgxPerlinPFXManager,
    TgxLinePFXManager, TgxFireFXManager, TgxThorFXManager,
    TgxEParticleMasksManager]);

  RegisterComponents('GXScene Utils', [TgxAsyncTimer, TgxStaticImposterBuilder,
    TgxCollisionManager, TgxAnimationControler, TgxAVIRecorder, TgxDCEManager,
    TgxFPSMovementManager, TgxMaterialScripter, TgxUserInterface, TgxNavigator,
    TgxSmoothNavigator, TgxSmoothUserInterface, TgxTimeEventsMGR,
    TgxApplicationFileIO, TgxVfsPAK, TgxSimpleNavigation, TgxGizmo,
    TgxCameraController, TgxSLanguage, TgxSLogger, TgxSArchiveManager,
    TgxJoystick, TgxScreenSaver, TgxSSynHiMemo]);

  RegisterComponents('GXScene Terrain', [TgxBitmapHDS, TgxCustomHDS,
    TgxHeightTileFileHDS, TgxBumpmapHDS, TgxPerlinHDS, TgxTexturedHDS,
    TgxAsyncHDS, TgxShadowHDS]);

  RegisterComponents('GXScene Shaders', [TgxTexCombineShader, TgxPhongShader,
    TgxUserShader, TgxHiddenLineShader, TgxCelShader, TgxOutlineShader,
    TgxMultiMaterialShader, TgxBumpShader, TgxGLSLShader,
    TgxSLDiffuseSpecularShader, TgxSLBumpShader, TgxAsmShader,
    TgxShaderCombiner, TgxTextureSharingShader, TgxSLPostBlurShader]);

  RegisterComponentEditor(TgxSceneViewer, TgxSceneViewerEditor);
  RegisterComponentEditor(TgxScene, TgxSceneEditor);
  RegisterComponentEditor(TgxMaterialLibrary, TgxMaterialLibraryEditor);
  RegisterComponentEditor(TgxMaterialLibraryEx, TgxMaterialLibraryEditor);
  RegisterComponentEditor(TgxSArchiveManager, TgxSArchiveManagerEditor);

  RegisterPropertiesInCategories;

  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TgxResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TgxTexture), TgxMaterial, '',
    TgxTextureProperty);
  RegisterPropertyEditor(TypeInfo(TgxTextureImage), TgxTexture, '',
    TgxTextureImageProperty);
  RegisterPropertyEditor(TypeInfo(string), TgxTexture, 'ImageClassName',
    TgxImageClassProperty);

  RegisterPropertyEditor(TypeInfo(TgxSoundFile), TgxSoundSample, '',
    TgxSoundFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TgxBaseSoundSource, 'SoundName',
    TgxSoundNameProperty);

  RegisterPropertyEditor(TypeInfo(TgxCoordinates), nil, '',
    TgxCoordinatesProperty);

  RegisterPropertyEditor(TypeInfo(TgxColor), nil, '', TgxColorProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterial), nil, '', TgxMaterialProperty);
  RegisterComponentEditor(TgxGuiLayout, TgxGUILayoutEditor);

  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxMaterial, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxLibMaterial,
    'Texture2Name', TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxSkyBox, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxEParticleMask, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxGameMenu, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName),
    TgxMaterialMultiProxyMaster, '', TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxSLBumpShader, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TSpriteAnimation, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxMaterialProxy, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxActorProxy, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxFBORenderer, '',
    TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxActorAnimationName), TgxAnimationControler,
    '', TgxAnimationNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName),
    TgxTextureSharingShaderMaterial, 'LibMaterialName', TgxLibMaterialNameProperty);
  RegisterSelectionEditor(TgxBaseSceneObject, TgxBaseSceneObjectSelectionEditor);
  RegisterSelectionEditor(TgxSoundLibrary, TgxSoundLibrarySelectionEditor);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TgxLibMaterialProperty,
    'NextPass', TgxLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName),
    TgxTextureProperties, 'LibTextureName', TgxLibTextureNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName),
    TgxTextureProperties, 'LibSamplerName', TgxLibSamplerNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName),
    TgxMultitexturingProperties, 'LibCombinerName', TgxLibCombinerNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName),
    TgxMultitexturingProperties, 'LibAsmProgName', TgxLibAsmProgNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel3,
    'LibVertexShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel3,
    'LibFragmentShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel4,
    'LibVertexShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel4,
    'LibFragmentShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel4,
    'LibGeometryShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel5,
    'LibVertexShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel5,
    'LibFragmentShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel5,
    'LibGeometryShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel5,
    'LibTessControlShaderName', TgxLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TgxMaterialComponentName), TgxShaderModel5,
    'LibTessEvalShaderName', TgxLibShaderNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TgxTextureImageEx, 'SourceFile', TPictureFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TgxShaderEx, 'SourceFile', TShaderFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TgxASMVertexProgram, 'SourceFile', TAsmProgFileProperty);

  RegisterPropertyEditor(TypeInfo(Boolean), TgxBaseShaderModel, 'AutoFillOfUniforms', TUniformAutoSetProperty);
  RegisterPropertyEditor(TypeInfo(TStringList), TgxShaderEx, 'Source', TgxShaderEditorProperty);
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

SplashScreenServices.AddPluginBitmap(GetGLXceneVersion,
  LoadBitmap(HInstance, 'TgxScene'), False, 'MPL 2 license', 'FMX version');

GXS.CrossPlatform.IsDesignTime := True;
GXS.CrossPlatform.vProjectTargetName := GetProjectTargetName;
GXS.Color.vUseDefaultColorSets := True;
GXS.Coordinates.vUseDefaultCoordinateSets := True;
ReadVideoModes;

with ObjectManager do
begin
  CreateDefaultObjectIcons(HInstance);
  RegisterSceneObject(TgxCamera, 'Camera', '', HInstance);
  RegisterSceneObject(TgxLightSource, 'LightSource', '', HInstance);
  RegisterSceneObject(TgxDummyCube, 'DummyCube', '', HInstance);

  // Basic geometry
  RegisterSceneObject(TgxSprite, 'Sprite', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxPoints, 'Points', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxLines, 'Lines', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxPlane, 'Plane', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxPolygon, 'Polygon', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxCube, 'Cube', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxFrustrum, 'Frustrum', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxSphere, 'Sphere', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxDisk, 'Disk', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxCone, 'Cone', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxCylinder, 'Cylinder', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxCapsule, 'Capsule', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxDodecahedron, 'Dodecahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxIcosahedron, 'Icosahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxOctahedron, 'Octahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxTetrahedron, 'Tetrahedron', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TgxSuperellipsoid, 'Superellipsoid', strOCBasicGeometry, HInstance);

  // Advanced geometry
  RegisterSceneObject(TgxAnimatedSprite, 'Animated Sprite', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxArrowLine, 'ArrowLine', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxArrowArc, 'ArrowArc', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxAnnulus, 'Annulus', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxExtrusionSolid, 'ExtrusionSolid', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxMultiPolygon, 'MultiPolygon', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxPipe, 'Pipe', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxRevolutionSolid, 'RevolutionSolid', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TgxTorus, 'Torus', strOCAdvancedGeometry, HInstance);

  // Mesh objects
  RegisterSceneObject(TgxActor, 'Actor', strOCMeshObjects, HInstance);
  RegisterSceneObject(TgxFreeForm, 'FreeForm', strOCMeshObjects, HInstance);
  RegisterSceneObject(TgxMesh, 'Mesh', strOCMeshObjects, HInstance);
  RegisterSceneObject(TgxTilePlane, 'TilePlane', strOCMeshObjects, HInstance);
  RegisterSceneObject(TgxPortal, 'Portal', strOCMeshObjects, HInstance);
  RegisterSceneObject(TgxTerrainRenderer, 'TerrainRenderer', strOCMeshObjects, HInstance);

  // Graph-plotting objects
  RegisterSceneObject(TgxFlatText, 'FlatText', strOCGraphPlottingObjects, HInstance);
  RegisterSceneObject(TgxHeightField, 'HeightField', strOCGraphPlottingObjects, HInstance);
  RegisterSceneObject(TgxXYZGrid, 'XYZGrid', strOCGraphPlottingObjects, HInstance);

  // Particle systems
  RegisterSceneObject(TgxParticles, 'Particles', strOCParticleSystems, HInstance);
  RegisterSceneObject(TgxParticleFXRenderer, 'PFX Renderer', strOCParticleSystems, HInstance);

  // Environment objects
  RegisterSceneObject(TgxEarthSkyDome, 'EarthSkyDome', strOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TgxSkyDome, 'SkyDome', strOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TgxSkyBox, 'SkyBox', strOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TgxAtmosphere, 'Atmosphere', strOCEnvironmentObjects, HInstance);

  // HUD objects.
  RegisterSceneObject(TgxHUDSprite, 'HUD Sprite', strOCHUDObjects, HInstance);
  RegisterSceneObject(TgxHUDText, 'HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TgxResolutionIndependantHUDText,
    'Resolution Independant HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TgxAbsoluteHUDText, 'Absolute HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TgxGameMenu, 'GameMenu', strOCHUDObjects, HInstance);
  RegisterSceneObject(TgxConsole, 'Console', strOCHUDObjects, HInstance);

  // GUI objects.
  RegisterSceneObject(TgxBaseControl, 'Root Control', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxPopupMenu, 'PopupMenu', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxForm, 'Form', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxPanel, 'Panel', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxButton, 'Button', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxCheckBox, 'CheckBox', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxEdit, 'Edit', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxLabel, 'Label', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxAdvancedLabel, 'AdvancedLabel', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxScrollbar, 'Scrollbar', strOCGuiObjects, HInstance);
  RegisterSceneObject(StringGrid, 'StringGrid', strOCGuiObjects, HInstance);
  RegisterSceneObject(TgxCustomControl, 'BitmapControl', strOCGuiObjects, HInstance);

  // Special objects
  RegisterSceneObject(TgxLensFlare, 'LensFlare', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxTextureLensFlare, 'TextureLensFlare', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxMirror, 'Mirror', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxShadowPlane, 'ShadowPlane', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxShadowVolume, 'ShadowVolume', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxZShadows, 'ZShadows', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxSLTextureEmitter, 'GLSL Texture Emitter', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxSLProjectedTextures, 'GLSL Projected Textures', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxTextureEmitter, 'Texture Emitter', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxProjectedTextures, 'Projected Textures', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxBlur, 'Blur', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxMotionBlur, 'MotionBlur', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxSpaceText, 'SpaceText', strOCDoodad, HInstance);
  RegisterSceneObject(TgxTrail, 'Trail', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxPostEffect, 'PostEffect', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TgxPostShaderHolder, 'PostShaderHolder', strOCSpecialObjects, HInstance);

  // Doodad objects.
  RegisterSceneObject(TgxTeapot, 'Teapot', strOCDoodad, HInstance);
  RegisterSceneObject(TgxTree, 'Tree', strOCDoodad, HInstance);
  RegisterSceneObject(TgxWaterPlane, 'WaterPlane', strOCDoodad, HInstance);

  // Proxy objects.
  RegisterSceneObject(TgxProxyObject, 'ProxyObject', strOCProxyObjects, HInstance);
  RegisterSceneObject(TgxColorProxy, 'ColorProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TgxFreeFormProxy, 'FreeFormProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TgxMaterialProxy, 'MaterialProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TgxActorProxy, 'ActorProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TgxMultiProxy, 'MultiProxy', strOCProxyObjects, HInstance);
  RegisterSceneObject(TgxMaterialMultiProxy, 'MaterialMultiProxy', strOCProxyObjects, HInstance);

  // Other objects.
  RegisterSceneObject(TgxDirectOpenGL, 'Direct OpenGL', '', HInstance);
  RegisterSceneObject(TgxDirectVulkan, 'Direct Vulkan', '', HInstance);
  RegisterSceneObject(TgxRenderPoint, 'Render Point', '', HInstance);
  RegisterSceneObject(TgxImposter, 'Imposter Sprite', '', HInstance);
  RegisterSceneObject(TgxFeedback, 'OpenXR Feedback', '', HInstance);
  RegisterSceneObject(TgxFBORenderer, 'OpenXR FrameBuffer', '', HInstance);
end;

//=================================================================
finalization
//=================================================================

ObjectManager.Free;

end.
