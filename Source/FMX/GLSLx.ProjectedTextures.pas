//
// The graphics platform GLArena https://github.com/glscene
//
unit GLSLx.ProjectedTextures;

(* Implements projected textures through a GLScene object via GLSL *)

(* Known bugs/limitations:

1. Only 1 texture can be used for all emitters
2. Only up to 6 Emitters can be used (more on better cards)
   A way round this is to make the emiitters a children of the 6 nearest objects
   to the camera.
3. Changing emitter properties causes a slight delay while recreating the shader.
   To make an emitter invisible, just move it to somewhere it won't project on
   anything, or set the brightness to 0. (?)
4. All children of the ProjectedTextures must have use a texture.
   The shader can't be changed between rendering each seperate object..
*)

interface

{$I Scena.inc}

uses
  System.Classes,
  System.SysUtils,

  GLX.PersistentClasses,
  GLX.Scene,
  GLX.Texture,
  Scena.VectorGeometry,
  GLX.Context,
  GLX.Color,
  GLX.RenderContextInfo,
  Scena.TextureFormat,
  Scena.PipelineTransformation,
  Scena.VectorTypes;

type
  TgxSLProjectedTexturesStyle = (ptsLight, ptsShadow);

  TgxSLProjectedTextures = class;

  (* A projected texture emitter.
     Can be places anywhere in the scene.
     Used to generate a modelview and texture matrix for the shader *)
  TgxSLTextureEmitter = class(TgxBaseSceneObject)
  private
    FFOV: single;
    FAspect, FBrightness, FAttenuation: single;
    FStyle: TgxSLProjectedTexturesStyle;
    FColor: TgxColor;
    FUseAttenuation, FAllowReverseProjection: boolean;
    FUseQuadraticAttenuation: boolean;
  protected
    ProjectedTexturesObject: TgxSLProjectedTextures;
    TexMatrix: TMatrix4f;
    procedure SetupTexMatrix;
    procedure SetStyle(val: TgxSLProjectedTexturesStyle);
    procedure SetUseAttenuation(val: boolean);
    procedure SetUseQuadraticAttenuation(val: boolean);
    procedure SetAllowReverseProjection(val: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: boolean); override;
  published
    { Indicates the field-of-view of the projection frustum.}
    property FOV: single read FFOV write FFOV;
    { x/y ratio. For no distortion, this should be set to
       texture.width/texture.height.}
    property Aspect: single read FAspect write FAspect;
    { Indicates the style of the projected textures.}
    property Style: TgxSLProjectedTexturesStyle read FStyle write SetStyle;
    {Fall off/ attenuation of the projected texture}
    property Attenuation: single read FAttenuation write FAttenuation;
    property Brightness: single read FBrightness write FBrightness;
    property Color: TgxColor read FColor write FColor;
    property UseAttenuation: boolean read FUseAttenuation write SetUseAttenuation;
    property UseQuadraticAttenuation: Boolean read FUseQuadraticAttenuation write SetUseQuadraticAttenuation;
    property AllowReverseProjection: boolean read FAllowReverseProjection write SetAllowReverseProjection;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

  // Specifies an item on the TgxSLTextureEmitters collection.
  TgxSLTextureEmitterItem = class(TCollectionItem)
  private
    FEmitter: TgxSLTextureEmitter;
  protected
    procedure SetEmitter(const val: TgxSLTextureEmitter);
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Emitter: TgxSLTextureEmitter read FEmitter write SetEmitter;
  end;

  // Collection of TgxSLTextureEmitter.
  TgxSLTextureEmitters = class(TCollection)
  private
    FOwner: TgxSLProjectedTextures;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TgxSLTextureEmitterItem;
    procedure RemoveNotification(aComponent: TComponent);
  public
    procedure AddEmitter(texEmitter: TgxSLTextureEmitter);
    property Items[index: Integer]: TgxSLTextureEmitterItem read GetItems; default;
  end;

  (* Projected Texture Manager.
     Specifies active Emitters and receivers (children of this object).
     At the moment, only 1 texture can be used. *)
  TgxSLProjectedTextures = class(TgxSceneObject)
  private
    FEmitters: TgxSLTextureEmitters;
    FUseLightmaps: boolean;
    Shader: TgxProgramHandle;
    FAmbient: TgxColor;
    procedure SetupShader;
  protected
    ShaderChanged: boolean;
    procedure SetUseLightmaps(val: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TgxRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
  published
    { List of emitters. }
    property Emitters: TgxSLTextureEmitters read FEmitters write FEmitters;

    //Ambient is use if no lightmap..
    property Ambient: TgxColor read fAmbient write fAmbient;
    property UseLightmaps: boolean read FUseLightmaps write SetUseLightmaps;
  end;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------

// ------------------
// ------------------ TgxSLTextureEmitter ------------------
// ------------------

constructor TgxSLTextureEmitter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFOV := 90;
  FAspect := 1;
  FStyle := ptsLight;
  FAllowReverseProjection := false;
  FUseAttenuation := false;
  FAttenuation := 100;
  FBrightness := 1;
  FColor := TgxColor.create(self);
  FColor.SetColor(1, 1, 1);
end;

destructor TgxSLTextureEmitter.Destroy;
begin
  FColor.Free;
  inherited;
end;

procedure TgxSLTextureEmitter.DoRender(var rci: TgxRenderContextInfo;
  renderSelf, renderChildren: boolean);
begin
  SetupTexMatrix;
  inherited;
end;

procedure TgxSLTextureEmitter.SetupTexMatrix;
const
  cBaseMat: TMatrix4f = (V:((X:0.5; Y:0;   Z:0; W:0),
                          (X:0;   Y:0.5; Z:0; W:0),
                          (X:0;   Y:0;   Z:1; W:0),
                          (X:0.5; Y:0.5; Z:0; W:1)));
begin
  // Set the projector's "perspective" (i.e. the "spotlight cone"):.
  TexMatrix := MatrixMultiply(
    CreatePerspectiveMatrix(FFOV, FAspect, 0.1, 1), cBaseMat);
  TexMatrix := MatrixMultiply(
    CurrentContext.PipelineTransformation.InvModelViewMatrix^, TexMatrix);
end;

procedure TgxSLTextureEmitter.SetAllowReverseProjection(val: boolean);
begin
  FAllowReverseProjection := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TgxSLTextureEmitter.SetUseAttenuation(val: boolean);
begin
  FUseAttenuation := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TgxSLTextureEmitter.SetUseQuadraticAttenuation(val: boolean);
begin
  FUseQuadraticAttenuation := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TgxSLTextureEmitter.SetStyle(val: TgxSLProjectedTexturesStyle);
begin
  FStyle := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

// ------------------
// ------------------ TgxSLTextureEmitterItem ------------------
// ------------------

constructor TgxSLTextureEmitterItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

procedure TgxSLTextureEmitterItem.Assign(Source: TPersistent);
begin
  if Source is TgxSLTextureEmitterItem then
  begin
    FEmitter := TgxSLTextureEmitterItem(Source).FEmitter;
    TgxSLProjectedTextures(TgxSLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
  inherited;
end;

procedure TgxSLTextureEmitterItem.SetEmitter(const val: TgxSLTextureEmitter);
begin
  if FEmitter <> val then
  begin
    FEmitter := val;
    TgxSLProjectedTextures(TgxSLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
end;

procedure TgxSLTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FEmitter then
    FEmitter := nil;
end;

function TgxSLTextureEmitterItem.GetDisplayName: string;
begin
  if Assigned(FEmitter) then
  begin
    Result := '[Emitter] ' + FEmitter.Name;
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TgxSLTextureEmitters ------------------
// ------------------

function TgxSLTextureEmitters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TgxSLTextureEmitters.GetItems(index: Integer): TgxSLTextureEmitterItem;
begin
  Result := TgxSLTextureEmitterItem(inherited Items[index]);
end;

procedure TgxSLTextureEmitters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].RemoveNotification(aComponent);
    TgxSLProjectedTextures(GetOwner).shaderChanged := true;
  end;
end;

procedure TgxSLTextureEmitters.AddEmitter(texEmitter: TgxSLTextureEmitter);
var
  item: TgxSLTextureEmitterItem;
begin
  item := TgxSLTextureEmitterItem(self.Add);
  item.Emitter := texEmitter;
  item.Emitter.ProjectedTexturesObject := TgxSLProjectedTextures(GetOwner);
  TgxSLProjectedTextures(GetOwner).shaderChanged := true;
end;

// ------------------
// ------------------ TgxSLProjectedTextures ------------------
// ------------------

constructor TgxSLProjectedTextures.Create(AOwner: TComponent);
begin
  inherited Create(aOWner);
  FEmitters := TgxSLTextureEmitters.Create(TgxSLTextureEmitterItem);
  FEmitters.FOwner := self;
  FUseLightmaps := false;
  ShaderChanged := true;
  Ambient := TgxColor.Create(self);
  ambient.SetColor(0.5, 0.5, 0.5, 0.5);
end;

destructor TgxSLProjectedTextures.Destroy;
begin
  if assigned(shader) then
    Shader.free;
  FEmitters.Free;
  Ambient.Free;
  inherited destroy;
end;

procedure TgxSLProjectedTextures.SetUseLightmaps(val: boolean);
begin
  FUseLightmaps := val;
  ShaderChanged := true;
end;

procedure TgxSLProjectedTextures.SetupShader;
const
  AbsFunc: array[boolean] of string = ('', 'abs');
var
  vp, fp: TStringlist;
  i: integer;
  emitter: TgxSLTextureEmitter;
  OldSeparator: char;
begin
  if assigned(shader) then
    FreeAndNil(shader);

  Shader := TgxProgramHandle.CreateAndAllocate;

  OldSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  vp := TStringlist.create;
  fp := TStringlist.create;

  try
    //define the vertex program
    if emitters.count > 0 then
    begin
      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        if not emitter.Visible then
          continue;
        vp.add(format('uniform mat4 TextureMatrix%d;', [i]));
        vp.add(format('varying vec4 ProjTexCoords%d;', [i]));
      end;
    end;

    vp.add('void main(){');
    vp.add('vec4 P = gl_Vertex;');
    vp.add('gl_Position = gl_ModelViewProjectionMatrix * P;');
    vp.add('vec4 Pe = gl_ModelViewMatrix * P;');

    vp.add('gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;');

    if UseLightmaps then
      vp.add('gl_TexCoord[1] = gl_TextureMatrix[1] * gl_MultiTexCoord1;');
    if emitters.count > 0 then
    begin
      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        vp.add(format('ProjTexCoords%d = TextureMatrix%d * Pe;', [i, i]));
      end;
    end;
    vp.add('}');

    //define the fragment program
    fp.add('uniform sampler2D TextureMap;');
    if UseLightmaps then
      fp.add('uniform sampler2D LightMap;');
    if emitters.count > 0 then
    begin
      fp.add('uniform sampler2D ProjMap;');

      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        fp.add(format('varying vec4 ProjTexCoords%d;', [i]));
        if Emitter.UseAttenuation then
          fp.add(format('uniform float Attenuation%d;', [i]));
        fp.add(format('uniform float Brightness%d;', [i]));
        fp.add(format('uniform vec3 Color%d;', [i]));
      end;
    end;

    fp.add('void main(){');
    fp.add('vec4 color = texture2D(TextureMap, gl_TexCoord[0].st).rgba;');
    if UseLightmaps then
      fp.add('vec3 light = texture2D(LightMap, gl_TexCoord[1].st).rgb;')
    else
      fp.add(format('vec3 light = vec3(%.4, %.4, %.4);', [Ambient.Red, ambient.Green, ambient.Blue]));
    if emitters.count > 0 then
    begin
      fp.add('vec3 projlight = vec3(0.0);');
      fp.add('vec3 projshadow = vec3(0.0);');
      fp.add('vec3 temp;');
      fp.add('float dist;');
      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        if not emitter.visible then
          continue;
        if not emitter.AllowReverseProjection then
          fp.add(format('if (ProjTexCoords%d.q<0.0){', [i]));
        case emitter.Style of
          ptslight:
            fp.add(format('projlight+= (texture2DProj(ProjMap, ProjTexCoords%d).rgb*Color%d*Brightness%d);', [i, i, i]));
          ptsShadow:
            fp.add(format('projshadow+= (texture2DProj(ProjMap, ProjTexCoords%d).rgb*Color%d*Brightness%d);', [i, i, i]));
        end;

        if emitter.UseAttenuation then
        begin
          // for attenuation we need the distance to the point
          // so use absolute value when AllowReverseProjection is enabled
          fp.add(format('dist = 1.0 - clamp(%s(ProjTexCoords%d.q/Attenuation%d), 0.0, 1.0);',
            [AbsFunc[emitter.AllowReverseProjection], i, i]));
          if emitter.UseQuadraticAttenuation then
            fp.add('dist *= dist;');
          case emitter.Style of
            ptslight:
              fp.add('projlight *= dist;');
            ptsShadow:
              fp.add('projshadow *= dist;');
          end;

        end;
        if not emitter.AllowReverseProjection then
          fp[fp.Count - 1] := fp[fp.Count - 1] + '}';
      end;

      fp.add('projlight = clamp(projlight,0.0,1.2);');
      fp.add('projshadow = clamp(projshadow,0.0,0.8);');

      fp.add('vec3 totlight = 1.0-((( 1.0-projlight)*( 1.0-light)) +(projshadow*light)) ;');
    end
    else
      fp.add('vec3 totlight = light;');

    fp.add('gl_FragColor = vec4(1.5*totlight * color.rgb, color.a);}');

    Shader.AddShader(TgxVertexShaderHandle, vp.Text, True);
    Shader.AddShader(TgxFragmentShaderHandle, fp.Text, True);
  finally
    FormatSettings.DecimalSeparator := OldSeparator;
    vp.free;
    fp.free;
  end;

  if not Shader.LinkProgram then
    raise Exception.Create(Shader.InfoLog);
  if not Shader.ValidateProgram then
    raise Exception.Create(Shader.InfoLog);
end;

procedure TgxSLProjectedTextures.DoRender(var rci: TgxRenderContextInfo;
  renderSelf, renderChildren: boolean);
var
  i: integer;
  emitter: TgxSLTextureEmitter;
begin
  if not (renderSelf or renderChildren) then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  if ShaderChanged then
  begin
    SetupShader;
    ShaderChanged := false;
  end;

  with Shader do
  begin
    UseProgramObject;

    for i := 0 to Emitters.Count - 1 do
    begin
      emitter := Emitters[i].Emitter;
      if not assigned(emitter) then
        continue;
      if emitter.UseAttenuation then
        // negate attenuation here, instead of negating q inside the shader
        // otherwise the result of q/attenuation is negative.
        Uniform1f['Attenuation' + inttostr(i)] := -emitter.Attenuation;

      Uniform1f['Brightness' + inttostr(i)] := emitter.Brightness;
      Uniform3f['Color' + inttostr(i)] := PAffinevector(@emitter.Color.Color)^;
      Uniformmatrix4fv['TextureMatrix' + inttostr(i)] := emitter.texMatrix;
    end;

    Uniform1i['TextureMap'] := 0;

    if UseLightmaps then
      Uniform1i['LightMap'] := 1;

    if emitters.count > 0 then
      Shader.Uniform1i['ProjMap'] := 2;

    rci.gxStates.TextureBinding[2, ttTexture2D] := Material.Texture.Handle;

    self.RenderChildren(0, Count - 1, rci);

    EndUseProgramObject;
  end;
end;

procedure TgxSLProjectedTextures.StructureChanged;
begin
  inherited;
  shaderchanged := true;
end;

//===========================================================
initialization
//===========================================================

  RegisterClasses([TgxSLTextureEmitter, TgxSLProjectedTextures]);

end.

