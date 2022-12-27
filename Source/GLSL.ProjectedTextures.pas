//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLSL.ProjectedTextures;

(* Implements projected textures via GLSL. 

  Known bugs/limitations

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

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.PipelineTransformation,
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.Texture,
  GLS.VectorGeometry,
  GLS.Context,
  GLS.Color,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLS.VectorTypes;

type
  TGLSLProjectedTexturesStyle = (ptsLight, ptsShadow);

  TGLSLProjectedTextures = class;

  (* A projected texture emmiter.
     Can be places anywhere in the scene.
     Used to generate a modelview and texture matrix for the shader*)
  TGLSLTextureEmitter = class(TGLBaseSceneObject)
  private
    FFOV: single;
    FAspect, FBrightness, FAttenuation: single;
    FStyle: TGLSLProjectedTexturesStyle;
    FColor: TGLColor;
    FUseAttenuation, FAllowReverseProjection: boolean;
    FUseQuadraticAttenuation: boolean;
  protected
    ProjectedTexturesObject: TGLSLProjectedTextures;
    TexMatrix: TGLMatrix;
    procedure SetupTexMatrix;
    procedure SetStyle(val: TGLSLProjectedTexturesStyle);
    procedure SetUseAttenuation(val: boolean);
    procedure SetUseQuadraticAttenuation(val: boolean);
    procedure SetAllowReverseProjection(val: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: boolean); override;
  published
    // Indicates the field-of-view of the projection frustum.
    property FOV: single read FFOV write FFOV;
    (* x/y ratio. For no distortion, this should be set to
       texture.width/texture.height.*)
    property Aspect: single read FAspect write FAspect;
    // Indicates the style of the projected textures.
    property Style: TGLSLProjectedTexturesStyle read FStyle write SetStyle;
    // Fall off/ attenuation of the projected texture
    property Attenuation: single read FAttenuation write FAttenuation;
    property Brightness: single read FBrightness write FBrightness;
    property Color: TGLColor read FColor write FColor;
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

  // Specifies an item on the TGLSLTextureEmitters collection.
  TGLSLTextureEmitterItem = class(TCollectionItem)
  private
    FEmitter: TGLSLTextureEmitter;
  protected
    procedure SetEmitter(const val: TGLSLTextureEmitter);
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Emitter: TGLSLTextureEmitter read FEmitter write SetEmitter;
  end;

  // Collection of TGLSLTextureEmitter.
  TGLSLTextureEmitters = class(TCollection)
  private
    FOwner: TGLSLProjectedTextures;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TGLSLTextureEmitterItem;
    procedure RemoveNotification(aComponent: TComponent);
  public
    procedure AddEmitter(texEmitter: TGLSLTextureEmitter);
    property Items[index: Integer]: TGLSLTextureEmitterItem read GetItems; default;
  end;

  (* Projected Texture Manager.
     Specifies active Emitters and receivers (children of this object).
     At the moment, only 1 texture can be used.*)
  TGLSLProjectedTextures = class(TGLSceneObject)
  private
    ShaderSupported: boolean;
    FEmitters: TGLSLTextureEmitters;
    FUseLightmaps: boolean;
    Shader: TGLProgramHandle;
    FAmbient: TGLColor;
    procedure SetupShader;
  protected
    ShaderChanged: boolean;
    procedure SetUseLightmaps(val: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
  published
    // List of emitters.
    property Emitters: TGLSLTextureEmitters read FEmitters write FEmitters;

    //Ambient is use if no lightmap..
    property Ambient: TGLColor read fAmbient write fAmbient;
    property UseLightmaps: boolean read FUseLightmaps write SetUseLightmaps;
  end;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------

// ------------------
// ------------------ TGLSLTextureEmitter ------------------
// ------------------

constructor TGLSLTextureEmitter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFOV := 90;
  FAspect := 1;
  FStyle := ptsLight;
  FAllowReverseProjection := false;
  FUseAttenuation := false;
  FAttenuation := 100;
  FBrightness := 1;
  FColor := TGLColor.create(self);
  FColor.SetColor(1, 1, 1);
end;

destructor TGLSLTextureEmitter.Destroy;
begin
  FColor.free;
  inherited;
end;

procedure TGLSLTextureEmitter.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: boolean);
begin
  SetupTexMatrix;
  inherited;
end;

procedure TGLSLTextureEmitter.SetupTexMatrix;
const
  cBaseMat: TGLMatrix = (V:((X:0.5; Y:0;   Z:0; W:0),
                          (X:0;   Y:0.5; Z:0; W:0),
                          (X:0;   Y:0;   Z:1; W:0),
                          (X:0.5; Y:0.5; Z:0; W:1)));
begin
  // Set the projector's "perspective" (i.e. the "spotlight cone"):.
  TexMatrix := MatrixMultiply(
    CreatePerspectiveMatrix(FFOV, FAspect, 0.1, 1), cBaseMat);
  TexMatrix := MatrixMultiply(
    CurrentGLContext.PipelineTransformation.InvModelViewMatrix^, TexMatrix);
end;

procedure TGLSLTextureEmitter.SetAllowReverseProjection(val: boolean);
begin
  FAllowReverseProjection := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetUseAttenuation(val: boolean);
begin
  FUseAttenuation := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetUseQuadraticAttenuation(val: boolean);
begin
  FUseQuadraticAttenuation := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TGLSLTextureEmitter.SetStyle(val: TGLSLProjectedTexturesStyle);
begin
  FStyle := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

// ------------------
// ------------------ TGLSLTextureEmitterItem ------------------
// ------------------


constructor TGLSLTextureEmitterItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;


procedure TGLSLTextureEmitterItem.Assign(Source: TPersistent);
begin
  if Source is TGLSLTextureEmitterItem then
  begin
    FEmitter := TGLSLTextureEmitterItem(Source).FEmitter;
    TGLSLProjectedTextures(TGLSLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
  inherited;
end;


procedure TGLSLTextureEmitterItem.SetEmitter(const val: TGLSLTextureEmitter);
begin
  if FEmitter <> val then
  begin
    FEmitter := val;
    TGLSLProjectedTextures(TGLSLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
end;


procedure TGLSLTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FEmitter then
    FEmitter := nil;
end;


function TGLSLTextureEmitterItem.GetDisplayName: string;
begin
  if Assigned(FEmitter) then
  begin
    Result := '[Emitter] ' + FEmitter.Name;
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TGLSLTextureEmitters ------------------
// ------------------


function TGLSLTextureEmitters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


function TGLSLTextureEmitters.GetItems(index: Integer): TGLSLTextureEmitterItem;
begin
  Result := TGLSLTextureEmitterItem(inherited Items[index]);
end;


procedure TGLSLTextureEmitters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].RemoveNotification(aComponent);
    TGLSLProjectedTextures(GetOwner).shaderChanged := true;
  end;
end;


procedure TGLSLTextureEmitters.AddEmitter(texEmitter: TGLSLTextureEmitter);
var
  item: TGLSLTextureEmitterItem;
begin
  item := TGLSLTextureEmitterItem(self.Add);
  item.Emitter := texEmitter;
  item.Emitter.ProjectedTexturesObject := TGLSLProjectedTextures(GetOwner);
  TGLSLProjectedTextures(GetOwner).shaderChanged := true;
end;

// ------------------
// ------------------ TGLSLProjectedTextures ------------------
// ------------------


constructor TGLSLProjectedTextures.Create(AOwner: TComponent);
begin
  inherited Create(aOWner);
  FEmitters := TGLSLTextureEmitters.Create(TGLSLTextureEmitterItem);
  FEmitters.FOwner := self;
  FUseLightmaps := false;
  ShaderChanged := true;
  Ambient := TGLColor.Create(self);
  ambient.SetColor(0.5, 0.5, 0.5, 0.5);
end;


destructor TGLSLProjectedTextures.Destroy;
begin
  if assigned(shader) then
    Shader.free;
  FEmitters.Free;
  Ambient.Free;

  inherited destroy;
end;

procedure TGLSLProjectedTextures.SetUseLightmaps(val: boolean);
begin
  FUseLightmaps := val;
  ShaderChanged := true;
end;

procedure TGLSLProjectedTextures.SetupShader;
const
  AbsFunc: array[boolean] of string = ('', 'abs');
var
  vp, fp: TStringlist;
  i: integer;
  emitter: TGLSLTextureEmitter;
  OldSeparator: char;
begin
  ShaderSupported := (GL.ARB_shader_objects and GL.ARB_vertex_program and
    GL.ARB_vertex_shader and GL.ARB_fragment_shader);

  if not ShaderSupported then
    exit;

  if assigned(shader) then
    FreeAndNil(shader);

  Shader := TGLProgramHandle.CreateAndAllocate;

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

    Shader.AddShader(TGLVertexShaderHandle, vp.Text, True);
    Shader.AddShader(TGLFragmentShaderHandle, fp.Text, True);
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


procedure TGLSLProjectedTextures.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: boolean);
var
  i: integer;
  emitter: TGLSLTextureEmitter;
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

  if ShaderSupported then
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

    rci.GLStates.TextureBinding[2, ttTexture2D] := Material.Texture.Handle;

    self.RenderChildren(0, Count - 1, rci);

    EndUseProgramObject;
  end
  else
    self.RenderChildren(0, Count - 1, rci);
end;

procedure TGLSLProjectedTextures.StructureChanged;
begin
  inherited;
  shaderchanged := true;
end;

initialization

  RegisterClasses([TGLSLTextureEmitter, TGLSLProjectedTextures]);

end.

