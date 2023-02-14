//
// Graphic Scene Engine, http://glscene.org
//
{
   SEM shader : Spherical Environment Mapping
   The main idea of SEM is to get the UV coordinates (which are used to lookup the matCap texture)
   from the normal vector on the fragment instead of the original texture coordinates from the object. 
    
   A material using SEM is very useful to highlight variations in the mesh: creases, bumps, even slow ondulations.
   It doesn't work that well on a cube, for instance. And does absolutely nothing on a sphere:
   SEM on a sphere is exactly the same as a planar projection of the matCap texture. 

   At this time only one light source is supported
               
}


unit GLX.GLSLSemShader;

interface

{$I GLX.Scene.inc}

uses
  System.Classes,
  
  GLX.Scene, 
  GLX.CrossPlatform, 
  GLX.BaseClasses, 
  GLX.State, 
  Winapi.OpenGL,
  Winapi.OpenGLext,  
  Imporet.OpenGL1x, 
  GLX.Context, 
  GLX.RenderContextInfo, 
  GLX.VectorGeometry, 
  GLX.Coordinates, 
  GLX.TextureFormat, 
  GLX.Color, 
  GLX.Texture, 
  GLX.Material, 
  GLX.GLSLShader, 
  GLX.CustomShader;

{ Custom class for GLSLSEMShader. 
 SEM Shader : Spherical Environment Mapping }
Type
TgxCustomGLSLSemShader = class(TgxCustomGLSLShader)
  private
    FAmbientColor: TgxColor;
//    FDiffuseColor: TgxColor;
    FSpecularColor: TgxColor;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;

    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FMainTexture: TgxTexture;
    FMainTexName   : TgxLibMaterialName;

//    FSpecularPower: Single;
//    FLightPower: Single;

    function GetMaterialLibrary: TgxAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TgxTexture);
    function GetMainTexName: TgxLibMaterialName;
    procedure SetMainTexName(const Value: TgxLibMaterialName);

    //procedure SetDiffuseColor(AValue: TgxColor);
    procedure SetAmbientColor(AValue: TgxColor);
    procedure SetSpecularColor(AValue: TgxColor);

  protected
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;

    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

//    property DiffuseColor : TgxColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TgxColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TgxColor Read FAmbientColor Write setAmbientColor;

    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;

    property MaterialLibrary: TgxAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TgxTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TgxLibMaterialName read GetMainTexName write SetMainTexName;

//    property SpecularPower: Single read FSpecularPower write FSpecularPower;
//    property LightPower: Single read FLightPower write FLightPower;

  end;

  TgxSLSemShader = class(TgxCustomGLSLSemShader)
  published

    property AmbientColor;
//    property DiffuseColor;
    property SpecularColor;

    property AmbientFactor;
    property DiffuseFactor;
    property SpecularFactor;

    property MaterialLibrary;
    property MainTexture;
    property MainTextureName;


  end;
implementation

constructor TgxCustomGLSLSemShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');

    Add('void main() { ');
    Add('  vec4 p = gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  lightVec = vec3(lightPos - p); ');
    Add('  viewVec = -vec3(p); ');
    Add('  normal = normalize(gl_NormalMatrix * gl_Normal ); ');

    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    clear;
    Add('uniform vec4 AmbientColor; ');
    Add('uniform vec4 SpecularColor; ');
    Add('uniform float DiffuseIntensity; ');
    Add('uniform float AmbientIntensity; ');
    Add('uniform float SpecularIntensity; ');

    Add('uniform sampler2D MainTexture; ');

    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');

    Add('void main() { ');
    Add('  vec3 V = normalize(viewVec); ');
    Add('  vec3 r = reflect( V, normal ); ');
    Add('  float m = 2.0 * sqrt( pow( r.x, 2.0 ) + pow( r.y, 2.0 ) + pow( r.z + 1.0, 2.0 ) ); ');
    Add('  vec2 vN = r.xy / m + 0.5; ');
    Add('  vec4 DiffuseColor;    ');
    Add('  DiffuseColor = texture2D( MainTexture, vN ); //.rgb; ');

    // Simple Lighting
    Add('  vec3 L = normalize(lightVec); ');

    Add('  vec3 halfAngle = normalize(L + V); ');
    Add('  float NdotL = dot(L, normal); ');
    Add('  float NdotH = clamp(dot(halfAngle, normal), 0.0, 1.0); ');
    Add('  // "Half-Lambert" technique for more pleasing diffuse term ');
    Add('  float diffuse = DiffuseColor*(0.5 * NdotL + 0.5); ');
    Add('  float specular = pow(NdotH, 64.0); ');

    Add('  vec4 FinalColour = AmbientColor*AmbientIntensity + ');
    Add('                     DiffuseColor*diffuse*DiffuseIntensity + ');
    Add('                     SpecularColor*specular*SpecularIntensity; ');

    Add('  gl_FragColor = FinalColour; //vec4( FinalColour, 1.0 ); ');
    Add('} ');
  end;

  FAmbientColor := TgxColor.Create(Self);
  //FDiffuseColor := TgxColor.Create(Self);
  FSpecularColor := TgxColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1.0);
  //FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1.0, 1.0, 1.0, 1.0);
  FAmbientFactor  := 0.8;
  FDiffuseFactor  :=0.9;
  FSpecularFactor :=0.8;


end;

destructor TgxCustomGLSLSemShader.Destroy;
begin
  FAmbientColor.Destroy;
 // FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TgxCustomGLSLSemShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  //Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['AmbientIntensity'].AsVector1f := FAmbientFactor;
  param['DiffuseIntensity'].AsVector1f := FDiffuseFactor;
  param['SpecularIntensity'].AsVector1f := FSpecularFactor;

//  Param['SpecPower'].AsVector1f := FSpecularPower;
//  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

function TgxCustomGLSLSemShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;


function TgxCustomGLSLSemShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxCustomGLSLSemShader.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxCustomGLSLSemShader.SetMainTexTexture(const Value: TgxTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TgxCustomGLSLSemShader.GetMainTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TgxCustomGLSLSemShader.SetMainTexName(const Value: TgxLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


//procedure TgxCustomGLSLSemShader.SetDiffuseColor(AValue: TgxColor);
//begin
//  FDiffuseColor.DirectColor := AValue.Color;
//end;

procedure TgxCustomGLSLSemShader.SetAmbientColor(AValue: TgxColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLSemShader.SetSpecularColor(AValue: TgxColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLSemShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin

        if FMainTexture <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;


end.
