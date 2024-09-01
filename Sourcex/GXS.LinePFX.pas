//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.LinePFX;

(* A PFX whose particles are lines *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GXS.VectorTypes,
  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.ParticleFX,
  GXS.Texture,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.Context;

type

   // Linear particle.
   TgxLineParticle = class (TgxParticle)
      private
         FDirection : TAffineVector;
         FLength : Single;
      protected
      public
         procedure WriteToFiler(writer : TgxVirtualWriter); override;
         procedure ReadFromFiler(reader : TgxVirtualReader); override;
         { Direction of the line. }
         property Direction : TAffineVector read FDirection write FDirection;
         { Length of the line }
         property Length : Single read FLength write FLength;
   end;

   { Polygonal particles FX manager.
      The particles of this manager are made of N-face regular polygon with
      a core and edge color. No texturing is available.
      If you render large particles and don't have T&L acceleration, consider
      using TgxPointLightPFXManager. }
   TgxLinePFXManager = class (TgxLifeColoredPFXManager)
      private
         Fvx, Fvy : TAffineVector;        // NOT persistent
         FNvx, FNvy : TAffineVector;        // NOT persistent
         FDefaultLength : Single;
      protected
         function StoreDefaultLength : Boolean;
         function TexturingMode : Cardinal; override;
         procedure InitializeRendering(var rci: TgxRenderContextInfo); override;
         procedure BeginParticles(var rci: TgxRenderContextInfo); override;
         procedure RenderParticle(var rci: TgxRenderContextInfo; aParticle : TgxParticle); override;
         procedure EndParticles(var rci: TgxRenderContextInfo); override;
         procedure FinalizeRendering(var rci: TgxRenderContextInfo); override;
      public
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;
         class function ParticlesClass : TgxParticleClass; override;
         function CreateParticle : TgxParticle; override;
	   published
         property DefaultLength : Single read FDefaultLength write FDefaultLength stored StoreDefaultLength;
         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxLinePFXManager ------------------
// ------------------

constructor TgxLinePFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FDefaultLength:=1;
end;

destructor TgxLinePFXManager.Destroy;
begin
   inherited Destroy;
end;

class function TgxLinePFXManager.ParticlesClass : TgxParticleClass;
begin
   Result:=TgxLineParticle;
end;

function TgxLinePFXManager.CreateParticle : TgxParticle;
begin
   Result:=inherited CreateParticle;
   TgxLineParticle(Result).FLength:=DefaultLength;
end;

function TgxLinePFXManager.TexturingMode : Cardinal;
begin
   Result:=0;
end;

procedure TgxLinePFXManager.InitializeRendering(var rci: TgxRenderContextInfo);
var
   i : Integer;
   matrix : TMatrix4f;
begin
   inherited;
   glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);
   for i:=0 to 2 do begin
      Fvx.V[i]:=matrix.V[i].X;
      Fvy.V[i]:=matrix.V[i].Y;
   end;
   FNvx:=VectorNormalize(Fvx);
   FNvy:=VectorNormalize(Fvy);
end;

procedure TgxLinePFXManager.BeginParticles(var rci: TgxRenderContextInfo);
begin
   ApplyBlendingMode(rci);
end;

procedure TgxLinePFXManager.RenderParticle(var rci: TgxRenderContextInfo; aParticle : TgxParticle);
var
   lifeTime, sizeScale, fx, fy, f : Single;
   inner, outer : TgxColorVector;
   pos, dir, start, stop, dv : TAffineVector;
begin
   lifeTime:=CurrentTime-aParticle.CreationTime;
   ComputeColors(lifeTime, inner, outer);
   if ComputeSizeScale(lifeTime, sizeScale) then
      sizeScale:=sizeScale*ParticleSize
   else sizeScale:=ParticleSize;

   pos:=aParticle.Position;

   with TgxLineParticle(aParticle) do begin
      dir:=VectorNormalize(aParticle.Velocity);
      f:=Length*0.5;
   end;

   start:=VectorCombine(pos, dir, 1, f);
   stop:=VectorCombine(pos, dir, 1, -f);

   fx:=VectorDotProduct(dir, FNvy)*sizeScale;
   fy:=-VectorDotProduct(dir, FNvx)*sizeScale;

   dv:=VectorCombine(Fvx, Fvy, fx, fy);

   glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@inner);
      glVertex3fv(@start);
      glColor4fv(@outer);
      glVertex3f(start.X+dv.X, start.Y+dv.Y, start.Z+dv.Z);
      glVertex3f(stop.X+dv.X, stop.Y+dv.Y, stop.Z+dv.Z);
      glColor4fv(@inner);
      glVertex3fv(@stop);
      glColor4fv(@outer);
      glVertex3f(stop.X-dv.X, stop.Y-dv.Y, stop.Z-dv.Z);
      glVertex3f(start.X-dv.X, start.Y-dv.Y, start.Z-dv.Z);
   glEnd;
end;

procedure TgxLinePFXManager.EndParticles(var rci: TgxRenderContextInfo);
begin
   UnapplyBlendingMode(rci);
end;

procedure TgxLinePFXManager.FinalizeRendering(var rci: TgxRenderContextInfo);
begin
   inherited;
end;

function TgxLinePFXManager.StoreDefaultLength : Boolean;
begin
   Result:=(FDefaultLength<>1);
end;

// ------------------
// ------------------ TgxLineParticle ------------------
// ------------------

procedure TgxLineParticle.WriteToFiler(writer : TgxVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      Write(FDirection, SizeOf(FDirection));
      WriteFloat(FLength);
   end;
end;

procedure TgxLineParticle.ReadFromFiler(reader : TgxVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      Read(FDirection, SizeOf(FDirection));
      FLength:=ReadFloat;
   end else RaiseFilerException(archiveVersion);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

   
   RegisterClasses([TgxLineParticle, TgxLinePFXManager]);

end.
