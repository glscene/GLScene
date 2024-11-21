unit uFountainD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.PersistentClasses,
  GLS.Scene,
  GLS.Objects,
  Stage.VectorGeometry,
  GLS.Texture,
  Stage.VectorTypes,
  GLS.RenderContextInfo;

const
  F_GRAVITY = 9.81;

  // ---------------------------------------------------------------------
  // TFctCondition
  // ---------------------------------------------------------------------
type
  TFctCondition = function(const ptCondition: Pointer): Boolean;
  TpNode = ^TNode;

  TNode = record
    Info: Pointer;
    Next: TpNode;
  end;

const
  SIZE_NODE = SizeOf(TNode);

  // ---------------------------------------------------------------------
  // TListSPTR
  // ---------------------------------------------------------------------
type
  TListSPTR = class
  private
    Head: TpNode;
    Final: TpNode;
    Current: TpNode;
    Count: Cardinal;
    SizeInfo: Cardinal;
  public
    constructor Create(_SizeInfo: Cardinal);
    destructor Destroy; override;
    function Add(New: Pointer): Boolean;
    function CurrentModify(Modification: Pointer): Boolean;
    function DeleteIf(FctCondition: TFctCondition): integer;
    function DeleteCurrent: Boolean;
    procedure Clear;
    function GetNbCount: Cardinal;
    function GetCurrent(Information: Pointer): Boolean;
    function GetFirst(Information: Pointer): Boolean;
    function GetLast(Information: Pointer): Boolean;
    function GetNext(Information: Pointer): Boolean;
  end;

  // ---------------------------------------------------------------------
  // TParticle
  // ---------------------------------------------------------------------
type
  pParticle = ^TParticle;

  TParticle = record
    Pos: TAffineVector;
    Accel: TAffineVector;
    Velocity: single;
    Times: double;
    Life: single;
    AngleStart: single;
    Bounding: integer;
    Width: single;
    Color: TAffineVector;
    ColorDiff: TAffineVector;
  end;

const
  SIZE_STR_PARTICLE = SizeOf(TParticle);

  // ---------------------------------------------------------------------
  // TGLFountainDummy
  // ---------------------------------------------------------------------
type
  TGLFountainDummy = class(TGLImmaterialSceneObject)
  protected
    FActived: Boolean;
    FNbParticles: integer;
    FMaxParticles: integer;
    FVelocityMax: integer;
    FVelocityMin: integer;
    FAngleStart: integer;
    FFloor: single;
    FFountainSize: single;
    FParticlesSizeMax: integer;
    FParticlesSizeMin: integer;
    FBoundingFactor: single;
    FParticleMass: single;
    FTimesFactor: double;
    FLifeFactor: single;
    FBounding: Boolean;
    FColorStart: longint;
    FColorEnd: longint;
    FNewTime: double;
    FDeltaTime: double;
    function GetActived: Boolean;
    procedure SetActived(const Activ: Boolean);
    function GetNbParticles: integer;
    function GetMaxParticles: integer;
    procedure SetMaxParticles(const Max: integer);
    function GetVelocityMax: integer;
    procedure SetVelocityMax(const VeloMax: integer);
    function GetVelocityMin: integer;
    procedure SetVelocityMin(const VeloMin: integer);
    function GetAngleStart: integer;
    procedure SetAngleStart(const AngleS: integer);
    function GetFloor: single;
    procedure SetFloor(const TheFloor: single);
    function GetFountainSize: single;
    procedure SetFountainSize(const FountainSize: single);
    function GetParticlesSizeMax: integer;
    procedure SetParticlesSizeMax(const PartMax: integer);
    function GetParticlesSizeMin: integer;
    procedure SetParticlesSizeMin(const PartMin: integer);
    function GetBoundingFact: single;
    procedure SetBoundingFact(const BoundSize: single);
    function GetParticlesMass: single;
    procedure SetParticlesMass(const Mass: single);
    function GetTimesFactor: double;
    procedure SetTimesFactor(const TimesFact: double);
    function GetLifeFactor: single;
    procedure SetLifeFactor(const LifeFact: single);
    function GetBounding: Boolean;
    procedure SetBounding(const Bound: Boolean);
    function GetColorStart: longint;
    procedure SetColorStart(const ColStart: longint);
    function GetColorEnd: longint;
    procedure SetColorEnd(const ColEnd: longint);
  private
    LsParticles: TListSPTR;
    TabCos, TabSin: array [0 .. 360] of double;
    RD, GD, BD, RF, GF, BF: Byte;
    procedure initFountain;
    function AddParticle: Boolean;
    procedure DeleteParticle;
    procedure CalculBoundPosParticles;
    procedure CalculPosParticles;
    procedure DrawParticles(rci: TGLRenderContextInfo);
    procedure Animation(rci: TGLRenderContextInfo);
    procedure UpdateFountain;
  public
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean);
      override;
    // procedure DoProgress( const progressTime : TGLProgressTimes ); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Actived: Boolean read FActived write SetActived;
    property NbParticles: integer read GetNbParticles;
    property MaxParticles: integer read GetMaxParticles write SetMaxParticles;
    property VelocityMax: integer read GetVelocityMax write SetVelocityMax;
    property VelocityMin: integer read GetVelocityMin write SetVelocityMin;
    property AngleInit: integer read GetAngleStart write SetAngleStart;
    property Floor: single read GetFloor write SetFloor;
    property ParticlesSizeMax: integer read GetParticlesSizeMax write SetParticlesSizeMax;
    property ParticlesSizeMin: integer read GetParticlesSizeMin write SetParticlesSizeMin;
    property BoundingFactor: single read GetBoundingFact write SetBoundingFact;
    property ParticleMass: single read GetParticlesMass write SetParticlesMass;
    property TimesFactor: double read GetTimesFactor write SetTimesFactor;
    property LifeFactor: single read GetLifeFactor write SetLifeFactor;
    property Bounding: Boolean read GetBounding write SetBounding;
    property ColorStart: longint read GetColorStart write SetColorStart;
    property ColorEnd: longint read GetColorEnd write SetColorEnd;
  end;

// ================================================================
implementation
// ================================================================

// ---------------------------------------------------------------
// TListSPTR
// ---------------------------------------------------------------
constructor TListSPTR.Create(_SizeInfo: Cardinal);
begin
  inherited Create;
  Clear;
  SizeInfo := _SizeInfo;
end;

destructor TListSPTR.Destroy;
begin
  inherited Destroy;
  Clear;
end;

function TListSPTR.Add(New: Pointer): Boolean;
var
  p: TpNode;
begin
  GetMem(p, SIZE_NODE);
  FillChar(p^, SIZE_NODE, 0);
  Result := p <> Nil;
  if (Result) then
  begin
    GetMem(p^.Info, SizeInfo);
    FillChar(p^.Info^, SizeInfo, 0);
    Result := (p^.Info <> nil);
    if (Result) then
    begin
      p^.Next := Head;
      Head := p;
      Current := p;
      Move(New^, p^.Info^, SizeInfo);
      Inc(Count);
    end;
  end
end;

function TListSPTR.CurrentModify(Modification: Pointer): Boolean;
begin
  Result := (Current <> nil) and (Modification <> nil);
  if Result then
    Move(Modification^, Current^.Info^, SizeInfo);
end;

function TListSPTR.DeleteCurrent: Boolean;
var
  p: TpNode;
  pContinue: TpNode;
Begin
  Result := (Current <> nil) and (Count > 0);
  if Result then
  begin
    p := Current;
    if (p = Head) then
    begin
      Head := p^.Next;
      Current := Current^.Next;
      FreeMem(p^.Info, SizeInfo);
      FreeMem(p, SIZE_NODE);
      Dec(Count);
    end
    else
    begin
      pContinue := Head;
      while (pContinue <> nil) and (pContinue^.Next <> p) do
        pContinue := pContinue^.Next;
      if (pContinue <> nil) then
      begin
        pContinue^.Next := p^.Next;
        Current := Current^.Next;
        FreeMem(p^.Info, SizeInfo);
        FreeMem(p, SIZE_NODE);
        Dec(Count);
      end;
    end;
  end;
end;

function TListSPTR.DeleteIf(FctCondition: TFctCondition): integer;
var
  p, GCurrent: TpNode;
begin
  Result := 0;
  GCurrent := Current;
  p := Head;
  while (p <> nil) do
  begin
    if FctCondition(p^.Info) then
    begin
      Current := p;
      DeleteCurrent;
      p := Current;
      Inc(Result);
    end
    else
      p := p^.Next;
  end;
  Current := GCurrent;
end;

procedure TListSPTR.Clear;
var
  pAClean: TpNode;
begin
  if (Head <> nil) then
  begin
    while (Head <> nil) do
    begin
      pAClean := Head;
      Head := pAClean^.Next;
      FreeMem(pAClean^.Info, SizeInfo);
      FreeMem(pAClean, SIZE_NODE);
    end;
  end;
  Head := nil;
  Final := nil;
  Current := nil;
  Count := 0;
end;

function TListSPTR.GetNbCount: Cardinal;
begin
  Result := Count;
end;

function TListSPTR.GetCurrent(Information: Pointer): Boolean;
Begin
  Result := (Head <> nil) and (Information <> nil) and (Current <> nil);
  if Result then
    Move(Current^.Info^, Information^, SizeInfo);
end;

function TListSPTR.GetFirst(Information: Pointer): Boolean;
begin
  Result := (Head <> nil) and (Information <> nil);
  if Result then
  begin
    Move(Head^.Info^, Information^, SizeInfo);
    Current := Head;
  end;
end;

function TListSPTR.GetLast(Information: Pointer): Boolean;
begin
  Result := (Final <> nil) and (Information <> nil);
  if Result then
  begin
    Move(Final^.Info^, Information^, SizeInfo);
    Current := Final;
  end;
end;

function TListSPTR.GetNext(Information: Pointer): Boolean;
begin
  Result := (Count > 0) and (Current^.Next <> nil) and (Information <> nil);
  if Result then
  begin
    Move(Current^.Next^.Info^, Information^, SizeInfo);
    Current := Current^.Next;
  end;
end;

// -------------------------------------------------
// TGLFountainDummy
// -------------------------------------------------
constructor TGLFountainDummy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNewTime := 0.0;
  FDeltaTime := 0.0;
  FActived := True;
  FNbParticles := 0;
  FMaxParticles := 500;
  FVelocityMin := 14;
  FVelocityMax := 15;
  FAngleStart := 360;
  FFloor := 0.0;
  FFountainSize := 0.2;
  FParticlesSizeMin := 20;
  FParticlesSizeMax := 40;
  FBoundingFactor := 55;
  FParticleMass := 5.0;
  FTimesFactor := 0.005;
  FLifeFactor := 0.005;
  FBounding := False;
  SetColorStart($FF0000);
  SetColorEnd($FF0000);
  initFountain;
end;

procedure TGLFountainDummy.initFountain;
var
  i: integer;
begin
  for i := 0 to 360 do
  begin
    TabCos[i] := Cos(i);
    TabSin[i] := Sin(i);
  end;
  Randomize;
  LsParticles := TListSPTR.Create(SIZE_STR_PARTICLE);
end;

procedure TGLFountainDummy.UpdateFountain;
begin
  FNbParticles := 0;
  if assigned(LsParticles) then
    LsParticles.Free;
  initFountain;
  NotifyChange(self);
end;

function TGLFountainDummy.AddParticle: Boolean;
var
  PTime: TParticle;
begin
  Result := (FActived) and (NbParticles < FMaxParticles);
  if Result then
  begin
    with PTime do
    begin
      Pos.X := 0.0;
      Pos.Y := FFloor;
      Pos.Z := 0.0;
      AngleStart := Random(FAngleStart);
      Velocity := (Random(FVelocityMax - FVelocityMin) + FVelocityMin) * 0.1;
      Accel.X := TabCos[Round(AngleStart)] * Velocity * FFountainSize;
      Accel.Y := 0.0;
      Accel.Z := TabSin[Round(AngleStart)] * Velocity * FFountainSize;
      Times := 0.0;
      Life := 1.0;
      if FBounding then
        Bounding := 0
      else
        Bounding := 1;
      Width := (Random(FParticlesSizeMax - FParticlesSizeMin) + FParticlesSizeMin) * 0.1;
      Color := AffineVectorMake(RD Div 255, GD Div 255, BD Div 255);
      ColorDiff := AffineVectorMake((RF - RD) / (1 / FLifeFactor) / 255,
        (GF - GD) / (1 / FLifeFactor) / 255, (BF - BD) / (1 / FLifeFactor) / 255);
    end;
    Result := LsParticles.Add(@PTime);
    if Result then
      Inc(FNbParticles);
  end;
end;

procedure TGLFountainDummy.DeleteParticle;
  function LifeCheckParticle(const Particle: Pointer): Boolean;
  begin
    Result := (TParticle(Particle^).Bounding > 0) and (TParticle(Particle^).Life <= 0)
  end;

begin
  if (FActived) then
    FNbParticles := FNbParticles - LsParticles.DeleteIf(@LifeCheckParticle);
end;

procedure TGLFountainDummy.CalculBoundPosParticles;
var
  RoadParticle: TParticle;
  BoundFactor: single;
begin
  if (FActived) then
  begin
    if LsParticles.GetFirst(@RoadParticle) then
      repeat
        with RoadParticle do
        begin
          if (Pos.Y < FFloor) then
          begin
            if (Life > 0) then
            begin
              Times := 0.0;
              BoundFactor := (Velocity * FBoundingFactor * 0.01);
              Velocity := Velocity - BoundFactor;
              Pos.X := Pos.X + Accel.X - BoundFactor;
              Pos.Z := Pos.Z + Accel.Z - BoundFactor;
              Pos.Y := FFloor;
              Inc(Bounding);
            end
          end
          else
          begin
            if Bounding > 0 then
              Life := Life - FLifeFactor;
            Pos.X := Pos.X + Accel.X;
            Pos.Y := (Pos.Y + Times + Velocity) - (F_GRAVITY + FParticleMass) * Sqr(Times);
            Pos.Z := Pos.Z + Accel.Z;
          end;
          Color := VectorAdd(Color, ColorDiff);
          Times := Times + FTimesFactor;
        end;
        LsParticles.CurrentModify(@RoadParticle);
      until not LsParticles.GetNext(@RoadParticle);
  end;
end;

procedure TGLFountainDummy.CalculPosParticles;
var
  RoadParticle: TParticle;
begin
  if (FActived) then
  begin
    if LsParticles.GetFirst(@RoadParticle) then
      repeat
        with RoadParticle do
        begin
          if (Pos.Y >= FFloor) then
          begin
            Life := Life - FLifeFactor;
            Pos.X := Pos.X + Accel.X;
            Pos.Y := (Pos.Y + Times + Velocity) - (F_GRAVITY + FParticleMass) * Sqr(Times);
            Pos.Z := Pos.Z + Accel.Z;
          end
          else
            Life := Life - FLifeFactor;
          Color := VectorAdd(Color, ColorDiff);
          Times := Times + FTimesFactor;
        end;
        LsParticles.CurrentModify(@RoadParticle);
      Until Not LsParticles.GetNext(@RoadParticle);
  end;
end;

procedure TGLFountainDummy.DrawParticles(rci: TGLRenderContextInfo);
var
  RoadParticle: TParticle;
  GMatrix: array [0 .. 15] of GlFloat;
  VRight, VUp: TVector3f;
begin
  if LsParticles.GetFirst(@RoadParticle) then
    repeat
      with RoadParticle do
      begin
        glGetFloatv(GL_MODELVIEW_MATRIX, @GMatrix);
        VRight := AffineVectorMake(GMatrix[00], GMatrix[04], GMatrix[08]);
        VUp := AffineVectorMake(GMatrix[01], GMatrix[05], GMatrix[09]);
        NormalizeVector(VRight);
        NormalizeVector(VUp);
        ScaleVector(VRight, Width / 2);
        ScaleVector(VUp, Width / 2);
        glColor4f(Color.X, Color.Y, Color.Z, Life);
        glbegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex3d(Pos.X - (VRight.X + VUp.X), Pos.Y - (VRight.Y + VUp.Y),
          Pos.Z - (VRight.Z + VUp.Z));
        glTexCoord2f(1, 0);
        glVertex3d(Pos.X + (VRight.X - VUp.X), Pos.Y + (VRight.Y - VUp.Y),
          Pos.Z + (VRight.Z - VUp.Z));
        glTexCoord2f(1, 1);
        glVertex3d(Pos.X + (VRight.X + VUp.X), Pos.Y + (VRight.Y + VUp.Y),
          Pos.Z + (VRight.Z + VUp.Z));
        glTexCoord2f(0, 1);
        glVertex3d(Pos.X - (VRight.X - VUp.X), Pos.Y - (VRight.Y - VUp.Y),
          Pos.Z - (VRight.Z - VUp.Z));
        glend();
      end;
      LsParticles.CurrentModify(@RoadParticle);
    Until Not LsParticles.GetNext(@RoadParticle);
end;

procedure TGLFountainDummy.Animation(rci: TGLRenderContextInfo);
begin
  AddParticle;
  DeleteParticle;
  if FBounding then
    CalculBoundPosParticles
  else
    CalculPosParticles;
  glPushMatrix;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
  glDepthMask(0); // false
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  DrawParticles(rci);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  glDepthMask(1); // true
  glEnable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glPopMatrix;
end;

function TGLFountainDummy.GetActived: Boolean;
begin
  Result := FActived;
end;

procedure TGLFountainDummy.SetActived(const Activ: Boolean);
begin
  FActived := Activ;
  UpdateFountain;
end;

function TGLFountainDummy.GetNbParticles: integer;
begin
  Result := FNbParticles;
end;

function TGLFountainDummy.GetMaxParticles: integer;
begin
  Result := FMaxParticles;
end;

procedure TGLFountainDummy.SetMaxParticles(const Max: integer);
begin
  FMaxParticles := Max;
  UpdateFountain;
end;

function TGLFountainDummy.GetVelocityMax: integer;
begin
  Result := FVelocityMax;
end;

procedure TGLFountainDummy.SetVelocityMax(const VeloMax: integer);
begin
  if (VeloMax > FVelocityMin) then
    FVelocityMax := VeloMax
  else
    FVelocityMax := FVelocityMin + 1;
  UpdateFountain;
end;

function TGLFountainDummy.GetVelocityMin: integer;
begin
  Result := FVelocityMin;
end;

procedure TGLFountainDummy.SetVelocityMin(const VeloMin: integer);
begin
  if (VeloMin < FVelocityMax) then
    FVelocityMin := VeloMin
  else
    FVelocityMin := FVelocityMax - 1;
  UpdateFountain;
end;

function TGLFountainDummy.GetAngleStart: integer;
begin
  Result := FVelocityMin;
end;

procedure TGLFountainDummy.SetAngleStart(const AngleS: integer);
begin
  if (AngleS >= 0) and (AngleS <= 360) then
    FAngleStart := AngleS
  else
    FAngleStart := 360;
  UpdateFountain;
end;

function TGLFountainDummy.GetFloor: single;
begin
  Result := FFloor;
end;

procedure TGLFountainDummy.SetFloor(const TheFloor: single);
begin
  FFloor := TheFloor;
  UpdateFountain;
end;

function TGLFountainDummy.GetFountainSize: single;
begin
  Result := FFountainSize;
end;

procedure TGLFountainDummy.SetFountainSize(const FountainSize: single);
begin
  FFountainSize := FountainSize;
  UpdateFountain;
end;

function TGLFountainDummy.GetParticlesSizeMax: integer;
begin
  Result := FParticlesSizeMax;
end;

procedure TGLFountainDummy.SetParticlesSizeMax(const PartMax: integer);
begin
  if (PartMax > FParticlesSizeMin) then
    FParticlesSizeMax := PartMax
  else
    FParticlesSizeMax := FParticlesSizeMin + 1;
  UpdateFountain;
end;

function TGLFountainDummy.GetParticlesSizeMin: integer;
begin
  Result := FParticlesSizeMin;
end;

procedure TGLFountainDummy.SetParticlesSizeMin(const PartMin: integer);
begin
  if (PartMin < FParticlesSizeMax) then
    FParticlesSizeMin := PartMin
  else
    FParticlesSizeMin := FParticlesSizeMax - 1;
  UpdateFountain;
end;

function TGLFountainDummy.GetBoundingFact: single;
begin
  Result := FBoundingFactor;
end;

procedure TGLFountainDummy.SetBoundingFact(const BoundSize: single);
begin
  if (BoundSize >= 0) and (BoundSize <= 100) then
    FBoundingFactor := BoundSize
  else
    FBoundingFactor := 100;
  UpdateFountain;
end;

function TGLFountainDummy.GetParticlesMass: single;
begin
  Result := FParticleMass;
end;

procedure TGLFountainDummy.SetParticlesMass(const Mass: single);
begin
  FParticleMass := Mass;
  UpdateFountain;
end;

function TGLFountainDummy.GetTimesFactor: double;
begin
  Result := FTimesFactor;
end;

procedure TGLFountainDummy.SetTimesFactor(const TimesFact: double);
begin
  FTimesFactor := TimesFact;
  UpdateFountain;
end;

function TGLFountainDummy.GetLifeFactor: single;
begin
  Result := FLifeFactor;
end;

procedure TGLFountainDummy.SetLifeFactor(const LifeFact: single);
begin
  if LifeFact > 0 then
    FLifeFactor := LifeFact
  else
    FLifeFactor := 0.005;
  UpdateFountain;
end;

function TGLFountainDummy.GetBounding: Boolean;
begin
  Result := FBounding;
end;

procedure TGLFountainDummy.SetBounding(const Bound: Boolean);
begin
  FBounding := Bound;
  UpdateFountain;
end;

function TGLFountainDummy.GetColorStart: longint;
begin
  Result := FColorStart;
end;

procedure TGLFountainDummy.SetColorStart(const ColStart: longint);
begin
  FColorStart := ColStart;
  RD := FColorStart;
  GD := FColorStart Shr 8;
  BD := FColorStart Shr 16;
  UpdateFountain;
end;

function TGLFountainDummy.GetColorEnd: longint;
begin
  Result := FColorEnd;
end;

procedure TGLFountainDummy.SetColorEnd(const ColEnd: longint);
begin
  FColorEnd := ColEnd;
  RF := FColorEnd;
  GF := FColorEnd Shr 8;
  BF := FColorEnd Shr 16;
  UpdateFountain;
end;

procedure TGLFountainDummy.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  Animation(rci);
  if renderChildren then
    self.renderChildren(0, Count - 1, rci);
end;

destructor TGLFountainDummy.Destroy;
begin
  FNbParticles := 0;
  LsParticles.Free;
  DeleteChildren;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------
initialization

// ---------------------------------------------------------------------

RegisterClass(TGLFountainDummy);

end.
