//
// This unit is part of the GLScene Engine, http://glscene.org
//
unit GLTeapot;

(* Implements the standard Teapot, build from evaluators *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,

  GLScene,
  GLPersistentClasses,
  GLState,
  GLVectorGeometry,
  GLPipelineTransformation,
  GLContext,
  GLRenderContextInfo,
  GLVectorTypes,
  GLMaterial,
  GLTexture;

type

  (* The classic teapot.
     The only use of this object is as placeholder for testing... *)
  TGLTeapot = class(TGLSceneObject)
  private
    FGrid: Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

// ------------------
// ------------------ TGLTeapot ------------------
// ------------------

constructor TGLTeapot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGrid := 5;
end;

function TGLTeapot.AxisAlignedDimensionsUnscaled: TVector;
begin
  SetVector(Result, 0.55, 0.25, 0.35);
end;

procedure TGLTeapot.BuildList(var rci: TGLRenderContextInfo);

const
  PatchData: array[0..9, 0..15] of Integer =
    ((102, 103, 104, 105, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), // rim
    (12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27), // body
    (24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40), // body
    (96, 96, 96, 96, 97, 98, 99, 100, 101, 101, 101, 101, 0, 1, 2, 3), // lid
    (0, 1, 2, 3, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117), // lid
    (118, 118, 118, 118, 124, 122, 119, 121, 123, 126, 125, 120, 40, 39, 38, 37), // bottom
    (41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56), // handle
    (53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 28, 65, 66, 67), // handle
    (68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83), // spout
    (80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95)); // spout

  CPData: array[0..126, 0..2] of Single =
    ((0.2, 0, 2.7), (0.2, -0.112, 2.7), (0.112, -0.2, 2.7), (0, -0.2, 2.7), (1.3375, 0, 2.53125),
    (1.3375, -0.749, 2.53125), (0.749, -1.3375, 2.53125), (0, -1.3375, 2.53125),
    (1.4375, 0, 2.53125), (1.4375, -0.805, 2.53125), (0.805, -1.4375, 2.53125),
    (0, -1.4375, 2.53125), (1.5, 0, 2.4), (1.5, -0.84, 2.4), (0.84, -1.5, 2.4), (0, -1.5, 2.4),
    (1.75, 0, 1.875), (1.75, -0.98, 1.875), (0.98, -1.75, 1.875), (0, -1.75, 1.875), (2, 0, 1.35),
    (2, -1.12, 1.35), (1.12, -2, 1.35), (0, -2, 1.35), (2, 0, 0.9), (2, -1.12, 0.9), (1.12, -2, 0.9),
    (0, -2, 0.9), (-2, 0, 0.9), (2, 0, 0.45), (2, -1.12, 0.45), (1.12, -2, 0.45), (0, -2, 0.45),
    (1.5, 0, 0.225), (1.5, -0.84, 0.225), (0.84, -1.5, 0.225), (0, -1.5, 0.225), (1.5, 0, 0.15),
    (1.5, -0.84, 0.15), (0.84, -1.5, 0.15), (0, -1.5, 0.15), (-1.6, 0, 2.025), (-1.6, -0.3, 2.025),
    (-1.5, -0.3, 2.25), (-1.5, 0, 2.25), (-2.3, 0, 2.025), (-2.3, -0.3, 2.025), (-2.5, -0.3, 2.25),
    (-2.5, 0, 2.25), (-2.7, 0, 2.025), (-2.7, -0.3, 2.025), (-3, -0.3, 2.25), (-3, 0, 2.25),
    (-2.7, 0, 1.8), (-2.7, -0.3, 1.8), (-3, -0.3, 1.8), (-3, 0, 1.8), (-2.7, 0, 1.575),
    (-2.7, -0.3, 1.575), (-3, -0.3, 1.35), (-3, 0, 1.35), (-2.5, 0, 1.125), (-2.5, -0.3, 1.125),
    (-2.65, -0.3, 0.9375), (-2.65, 0, 0.9375), (-2, -0.3, 0.9), (-1.9, -0.3, 0.6), (-1.9, 0, 0.6),
    (1.7, 0, 1.425), (1.7, -0.66, 1.425), (1.7, -0.66, 0.6), (1.7, 0, 0.6), (2.6, 0, 1.425),
    (2.6, -0.66, 1.425), (3.1, -0.66, 0.825), (3.1, 0, 0.825), (2.3, 0, 2.1), (2.3, -0.25, 2.1),
    (2.4, -0.25, 2.025), (2.4, 0, 2.025), (2.7, 0, 2.4), (2.7, -0.25, 2.4), (3.3, -0.25, 2.4),
    (3.3, 0, 2.4), (2.8, 0, 2.475), (2.8, -0.25, 2.475), (3.525, -0.25, 2.49375),
    (3.525, 0, 2.49375), (2.9, 0, 2.475), (2.9, -0.15, 2.475), (3.45, -0.15, 2.5125),
    (3.45, 0, 2.5125), (2.8, 0, 2.4), (2.8, -0.15, 2.4), (3.2, 0.15, 2.4), (3.2, 0, 2.4),
    (0, 0, 3.15), (0.8, 0, 3.15), (0.8, -0.45, 3.15), (0.45, -0.8, 3.15), (0, -0.8, 3.15),
    (0, 0, 2.85), (1.4, 0, 2.4), (1.4, -0.784, 2.4), (0.784, -1.4, 2.4), (0, -1.4, 2.4),
    (0.4, 0, 2.55), (0.4, -0.224, 2.55), (0.224, -0.4, 2.55), (0, -0.4, 2.55), (1.3, 0, 2.55),
    (1.3, -0.728, 2.55), (0.728, -1.3, 2.55), (0, -1.3, 2.55), (1.3, 0, 2.4), (1.3, -0.728, 2.4),
    (0.728, -1.3, 2.4), (0, -1.3, 2.4), (0, 0, 0), (1.425, -0.798, 0), (1.5, 0, 0.075), (1.425, 0, 0),
    (0.798, -1.425, 0), (0, -1.5, 0.075), (0, -1.425, 0), (1.5, -0.84, 0.075), (0.84, -1.5, 0.075));

  Tex: array[0..1, 0..1, 0..1] of Single =
    (((0, 0), (1, 0)), ((0, 1), (1, 1)));

var
  P, Q, R, S: array[0..3, 0..3, 0..2] of Single;
  I, J, K, L, GRD: Integer;
begin
  if FGrid < 2 then
    FGrid := 2;
  GRD := FGrid;

  rci.GLStates.InvertGLFrontFace;
  glEnable(GL_AUTO_NORMAL);
  glEnable(GL_MAP2_VERTEX_3);
  glEnable(GL_MAP2_TEXTURE_COORD_2);
  for I := 0 to 9 do
  begin
    for J := 0 to 3 do
    begin
      for K := 0 to 3 do
      begin
        for L := 0 to 2 do
        begin
          P[J, K, L] := CPData[PatchData[I, J * 4 + K], L];
          Q[J, K, L] := CPData[PatchData[I, J * 4 + (3 - K)], L];
          if L = 1 then
            Q[J, K, L] := -Q[J, K, L];
          if I < 6 then
          begin
            R[J, K, L] := CPData[PatchData[I, J * 4 + (3 - K)], L];
            if L = 0 then
              R[J, K, L] := -R[J, K, L];
            S[J, K, L] := CPData[PatchData[I, J * 4 + K], L];
            if L < 2 then
              S[J, K, L] := -S[J, K, L];
          end;
        end;
      end;
    end;
    glMapGrid2f(GRD, 0, 1, GRD, 0, 1);
    glMap2f(GL_MAP2_TEXTURE_COORD_2, 0, 1, 2, 2, 0, 1, 4, 2, @Tex[0, 0, 0]);
    glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @P[0, 0, 0]);
    glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @Q[0, 0, 0]);
    glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    if I < 6 then
    begin
      glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @R[0, 0, 0]);
      glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
      glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @S[0, 0, 0]);
      glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    end;
  end;
  glDisable(GL_AUTO_NORMAL);
  glDisable(GL_MAP2_VERTEX_3);
  glDisable(GL_MAP2_TEXTURE_COORD_2);
  rci.GLStates.InvertGLFrontFace;
end;

procedure TGLTeapot.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
const
  M: TMatrix = (
  X:(X:0.150000005960464; Y:0; Z:0; W:0);
  Y:(X:0; Y:-6.55670850946422e-09; Z:-0.150000005960464; W:0);
  Z:(X:0; Y:0.150000005960464; Z:-6.55670850946422e-09; W:0);
  W:(X:0; Y:1.63917712736605e-09; Z:0.0375000014901161; W:1));
begin
  // start rendering self
  if ARenderSelf then
  begin
    with ARci.PipelineTransformation do
      SetModelMatrix(MatrixMultiply(M, ModelMatrix^));
    if ARci.ignoreMaterials then
      if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
        BuildList(ARci)
      else
        ARci.GLStates.CallList(GetHandle(ARci))
    else
    begin
      Material.Apply(ARci);
      repeat
        if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
          BuildList(ARci)
        else
          ARci.GLStates.CallList(GetHandle(ARci));
      until not Material.UnApply(ARci);
    end;
  end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClasses([TGLTeapot]);

end.


