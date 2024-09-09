//
// The graphics engine GLScene https://github.com/glscene
//
unit GLS.Polyhedra;

(*
  Polyhedra support.
  The polyhedra are subclasses of polygon structures.
  The registered classes are:
    [TGLPolyTet, TGLPolyOct, TGLPolyHec, TGLPolyIso, TGLPolyDod, TGLPolyhedron, TGLPolyhedra]
*)

interface

{$I GLScene.Defines.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  VCL.Consts,

  GLScene.OpenGLTokens,
  GLScene.VectorTypes,
  GLScene.VectorTypesExt,
  GLScene.TextureFormat,

  GLScene.VectorGeometry,
  GLS.Scene,
  GLScene.BaseClasses,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.Portal,
  GLScene.VectorLists,
  GLScene.PersistentClasses,
  GLS.Silhouette,
  GLScene.Strings,
  GLS.Texture,
  GLS.Material,
  GLS.Mesh,
  GLScene.Logger,
  GLS.Octree,
  GLScene.GeometryBB,
  GLS.ApplicationFileIO,
  GLS.Context,
  GLScene.Color;


type

  (*
  A PolyTet object that handles polygyns for tetrahedron
  *)
  TGLPolyTet = class(TGLPolygon)
  public
    constructor CreateOwned(AOwner: TGLPolygon);
    destructor Destroy; override;
  end;


implementation


{ TGLPolyTet }

constructor TGLPolyTet.CreateOwned(AOwner: TGLPolygon);
begin
 //
end;

destructor TGLPolyTet.Destroy;
begin
  //
  inherited;
end;

end.
