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

{$I GLS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  VCL.Consts,

  GLS.OpenGLTokens,
  GLS.VectorTypes,
  GLS.VectorTypesExt,
  GLS.TextureFormat,

  GLS.VectorGeometry,
  GLS.Scene,
  GLS.BaseClasses,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.Portal,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.Silhouette,
  GLS.Strings,
  GLS.Texture,
  GLS.Material,
  GLS.Mesh,
  GLS.Logger,
  GLS.Octree,
  GLS.GeometryBB,
  GLS.ApplicationFileIO,
  GLS.Context,
  GLS.Color;


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
