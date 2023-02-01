// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.ODEUtils.pas' rev: 35.00 (Windows)

#ifndef Physics_OdeutilsHPP
#define Physics_OdeutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Physics.ODEImport.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Scene.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Objects.hpp>
#include <GLS.VerletClothify.hpp>
#include <GLS.VectorFileObjects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Odeutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall ODERToGLSceneMatrix(Gls::Vectortypes::TMatrix4f &m, const Physics::Odeimport::TdMatrix3_As3x4 &R, const Physics::Odeimport::TdVector3 &pos)/* overload */;
extern DELPHI_PACKAGE void __fastcall ODERToGLSceneMatrix(Gls::Vectortypes::TMatrix4f &m, Physics::Odeimport::PdMatrix3 R, Physics::Odeimport::PdVector3 pos)/* overload */;
extern DELPHI_PACKAGE void __fastcall ODERToGLSceneMatrix(Gls::Vectortypes::TMatrix4f &m, const Physics::Odeimport::TdMatrix3 &R, const Physics::Odeimport::TdVector3 &pos)/* overload */;
extern DELPHI_PACKAGE void __fastcall DrawBox(const Physics::Odeimport::TdVector3 &Sides);
extern DELPHI_PACKAGE Physics::Odeimport::TdMatrix3 __fastcall GLSceneMatrixToODER(const Gls::Vectortypes::TMatrix4f &m);
extern DELPHI_PACKAGE void __fastcall dsDrawBox(Physics::Odeimport::PdVector3 pos, Physics::Odeimport::PdMatrix3 R, const Physics::Odeimport::TdVector3 &Sides)/* overload */;
extern DELPHI_PACKAGE void __fastcall dsDrawBox(const Physics::Odeimport::TdVector3 &pos, const Physics::Odeimport::TdMatrix3 &R, const Physics::Odeimport::TdVector3 &Sides)/* overload */;
extern DELPHI_PACKAGE void __fastcall setTransform(const Physics::Odeimport::TdVector3 &pos, const Physics::Odeimport::TdMatrix3 &R);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall ConvertdVector3ToVector3f(const Physics::Odeimport::TdVector3 &R)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall ConvertdVector3ToVector3f(Physics::Odeimport::PdVector3 R)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall ConvertdVector3ToVector4f(const Physics::Odeimport::TdVector3 &R)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall ConvertdVector3ToVector4f(Physics::Odeimport::PdVector3 R)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall ConvertdVector3ToAffineVector(Physics::Odeimport::PdVector3 R)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall ConvertdVector3ToAffineVector(const Physics::Odeimport::TdVector3 &R)/* overload */;
extern DELPHI_PACKAGE Physics::Odeimport::TdVector3 __fastcall ConvertVector3fTodVector3(const Gls::Vectortypes::TVector3f &R);
extern DELPHI_PACKAGE Physics::Odeimport::PdVector3 __fastcall ConvertVector3fToPdVector3(const Gls::Vectortypes::TVector3f &R);
extern DELPHI_PACKAGE Physics::Odeimport::TdVector3 __fastcall ConvertVector4fTodVector3(const Gls::Vectortypes::TVector4f &R);
extern DELPHI_PACKAGE Physics::Odeimport::PdVector3 __fastcall ConvertVector4fToPdVector3(const Gls::Vectortypes::TVector4f &R);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall GetBodyPositionAsAffineVector(Physics::Odeimport::PdxBody Body);
extern DELPHI_PACKAGE void __fastcall PositionSceneObjectForGeom(Physics::Odeimport::PdxGeom Geom);
extern DELPHI_PACKAGE Gls::Vectortypes::TMatrix4f __fastcall GLMatrixFromGeom(Physics::Odeimport::PdxGeom Geom);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall GLDirectionFromGeom(Physics::Odeimport::PdxGeom Geom);
extern DELPHI_PACKAGE void __fastcall PositionSceneObject(Gls::Scene::TGLBaseSceneObject* GLBaseSceneObject, Physics::Odeimport::PdxGeom Geom);
extern DELPHI_PACKAGE void __fastcall CopyCubeSizeFromBox(Gls::Objects::TGLCube* Cube, Physics::Odeimport::PdxGeom Geom);
extern DELPHI_PACKAGE void __fastcall CopyPosFromGeomToGL(Physics::Odeimport::PdxGeom Geom, Gls::Scene::TGLBaseSceneObject* GLBaseSceneObject);
extern DELPHI_PACKAGE Physics::Odeimport::PdxGeom __fastcall CreateGeomFromCube(Gls::Objects::TGLCube* Cube, Physics::Odeimport::PdxSpace Space);
extern DELPHI_PACKAGE Physics::Odeimport::PdxBody __fastcall CreateBodyFromCube(Physics::Odeimport::PdxGeom &Geom, Gls::Objects::TGLCube* Cube, Physics::Odeimport::PdxWorld World, Physics::Odeimport::PdxSpace Space);
extern DELPHI_PACKAGE Physics::Odeimport::PdxGeom __fastcall CreateTriMeshFromBaseMesh(Gls::Vectorfileobjects::TGLBaseMesh* GLBaseMesh, Physics::Odeimport::PdxSpace Space, Physics::Odeimport::PdVector3Array &Vertices, Physics::Odeimport::PdIntegerArray &Indices);
extern DELPHI_PACKAGE void __fastcall CopyBodyFromCube(Physics::Odeimport::PdxBody Body, Physics::Odeimport::PdxGeom &Geom, Gls::Objects::TGLCube* Cube, Physics::Odeimport::PdxSpace Space);
extern DELPHI_PACKAGE float __fastcall dBodyToBodyDistance(Physics::Odeimport::PdxBody Body1, Physics::Odeimport::PdxBody Body2);
extern DELPHI_PACKAGE float __fastcall dVector3Length(const Physics::Odeimport::TdVector3 &R)/* overload */;
extern DELPHI_PACKAGE float __fastcall dVector3Length(Physics::Odeimport::PdVector3 R)/* overload */;
extern DELPHI_PACKAGE void __fastcall RenderGeomList(Physics::Odeimport::TGeomList* GeomList);
extern DELPHI_PACKAGE Physics::Odeimport::PdxGeom __fastcall CreateODEPlaneFromGLPlane(Gls::Objects::TGLPlane* Plane, Physics::Odeimport::PdxSpace Space);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall RandomColorVector(void);
}	/* namespace Odeutils */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_ODEUTILS)
using namespace Physics::Odeutils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_OdeutilsHPP
