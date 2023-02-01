// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Isosurface.pas' rev: 35.00 (Windows)

#ifndef Gls_IsosurfaceHPP
#define Gls_IsosurfaceHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Mesh.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorTypesExt.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Isosurface
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMarchingCube;
class DELPHICLASS TGLIsoSurfaceExtractor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMarchingCube : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Extended FIsoValue;
	Gls::Vectorgeometry::TIntegerVector *PVertsX;
	Gls::Vectorgeometry::TIntegerVector *PVertsY;
	Gls::Vectorgeometry::TIntegerVector *PVertsZ;
	int _Nverts;
	int _Ntrigs;
	int _Sverts;
	int _Strigs;
	Gls::Vectortypesext::TVertArray *PVertices;
	Gls::Vectortypesext::TTriangleRecArray *PTriangles;
	unsigned _i;
	unsigned _j;
	unsigned _k;
	System::StaticArray<Gls::Vectortypesext::TVoxelRec, 8> _Cube;
	System::Byte _lut_entry;
	void __fastcall Init_temps();
	void __fastcall Init_all();
	void __fastcall Init_space();
	void __fastcall Clean_temps();
	void __fastcall Clean_all(bool keepFacets = false);
	void __fastcall Clean_space();
	void __fastcall Test_vertex_addiction();
	
protected:
	bool FOriginalMC;
	int FSizeX;
	int FSizeY;
	int FSizeZ;
	float FxMin;
	float FxMax;
	float FyMin;
	float FyMax;
	float FzMin;
	float FzMax;
	float FStepX;
	float FStepY;
	float FStepZ;
	Gls::Vectortypesext::TVoxelData *VoxelData;
	void __fastcall Process_Cube();
	void __fastcall Compute_Intersection_Points();
	void __fastcall Add_Triangle(int *trig, const int trig_High, System::Byte N, int v12 = 0xffffffff);
	int __fastcall Add_x_vertex();
	int __fastcall Add_y_vertex();
	int __fastcall Add_z_vertex();
	int __fastcall Add_c_vertex();
	float __fastcall Get_x_grad(int i, int j, int k);
	float __fastcall Get_y_grad(int i, int j, int k);
	float __fastcall Get_z_grad(int i, int j, int k);
	int __fastcall Get_x_vert(int i, int j, int k);
	int __fastcall Get_y_vert(int i, int j, int k);
	int __fastcall Get_z_vert(int i, int j, int k);
	void __fastcall Set_x_vert(int a_val, int i, int j, int k);
	void __fastcall Set_y_vert(int a_val, int i, int j, int k);
	void __fastcall Set_z_vert(int a_val, int i, int j, int k);
	System::Extended __fastcall GetVoxelValue(int i, int j, int k);
	void __fastcall SetVoxelValue(int i, int j, int k, System::Extended HfValue);
	Gls::Vectortypesext::TVoxelRec __fastcall GetVoxelData(int i, int j, int k);
	Gls::Vectortypesext::PVoxelRec __fastcall Voxel(int i, int j, int k);
	virtual System::Extended __fastcall Calc_u(float v1, float v2);
	
public:
	Gls::Vectortypesext::TGLScalarField ScalarField;
	__fastcall virtual TGLMarchingCube()/* overload */;
	__fastcall virtual TGLMarchingCube(int SizeX, int SizeY, int SizeZ, System::Extended AIsoValue, float xMin, float xMax, float yMin, float yMax, float zMin, float zMax)/* overload */;
	virtual void __fastcall ReDim(int ASizeX, int ASizeY, int ASizeZ, float xMin, float xMax, float yMin, float yMax, float zMin, float zMax);
	__fastcall virtual ~TGLMarchingCube();
	void __fastcall Run()/* overload */;
	void __fastcall Run(System::Extended IsoValue)/* overload */;
	virtual bool __fastcall Internal(System::Extended AValue);
	virtual void __fastcall FillVoxelData()/* overload */;
	virtual void __fastcall FillVoxelData(System::Extended AIsoValue, Gls::Vectortypesext::TGLScalarField AScalarField = 0x0)/* overload */;
	virtual void __fastcall FillVoxelData(System::Extended AIsoValue, Gls::Vectortypesext::TGLScalarFieldInt AScalarField)/* overload */;
	void __fastcall CalcVertices(Gls::Mesh::TGLVertexList* Vertices, float Alpha = 1.000000E+00f);
	void __fastcall CalcMeshObject(Gls::Vectorfileobjects::TGLMeshObject* AMeshObject, float Alpha = 1.000000E+00f);
	__property System::Extended IsoValue = {read=FIsoValue, write=FIsoValue};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLIsoSurfaceExtractor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Gls::Vectortypesext::TArray3DExt Data;
	System::StaticArray<int, 3> Dimensions;
	System::Word __fastcall BuildIndex(float *ADatavals, const int ADatavals_High, float Isovalue);
	Gls::Vectortypes::TVector3f __fastcall Interpolate(const Gls::Vectortypes::TVector3f &V0, const Gls::Vectortypes::TVector3f &V1, float &Val0, float &Val1, float &Isovalue, bool isPolished);
	
public:
	__fastcall TGLIsoSurfaceExtractor()/* overload */;
	__fastcall TGLIsoSurfaceExtractor(int Xdim, int Ydim, int Zdim, Gls::Vectortypesext::TArray3DExt &AData)/* overload */;
	__fastcall virtual ~TGLIsoSurfaceExtractor();
	void __fastcall AssignData(int Xdim, int Ydim, int Zdim, Gls::Vectortypesext::TArray3DExt &AData);
	void __fastcall MarchingCubes(float Isovalue, /* out */ Gls::Vectortypesext::TVertexArray &Vertices, /* out */ Gls::Vectortypesext::TIntegerArray &Triangles, bool isPolished);
	void __fastcall MarchingTetrahedra(float Isovalue, /* out */ Gls::Vectortypesext::TVertexArray &Vertices, /* out */ Gls::Vectortypesext::TIntegerArray &Triangles, bool isPolished);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const int ALLOC_SIZE = int(0x10000);
extern DELPHI_PACKAGE System::Extended __fastcall SFSphere(System::Extended X, System::Extended Y, System::Extended Z);
extern DELPHI_PACKAGE System::Extended __fastcall SFToroidal(System::Extended X, System::Extended Y, System::Extended Z);
extern DELPHI_PACKAGE System::Extended __fastcall SFDoubleTorus(System::Extended X, System::Extended Y, System::Extended Z);
extern DELPHI_PACKAGE System::Extended __fastcall SFChmutov1(System::Extended X, System::Extended Y, System::Extended Z);
extern DELPHI_PACKAGE System::Extended __fastcall SFChmutov2(System::Extended X, System::Extended Y, System::Extended Z);
extern DELPHI_PACKAGE System::Extended __fastcall SFKleinBottle(System::Extended X, System::Extended Y, System::Extended Z);
extern DELPHI_PACKAGE System::Extended __fastcall SFMinkowski(System::Extended X, System::Extended Y, System::Extended Z);
}	/* namespace Isosurface */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ISOSURFACE)
using namespace Gls::Isosurface;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_IsosurfaceHPP
