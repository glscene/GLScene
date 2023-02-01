// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.ODEImport.pas' rev: 35.00 (Windows)

#ifndef Physics_OdeimportHPP
#define Physics_OdeimportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.ModuleLoader.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Odeimport
{
//-- forward type declarations -----------------------------------------------
struct TdxJointGroup;
struct TdxJointLimitMotor;
struct TdMass;
struct TdxAutoDisable;
struct TdxDampingParameters;
struct TdxContactParameters;
struct TdxQuickStepParameters;
struct TdObject;
struct TdxJointNode;
struct TJointInfo1;
struct TJointInfo2;
struct TdxJoint;
struct TdxJointBall;
struct TdxJointHinge;
struct TdxJointUniversial;
struct TdxJointPR;
struct TdxJointPiston;
struct TdxJointSlider;
struct TdxJointHinge2;
struct TdxJointAMotor;
struct TdxJointLMotor;
struct TdxJointPlane2D;
struct TdxJointFixed;
struct TdxPosR;
struct TdxBody;
class DELPHICLASS TBodyList;
struct TdxWorld;
struct TdJointFeedback;
struct TdSurfaceParameters;
struct TdContactGeom;
struct TdContact;
struct TdGeomClass;
struct TdxTriMeshData;
struct TdxHeightfieldData;
struct TdGeomSpaceData;
struct TdxGeom;
class DELPHICLASS TGeomList;
struct TdxSpace;
struct TdxHashSpace;
struct TdxQuadTreeSpace;
//-- type declarations -------------------------------------------------------
typedef int TJointFlag;

typedef int TdContactType;

typedef int TBodyFlags;

typedef float TdReal;

typedef float *PdReal;

typedef TdxJointGroup *PdxJointGroup;

typedef PdxJointGroup TdJointGroupID;

struct DECLSPEC_DRECORD TdxJointGroup
{
public:
	int num;
	void *stack;
};


typedef TdxJointLimitMotor *PdxJointLimitMotor;

struct DECLSPEC_DRECORD TdxJointLimitMotor
{
public:
	float vel;
	float fmax;
	float lostop;
	float histop;
	float fudge_factor;
	float normal_cfm;
	float stop_erp;
	float stop_sfm;
	float bounce;
	int limit;
	float limit_err;
};


typedef System::StaticArray<float, 16> TdRealArray;

typedef TdRealArray *PdRealArray;

typedef System::StaticArray<float, 4> TdVector3;

typedef TdVector3 *PdVector3;

typedef System::StaticArray<System::StaticArray<float, 4>, 3> Td3Axis;

typedef Td3Axis *Pd3Axis;

typedef System::StaticArray<int, 3> TdInteger3;

typedef TdInteger3 *PdInteger3;

typedef System::StaticArray<TdxJointLimitMotor, 3> TdxJointLimitMotor3;

typedef TdxJointLimitMotor3 *PdxJointLimitMotor3;

typedef System::StaticArray<float, 4> TdVector4;

typedef TdVector4 *PdVector4;

typedef System::StaticArray<float, 12> TdMatrix3;

typedef TdMatrix3 *PdMatrix3;

typedef System::StaticArray<System::StaticArray<float, 4>, 3> TdMatrix3_As3x4;

typedef TdMatrix3_As3x4 *PdMatrix3_As3x4;

typedef System::StaticArray<float, 16> TdMatrix4;

typedef TdMatrix4 *PdMatrix4;

typedef System::StaticArray<float, 48> TdMatrix6;

typedef TdMatrix6 *PdMatrix6;

typedef TdVector4 TdQuaternion;

typedef TdVector4 *PdQuaternion;

typedef System::StaticArray<float, 6> TdAABB;

struct DECLSPEC_DRECORD TdMass
{
public:
	float mass;
	TdVector4 c;
	TdMatrix3 I;
};


typedef TdMass *PdMass;

struct DECLSPEC_DRECORD TdxAutoDisable
{
public:
	float idle_time;
	int idle_steps;
	float linear_average_threashold;
	float angular_average_threashold;
	unsigned average_samples;
};


struct DECLSPEC_DRECORD TdxDampingParameters
{
public:
	float linear_scale;
	float angular_scale;
	float linear_threahold;
	float angular_threashold;
};


struct DECLSPEC_DRECORD TdxContactParameters
{
public:
	float max_vel;
	float min_depth;
};


struct DECLSPEC_DRECORD TdxQuickStepParameters
{
public:
	int num_iterations;
	float w;
};


typedef TdxGeom *PdxGeom;

typedef PdxGeom *PPdxGeom;

typedef void __cdecl (*TdMovedCallback)(PdxGeom o1);

typedef int __fastcall (*TdTriCallback)(PdxGeom TriMesh, PdxGeom RefObject, int TriangleIndex);

typedef void __fastcall (*TdTriArrayCallback)(PdxGeom TriMesh, PdxGeom RefObject, System::PIntegerArray TriIndices, int TriCount);

typedef int __fastcall (*TdTriRayCallback)(PdxGeom TriMesh, PdxGeom Ray, int TriangleIndex, float u, float v);

typedef TdxWorld *PdxWorld;

typedef TdObject *PdObject;

typedef PdObject *PPdObject;

struct DECLSPEC_DRECORD TdObject
{
public:
	TdxWorld *World;
	TdObject *next;
	PdObject *tome;
	void *userdata;
	int tag;
};


typedef TdxBody *PdxBody;

typedef TdxJoint *PdxJoint;

typedef PdxJoint TdJointID;

typedef TdxJointNode *PdxJointNode;

struct DECLSPEC_DRECORD TdxJointNode
{
public:
	TdxJoint *joint;
	TdxBody *body;
	TdxJointNode *next;
};


struct DECLSPEC_DRECORD TJointInfo1
{
public:
	int m;
	int nub;
};


struct DECLSPEC_DRECORD TJointInfo2
{
public:
	float fps;
	float erp;
	float *J1l;
	float *J1a;
	float *J2l;
	float *J2a;
	int rowskip;
	float *c;
	float *cfm;
	float *lo;
	float *hi;
	int *findex;
};


struct DECLSPEC_DRECORD TdxJoint
{
public:
	TdObject baseObject;
	TJointInfo1 Info1;
	TJointInfo2 Info2;
};


typedef TdxJoint TdxJointNull;

typedef TdxJointBall *PdxJointBall;

struct DECLSPEC_DRECORD TdxJointBall
{
public:
	TdxJoint BaseJoint;
	TdVector3 anchor1;
	TdVector3 anchor2;
	float erp;
	float cfm;
};


typedef TdxJointHinge *PdxJointHinge;

struct DECLSPEC_DRECORD TdxJointHinge
{
public:
	TdxJoint BaseJoint;
	TdVector3 anchor1;
	TdVector3 anchor2;
	TdVector3 axis1;
	TdVector3 axis2;
	TdVector4 qrel;
	TdxJointLimitMotor limot;
};


typedef TdxJointUniversial *PdxJointUniversial;

struct DECLSPEC_DRECORD TdxJointUniversial
{
public:
	TdxJoint BaseJoint;
	TdVector3 anchor1;
	TdVector3 anchor2;
	TdVector3 axis1;
	TdVector3 axis2;
	TdVector4 qrel1;
	TdVector4 qrel2;
	TdxJointLimitMotor limot1;
	TdxJointLimitMotor limot2;
};


typedef TdxJointPR *PdxJointPR;

struct DECLSPEC_DRECORD TdxJointPR
{
public:
	TdxJoint BaseJoint;
	TdVector3 anchor2;
	TdVector3 axisR1;
	TdVector3 axisR2;
	TdVector3 axisP1;
	TdVector4 qrel;
	TdVector3 offset;
	TdxJointLimitMotor limotR;
	TdxJointLimitMotor limotP;
};


typedef TdxJointPiston *PdxJointPiston;

struct DECLSPEC_DRECORD TdxJointPiston
{
public:
	TdxJoint BaseJoint;
	TdVector3 axis1;
	TdVector3 axis2;
	TdVector4 qrel;
	TdVector3 anchor1;
	TdVector3 anchor2;
	TdxJointLimitMotor limotP;
	TdxJointLimitMotor limotR;
};


typedef TdxJointSlider *PdxJointSlider;

struct DECLSPEC_DRECORD TdxJointSlider
{
public:
	TdxJoint BaseJoint;
	TdVector3 axis1;
	TdVector4 qrel;
	TdVector3 offset;
	TdxJointLimitMotor limot;
};


typedef TdxJointHinge2 *PdxJointHinge2;

struct DECLSPEC_DRECORD TdxJointHinge2
{
public:
	TdxJoint BaseJoint;
	TdVector3 anchor1;
	TdVector3 anchor2;
	TdVector3 axis1;
	TdVector3 axis2;
	float c0;
	float s0;
	TdVector3 v1;
	TdVector3 v2;
	TdxJointLimitMotor limot1;
	TdxJointLimitMotor limot2;
	float susp_erp;
	float susp_cfm;
};


struct DECLSPEC_DRECORD TdxJointAMotor
{
public:
	TdxJoint BaseJoint;
	int num;
	int mode;
	TdInteger3 rel;
	Td3Axis axis;
	TdxJointLimitMotor3 limot;
	TdVector3 angle;
	TdVector3 reference1;
	TdVector3 reference2;
};


struct DECLSPEC_DRECORD TdxJointLMotor
{
public:
	TdxJoint BaseJoint;
	int num;
	TdInteger3 rel;
	Td3Axis axis;
	TdxJointLimitMotor3 limot;
};


struct DECLSPEC_DRECORD TdxJointPlane2D
{
public:
	TdxJoint BaseJoint;
	int row_motor_x;
	int row_motor_y;
	int row_motor_angle;
	TdxJointLimitMotor motor_x;
	TdxJointLimitMotor motor_y;
	TdxJointLimitMotor motor_angle;
};


struct DECLSPEC_DRECORD TdxJointFixed
{
public:
	TdxJoint BaseJoint;
	TdVector4 qrel;
	TdVector3 offset;
	float erp;
	float cfm;
};


typedef TdxPosR *PdxPosR;

struct DECLSPEC_DRECORD TdxPosR
{
public:
	TdVector3 pos;
	TdMatrix3 R;
};


struct DECLSPEC_DRECORD TdxBody
{
public:
	TdObject baseObject;
	System::Byte Padding;
	TdxJoint *firstjoint;
	int flags;
	TdxGeom *geom;
	TdMass mass;
	TdMatrix3 invI;
	float invMass;
	TdxPosR posr;
	TdVector4 q;
	TdVector3 lvel;
	TdVector3 avel;
	TdVector3 facc;
	TdVector3 tacc;
	TdVector3 finite_rot_axis;
	TdxAutoDisable adis;
	float adis_timeleft;
	int adis_stepsleft;
	TdVector3 *average_lvel_buffer;
	TdVector3 *average_avel_buffer;
	unsigned average_counter;
	int average_ready;
	TdMovedCallback moved_callback;
	TdxDampingParameters dampingp;
	float max_angular_speed;
};


class PASCALIMPLEMENTATION TBodyList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	PdxBody operator[](int I) { return this->Items[I]; }
	
private:
	PdxBody __fastcall GetItems(int I);
	void __fastcall SetItems(int I, const PdxBody Value);
	
public:
	__property PdxBody Items[int I] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall DeleteAllBodies();
public:
	/* TList.Destroy */ inline __fastcall virtual ~TBodyList() { }
	
public:
	/* TObject.Create */ inline __fastcall TBodyList() : System::Classes::TList() { }
	
};


struct DECLSPEC_DRECORD TdxWorld
{
public:
	TdxBody *firstbody;
	TdxJoint *firstjoint;
	int nb;
	int nj;
	TdVector3 gravity;
	float global_erp;
	float global_cfm;
	TdxAutoDisable adis;
	int body_flags;
	TdxQuickStepParameters qs;
	TdxContactParameters contactp;
	TdxDampingParameters dampingp;
	float max_angular_speed;
};


struct DECLSPEC_DRECORD TdJointFeedback
{
public:
	TdVector3 f1;
	TdVector3 t1;
	TdVector3 f2;
	TdVector3 t2;
};


typedef TdJointFeedback *PTdJointFeedback;

enum DECLSPEC_DENUM TdErrorType : unsigned char { d_ERR_UNKNOWN, d_ERR_IASSERT, d_ERR_UASSERT, d_ERR_LCP };

enum DECLSPEC_DENUM TdJointTypeNumbers : unsigned char { dJointTypeNone, dJointTypeBall, dJointTypeHinge, dJointTypeSlider, dJointTypeContact, dJointTypeUniversal, dJointTypeHinge2, dJointTypeFixed, dJointTypeNull, dJointTypeAMotor, dJointTypeLMotor, dJointTypePlane2D, dJointTypePR, dJointTypePU, dJointTypePiston };

enum DECLSPEC_DENUM TdAngularMotorModeNumbers : unsigned char { dAMotorUser, dAMotorEuler };

struct DECLSPEC_DRECORD TdSurfaceParameters
{
public:
	int mode;
	float mu;
	float mu2;
	float bounce;
	float bounce_vel;
	float soft_erp;
	float soft_cfm;
	float motion1;
	float motion2;
	float motionN;
	float slip1;
	float slip2;
};


struct DECLSPEC_DRECORD TdContactGeom
{
public:
	TdVector3 pos;
	TdVector3 normal;
	float depth;
	TdxGeom *g1;
	TdxGeom *g2;
	int side1;
	int side2;
};


typedef TdContactGeom *PdContactGeom;

struct DECLSPEC_DRECORD TdContact
{
public:
	TdSurfaceParameters surface;
	TdContactGeom geom;
	TdVector3 fdir1;
};


typedef TdContact *PdContact;

typedef void __cdecl (*TdNearCallback)(void * data, PdxGeom o1, PdxGeom o2);

typedef int __cdecl (*TdColliderFn)(PdxGeom o1, PdxGeom o2, int flags, PdContactGeom contact, int skip);

typedef TdColliderFn __cdecl (*TdGetColliderFnFn)(int num);

typedef void __cdecl (*TdGetAABBFn)(PdxGeom g, TdAABB &aabb);

typedef void __cdecl (*TdGeomDtorFn)(PdxGeom o);

typedef int __cdecl (*TdAABBTestFn)(PdxGeom o1, PdxGeom o2, const TdAABB &aabb2);

struct DECLSPEC_DRECORD TdGeomClass
{
public:
	int bytes;
	TdGetColliderFnFn collider;
	TdGetAABBFn aabb;
	TdAABBTestFn aabb_test;
	TdGeomDtorFn dtor;
};


typedef TdGeomClass *PdGeomClass;

typedef TdxSpace *PdxSpace;

typedef System::StaticArray<float, 65536> TdRealHugeArray;

typedef TdRealHugeArray *PdRealHugeArray;

typedef System::StaticArray<int, 65536> TdIntegerArray;

typedef TdIntegerArray *PdIntegerArray;

typedef System::StaticArray<System::StaticArray<float, 4>, 65536> TdVector3Array;

typedef TdVector3Array *PdVector3Array;

struct DECLSPEC_DRECORD TdxTriMeshData
{
public:
	System::Byte unknown;
};


typedef TdxTriMeshData *PdxTriMeshData;

struct DECLSPEC_DRECORD TdxHeightfieldData
{
public:
	float m_fWidth;
	float m_fDepth;
	float m_fSampleWidth;
	float m_fSampleDepth;
	float m_fInvSampleWidth;
	float m_fInvSampleDepth;
	float m_fHalfWidth;
	float m_fHalfDepth;
	float m_fMinHeight;
	float m_fMaxHeight;
	float m_fThickness;
	float m_fScale;
	float m_fOffset;
	int m_nWidthSamples;
	int m_nDepthSamples;
	int m_bCopyHeightData;
	int m_bWrapMode;
	int m_nGetHeightMode;
	void *m_pHeightData;
	void *m_pUserData;
	TdContactGeom *m_contacts;
};


typedef TdxHeightfieldData *PdxHeightfieldData;

typedef TdxSpace *PdxSimpleSpace;

typedef TdxHashSpace *PdxHashSpace;

struct DECLSPEC_DRECORD TdGeomSpaceData
{
public:
	TdxGeom *next;
};


struct DECLSPEC_DRECORD TdxGeom
{
public:
	int _type;
	System::Byte Padding;
	int gflags;
	void *data;
	TdxBody *body;
	TdxGeom *body_next;
	TdxPosR *final_posr;
	TdxPosR *offset_posr;
	TdxGeom *next;
	PdxGeom *tome;
	TdxSpace *parent_space;
	TdAABB aabb;
	unsigned category_bits;
	unsigned collide_bits;
};


class PASCALIMPLEMENTATION TGeomList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	PdxGeom operator[](int I) { return this->Items[I]; }
	
private:
	PdxGeom __fastcall GetItems(int I);
	void __fastcall SetItems(int I, const PdxGeom Value);
	
public:
	__property PdxGeom Items[int I] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall DeleteAllGeoms(bool DeleteDataAsObject = false);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGeomList() { }
	
public:
	/* TObject.Create */ inline __fastcall TGeomList() : System::Classes::TList() { }
	
};


struct DECLSPEC_DRECORD TdxSpace
{
public:
	TdxGeom baseGeom;
	int count;
	TdxGeom *first;
	int cleanup;
	int current_index;
	TdxGeom *current_geom;
	int lock_count;
};


typedef TdxSpace TdxSimpleSpace;

struct DECLSPEC_DRECORD TdxHashSpace
{
public:
	TdxSpace BaseSpace;
	int global_minlevel;
	int global_maxlevel;
};


struct DECLSPEC_DRECORD TdxQuadTreeSpace
{
public:
	TdxSpace BaseSpace;
	void *Blocks;
	void *DirtyList;
};


typedef int TJointParams;

//-- var, const, procedure ---------------------------------------------------
#define ODEDLL L"ode64s.dll"
static const System::Int8 TRIMESH_FACE_NORMALS = System::Int8(0x0);
static const System::Int8 TRIMESH_LAST_TRANSFORMATION = System::Int8(0x1);
static const unsigned CONTACTS_UNIMPORTANT = unsigned(0x80000000);
extern DELPHI_PACKAGE int dJOINT_INGROUP;
extern DELPHI_PACKAGE int dJOINT_REVERSE;
extern DELPHI_PACKAGE int dJOINT_TWOBODIES;
extern DELPHI_PACKAGE int dContactMu2;
extern DELPHI_PACKAGE int dContactFDir1;
extern DELPHI_PACKAGE int dContactBounce;
extern DELPHI_PACKAGE int dContactSoftERP;
extern DELPHI_PACKAGE int dContactSoftCFM;
extern DELPHI_PACKAGE int dContactMotion1;
extern DELPHI_PACKAGE int dContactMotion2;
extern DELPHI_PACKAGE int dContactMotionN;
extern DELPHI_PACKAGE int dContactSlip1;
extern DELPHI_PACKAGE int dContactSlip2;
extern DELPHI_PACKAGE int dContactApprox0;
extern DELPHI_PACKAGE int dContactApprox1_1;
extern DELPHI_PACKAGE int dContactApprox1_2;
extern DELPHI_PACKAGE int dContactApprox1;
extern DELPHI_PACKAGE int dxBodyFlagFiniteRotation;
extern DELPHI_PACKAGE int dxBodyFlagFiniteRotationAxis;
extern DELPHI_PACKAGE int dxBodyDisabled;
extern DELPHI_PACKAGE int dxBodyNoGravity;
static const System::Int8 _priv_dParamLoStop = System::Int8(0x0);
static const System::Word _priv_dParamLoStop2 = System::Word(0x100);
static const System::Word _priv_dParamLoStop3 = System::Word(0x200);
extern DELPHI_PACKAGE int dParamLoStop;
extern DELPHI_PACKAGE int dParamHiStop;
extern DELPHI_PACKAGE int dParamVel;
extern DELPHI_PACKAGE int dParamFMax;
extern DELPHI_PACKAGE int dParamFudgeFactor;
extern DELPHI_PACKAGE int dParamBounce;
extern DELPHI_PACKAGE int dParamCFM;
extern DELPHI_PACKAGE int dParamStopERP;
extern DELPHI_PACKAGE int dParamStopCFM;
extern DELPHI_PACKAGE int dParamSuspensionERP;
extern DELPHI_PACKAGE int dParamSuspensionCFM;
extern DELPHI_PACKAGE int dParamERP;
extern DELPHI_PACKAGE int dParamGroup1;
extern DELPHI_PACKAGE int dParamLoStop1;
extern DELPHI_PACKAGE int dParamHiStop1;
extern DELPHI_PACKAGE int dParamVel1;
extern DELPHI_PACKAGE int dParamFMax1;
extern DELPHI_PACKAGE int dParamFudgeFactor1;
extern DELPHI_PACKAGE int dParamBounce1;
extern DELPHI_PACKAGE int dParamCFM1;
extern DELPHI_PACKAGE int dParamStopERP1;
extern DELPHI_PACKAGE int dParamStopCFM1;
extern DELPHI_PACKAGE int dParamSuspensionERP1;
extern DELPHI_PACKAGE int dParamSuspensionCFM1;
extern DELPHI_PACKAGE int dParamERP1;
extern DELPHI_PACKAGE int dParamGroup2;
extern DELPHI_PACKAGE int dParamLoStop2;
extern DELPHI_PACKAGE int dParamHiStop2;
extern DELPHI_PACKAGE int dParamVel2;
extern DELPHI_PACKAGE int dParamFMax2;
extern DELPHI_PACKAGE int dParamFudgeFactor2;
extern DELPHI_PACKAGE int dParamBounce2;
extern DELPHI_PACKAGE int dParamCFM2;
extern DELPHI_PACKAGE int dParamStopERP2;
extern DELPHI_PACKAGE int dParamStopCFM2;
extern DELPHI_PACKAGE int dParamSuspensionERP2;
extern DELPHI_PACKAGE int dParamSuspensionCFM2;
extern DELPHI_PACKAGE int dParamERP2;
extern DELPHI_PACKAGE int dParamGroup3;
extern DELPHI_PACKAGE int dParamLoStop3;
extern DELPHI_PACKAGE int dParamHiStop3;
extern DELPHI_PACKAGE int dParamVel3;
extern DELPHI_PACKAGE int dParamFMax3;
extern DELPHI_PACKAGE int dParamFudgeFactor3;
extern DELPHI_PACKAGE int dParamBounce3;
extern DELPHI_PACKAGE int dParamCFM3;
extern DELPHI_PACKAGE int dParamStopERP3;
extern DELPHI_PACKAGE int dParamStopCFM3;
extern DELPHI_PACKAGE int dParamSuspensionERP3;
extern DELPHI_PACKAGE int dParamSuspensionCFM3;
extern DELPHI_PACKAGE int dParamERP3;
extern DELPHI_PACKAGE int dParamGroup;
extern "C" void __cdecl dInitODE(void);
extern "C" int __cdecl dInitODE2(unsigned uiInitFlags);
extern "C" void __cdecl dCloseODE(void);
extern "C" PdxWorld __cdecl dWorldCreate(void);
extern "C" void __cdecl dWorldDestroy(const PdxWorld World);
extern "C" float __cdecl dWorldGetCFM(const PdxWorld World);
extern "C" float __cdecl dWorldGetERP(const PdxWorld World);
extern "C" void __cdecl dWorldGetGravity(const PdxWorld World, TdVector3 &gravity);
extern "C" void __cdecl dWorldImpulseToForce(const PdxWorld World, const float stepsize, const float ix, const float iy, const float iz, TdVector3 &force);
extern "C" void __cdecl dWorldSetCFM(const PdxWorld World, float cfm);
extern "C" void __cdecl dWorldSetERP(const PdxWorld World, float erp);
extern "C" void __cdecl dWorldSetGravity(const PdxWorld World, const float x, const float y, const float z);
extern "C" void __cdecl dWorldSetContactMaxCorrectingVel(const PdxWorld World, const float vel);
extern "C" float __cdecl dWorldGetContactMaxCorrectingVel(const PdxWorld World);
extern "C" void __cdecl dWorldSetContactSurfaceLayer(const PdxWorld World, const float depth);
extern "C" float __cdecl dWorldGetContactSurfaceLayer(const PdxWorld World);
extern "C" void __cdecl dWorldExportDIF(const PdxWorld World, unsigned fileHandle, const char * world_name);
extern "C" float __cdecl dWorldGetLinearDampingThreshold(const PdxWorld World);
extern "C" void __cdecl dWorldSetLinearDampingThreshold(const PdxWorld World, const float threshold);
extern "C" float __cdecl dWorldGetAngularDampingThreshold(const PdxWorld World);
extern "C" void __cdecl dWorldSetAngularDampingThreshold(const PdxWorld World, const float threshold);
extern "C" float __cdecl dWorldGetLinearDamping(const PdxWorld World);
extern "C" void __cdecl dWorldSetLinearDamping(const PdxWorld World, const float scale);
extern "C" float __cdecl dWorldGetAngularDamping(const PdxWorld World);
extern "C" void __cdecl dWorldSetAngularDamping(const PdxWorld World, const float scale);
extern "C" void __cdecl dWorldSetDamping(const PdxWorld World, const float linear_scale, const float angular_scale);
extern "C" float __cdecl dWorldGetMaxAngularSpeed(const PdxWorld World);
extern "C" void __cdecl dWorldSetMaxAngularSpeed(const PdxWorld World, const float max_speed);
extern "C" void __cdecl dWorldStep(const PdxWorld World, const float stepsize);
extern "C" void __cdecl dWorldQuickStep(const PdxWorld World, const float stepsize);
extern "C" void __cdecl dWorldSetQuickStepNumIterations(const PdxWorld World, const int num);
extern "C" int __cdecl dWorldGetQuickStepNumIterations(const PdxWorld World);
extern "C" void __cdecl dWorldSetQuickStepW(const PdxWorld World, const float param);
extern "C" float __cdecl dWorldGetQuickStepW(const PdxWorld World);
extern "C" float __cdecl dWorldGetAutoDisableLinearAverageThreshold(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableLinearAverageThreshold(const PdxWorld World, float linear_average_threshold);
extern "C" float __cdecl dWorldGetAutoDisableAngularAverageThreshold(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableAngularAverageThreshold(const PdxWorld World, float linear_average_threshold);
extern "C" float __cdecl dWorldGetAutoDisableAverageSamplesCount(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableAverageSamplesCount(const PdxWorld World, float linear_average_threshold);
extern "C" float __cdecl dWorldGetAutoDisableLinearThreshold(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableLinearThreshold(const PdxWorld World, float linThreshold);
extern "C" float __cdecl dWorldGetAutoDisableAngularThreshold(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableAngularThreshold(const PdxWorld World, float angThreshold);
extern "C" int __cdecl dWorldGetAutoDisableSteps(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableSteps(const PdxWorld World, int steps);
extern "C" float __cdecl dWorldGetAutoDisableTime(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableTime(const PdxWorld World, float time);
extern "C" int __cdecl dWorldGetAutoDisableFlag(const PdxWorld World);
extern "C" void __cdecl dWorldSetAutoDisableFlag(const PdxWorld World, int do_auto_disable);
extern "C" void __cdecl dBodyAddForce(const PdxBody body, const float fx, const float fy, const float fz);
extern "C" void __cdecl dBodyAddForceAtPos(const PdxBody body, const float fx, const float fy, const float fz, const float px, const float py, const float pz);
extern "C" void __cdecl dBodyAddForceAtRelPos(const PdxBody body, const float fx, const float fy, const float fz, const float px, const float py, const float pz);
extern "C" void __cdecl dBodyAddRelForce(const PdxBody body, const float fx, const float fy, const float fz);
extern "C" void __cdecl dBodyAddRelForceAtPos(const PdxBody body, const float fx, const float fy, const float fz, const float px, const float py, const float pz);
extern "C" void __cdecl dBodyAddRelForceAtRelPos(const PdxBody body, const float fx, const float fy, const float fz, const float px, const float py, const float pz);
extern "C" void __cdecl dBodyAddRelTorque(const PdxBody body, const float fx, const float fy, const float fz);
extern "C" void __cdecl dBodyAddTorque(const PdxBody body, const float fx, const float fy, const float fz);
extern "C" PdxBody __cdecl dBodyCreate(const PdxWorld World);
extern "C" void __cdecl dBodyDestroy(const PdxBody body);
extern "C" void __cdecl dBodyDisable(const PdxBody body);
extern "C" void __cdecl dBodyEnable(const PdxBody body);
extern "C" PdVector3 __cdecl dBodyGetAngularVel(const PdxBody body);
extern "C" void __cdecl dBodyGetFiniteRotationAxis(const PdxBody body, TdVector3 &result);
extern "C" int __cdecl dBodyGetFiniteRotationMode(const PdxBody body);
extern "C" PdVector3 __cdecl dBodyGetForce(const PdxBody body);
extern "C" int __cdecl dBodyGetGravityMode(const PdxBody body);
extern "C" PdxJoint __cdecl dBodyGetJoint(const PdxBody body, const int index);
extern "C" PdVector3 __cdecl dBodyGetLinearVel(const PdxBody body);
extern "C" void __cdecl dBodyGetMass(const PdxBody body, TdMass &mass);
extern "C" int __cdecl dBodyGetNumJoints(const PdxBody body);
extern "C" void __cdecl dBodyGetPointVel(const PdxBody body, const float px, const float py, const float pz, TdVector3 &result);
extern "C" void __cdecl dBodyGetPosRelPoint(const PdxBody body, const float px, const float py, const float pz, TdVector3 &result);
extern "C" PdVector3 __cdecl dBodyGetPosition(const PdxBody body);
extern "C" PdQuaternion __cdecl dBodyGetQuaternion(const PdxBody body);
extern "C" void __cdecl dBodyGetRelPointPos(const PdxBody body, const float px, const float py, const float pz, TdVector3 &result);
extern "C" void __cdecl dBodyGetRelPointVel(const PdxBody body, const float px, const float py, const float pz, TdVector3 &result);
extern "C" PdMatrix3 __cdecl dBodyGetRotation(const PdxBody body);
extern "C" PdVector3 __cdecl dBodyGetTorque(const PdxBody body);
extern "C" int __cdecl dBodyIsEnabled(const PdxBody body);
extern "C" void __cdecl dBodySetAngularVel(const PdxBody body, const float x, const float y, const float z);
extern "C" void __cdecl dBodySetFiniteRotationAxis(const PdxBody body, const float x, const float y, const float z);
extern "C" void __cdecl dBodySetFiniteRotationMode(const PdxBody body, const int mode);
extern "C" void __cdecl dBodySetForce(const PdxBody body, const float x, const float y, const float z);
extern "C" void __cdecl dBodySetGravityMode(const PdxBody body, const int mode);
extern "C" void __cdecl dBodySetLinearVel(const PdxBody body, const float x, const float y, const float z);
extern "C" void __cdecl dBodySetMass(const PdxBody body, const PdMass mass);
extern "C" void __cdecl dBodySetPosition(const PdxBody body, const float x, const float y, const float z);
extern "C" void __cdecl dBodySetQuaternion(const PdxBody body, const TdVector4 &q);
extern "C" void __cdecl dBodySetRotation(const PdxBody body, const TdMatrix3 &R);
extern "C" void __cdecl dBodySetTorque(const PdxBody body, const float x, const float y, const float z);
extern "C" void __cdecl dBodyVectorFromWorld(const PdxBody body, const float px, const float py, const float pz, TdVector3 &result);
extern "C" void __cdecl dBodyVectorToWorld(const PdxBody body, const float px, const float py, const float pz, TdVector3 &result);
extern "C" void __cdecl dBodySetData(const PdxBody body, void * data);
extern "C" void * __cdecl dBodyGetData(const PdxBody body);
extern "C" void __cdecl dBodySetMovedCallback(const PdxBody body, TdMovedCallback callback);
extern "C" void __cdecl dBodyCopyPosition(const PdxBody body, const TdVector3 &pos);
extern "C" void __cdecl dBodyCopyRotation(const PdxBody body, const TdMatrix3 &R);
extern "C" void __cdecl dBodyCopyQuaternion(const PdxBody body, const TdVector4 &quat);
extern "C" void __cdecl dBodySetLinearDamping(const PdxBody body, float scale);
extern "C" float __cdecl dBodyGetLinearDamping(const PdxBody body);
extern "C" void __cdecl dBodySetAngularDamping(const PdxBody body, float scale);
extern "C" float __cdecl dBodyGetAngularDamping(const PdxBody body);
extern "C" void __cdecl dBodySetDamping(const PdxBody body, float linear_scale, float angular_scale);
extern "C" float __cdecl dBodyGetLinearDampingThreshold(const PdxBody body);
extern "C" void __cdecl dBodySetLinearDampingThreshold(const PdxBody body, float threshold);
extern "C" float __cdecl dBodyGetAngularDampingThreshold(const PdxBody body);
extern "C" void __cdecl dBodySetAngularDampingThreshold(const PdxBody body, float threshold);
extern "C" void __cdecl dBodySetDampingDefaults(const PdxBody body, float threshold);
extern "C" void __cdecl dBodySetMaxAngularSpeed(const PdxBody body, float max_speed);
extern "C" float __cdecl dBodyGetMaxAngularSpeed(const PdxBody body);
extern "C" float __cdecl dBodyGetAutoDisableLinearThreshold(const PdxBody body);
extern "C" void __cdecl dBodySetAutoDisableLinearThreshold(const PdxBody body, float linThreshold);
extern "C" float __cdecl dBodyGetAutoDisableAngularThreshold(const PdxBody body);
extern "C" void __cdecl dBodySetAutoDisableAngularThreshold(const PdxBody body, float angThreshold);
extern "C" int __cdecl dBodyGetAutoDisableSteps(const PdxBody body);
extern "C" void __cdecl dBodySetAutoDisableSteps(const PdxBody body, int steps);
extern "C" float __cdecl dBodyGetAutoDisableTime(const PdxBody body);
extern "C" void __cdecl dBodySetAutoDisableTime(const PdxBody body, float time);
extern "C" int __cdecl dBodyGetAutoDisableFlag(const PdxBody body);
extern "C" void __cdecl dBodySetAutoDisableFlag(const PdxBody body, int do_auto_disable);
extern "C" void __cdecl dBodySetAutoDisableDefaults(const PdxBody body);
extern "C" void __cdecl dBodySetAutoDisableAverageSamplesCount(const PdxBody body, unsigned average_samples_count);
extern "C" void __cdecl dJointGroupDestroy(const PdxJointGroup dJointGroupID);
extern "C" PdxJointGroup __cdecl dJointGroupCreate(const int max_size);
extern "C" void __cdecl dJointGroupEmpty(const PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointAttach(const PdxJoint dJointID, const PdxBody body1, const PdxBody body2);
extern "C" void __cdecl dJointDestroy(const PdxJoint dJointID);
extern "C" void * __cdecl dJointGetData(const PdxJoint dJointID);
extern "C" void __cdecl dJointSetData(const PdxJoint dJointID, void * data);
extern "C" void __cdecl dJointSetFeedback(const PdxJoint dJointID, PTdJointFeedback Feedback);
extern "C" PTdJointFeedback __cdecl dJointGetFeedback(const PdxJoint dJointID);
extern "C" int __cdecl dJointGetType(const PdxJoint dJointID);
extern "C" PdxBody __cdecl dJointGetBody(const PdxJoint dJointID, const int index);
extern "C" PdxJoint __cdecl dJointCreateContact(const PdxWorld World, PdxJointGroup dJointGroupID, const PdContact dContact);
extern "C" PdxJoint __cdecl dJointCreateAMotor(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetAMotorAngle(const PdxJoint dJointID, const int anum, const float angle);
extern "C" float __cdecl dJointGetAMotorAngle(const PdxJoint dJointID, const int anum);
extern "C" void __cdecl dJointSetAMotorAxis(const PdxJoint dJointID, const int anum, const int rel, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetAMotorAxis(const PdxJoint dJointID, const int anum, TdVector3 &result);
extern "C" void __cdecl dJointSetAMotorNumAxes(const PdxJoint dJointID, const int num);
extern "C" int __cdecl dJointGetAMotorNumAxes(const PdxJoint dJointID);
extern "C" void __cdecl dJointSetAMotorParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetAMotorParam(const PdxJoint dJointID, const int parameter);
extern "C" void __cdecl dJointSetAMotorMode(const PdxJoint dJointID, const TdAngularMotorModeNumbers mode);
extern "C" int __cdecl dJointGetAMotorMode(const PdxJoint dJointID);
extern "C" void __cdecl dJointAddAMotorTorques(const PdxJoint dJointID, float torque1, float torque2, float torque3);
extern "C" float __cdecl dJointGetAMotorAngleRate(const PdxJoint dJointID, const int anum);
extern "C" int __cdecl dJointGetAMotorAxisRel(const PdxJoint dJointID, const int anum);
extern "C" PdxJoint __cdecl dJointCreateLMotor(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetLMotorAxis(const PdxJoint dJointID, const int anum, const int rel, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetLMotorAxis(const PdxJoint dJointID, const int anum, TdVector3 &result);
extern "C" void __cdecl dJointSetLMotorNumAxes(const PdxJoint dJointID, const int num);
extern "C" int __cdecl dJointGetLMotorNumAxes(const PdxJoint dJointID);
extern "C" void __cdecl dJointSetLMotorParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetLMotorParam(const PdxJoint dJointID, const int parameter);
extern "C" PdxJoint __cdecl dJointCreateBall(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetBallAnchor(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetBallAnchor(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointGetBallAnchor2(const PdxJoint dJointID, TdVector3 &result);
extern "C" PdxJoint __cdecl dJointCreateHinge(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetHingeAnchor(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetHingeAnchor(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointGetHingeAnchor2(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetHingeAxis(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetHingeAxis(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetHingeParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetHingeParam(const PdxJoint dJointID, const int parameter);
extern "C" float __cdecl dJointGetHingeAngle(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetHingeAngleRate(const PdxJoint dJointID);
extern "C" void __cdecl dJointAddHingeTorque(const PdxJoint dJointID, float torque);
extern "C" PdxJoint __cdecl dJointCreateHinge2(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetHinge2Anchor(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetHinge2Anchor(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointGetHinge2Anchor2(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetHinge2Axis1(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetHinge2Axis1(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetHinge2Axis2(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetHinge2Axis2(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetHinge2Param(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetHinge2Param(const PdxJoint dJointID, const int parameter);
extern "C" float __cdecl dJointGetHinge2Angle1(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetHinge2Angle1Rate(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetHinge2Angle2Rate(const PdxJoint dJointID);
extern "C" void __cdecl dJointAddHinge2Torques(const PdxJoint dJointID, float torque1, float torque2);
extern "C" void __cdecl dJointCorrectHinge2(const PdxJoint dJointID);
extern "C" PdxJoint __cdecl dJointCreateSlider(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetSliderAxis(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetSliderAxis(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetSliderParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetSliderParam(const PdxJoint dJointID, const int parameter);
extern "C" float __cdecl dJointGetSliderPosition(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetSliderPositionRate(const PdxJoint dJointID);
extern "C" void __cdecl dJointAddSliderForce(const PdxJoint dJointID, float force);
extern "C" PdxJoint __cdecl dJointCreateUniversal(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointGetUniversalAnchor(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointGetUniversalAnchor2(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetUniversalAxis1(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetUniversalAxis1(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetUniversalAxis2(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetUniversalAxis2(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetUniversalParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetUniversalParam(const PdxJoint dJointID, const int parameter);
extern "C" float __cdecl dJointGetUniversalAngle1(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetUniversalAngle2(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetUniversalAngle1Rate(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetUniversalAngle2Rate(const PdxJoint dJointID);
extern "C" void __cdecl dJointSetUniversalAnchor(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointAddUniversalTorques(const PdxJoint dJointID, float torque1, float torque2);
extern "C" PdxJoint __cdecl dJointCreateFixed(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetFixed(const PdxJoint dJointID);
extern "C" PdxJoint __cdecl dJointCreatePlane2D(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetPlane2DXParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" void __cdecl dJointSetPlane2DYParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" void __cdecl dJointSetPlane2DAngleParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" PdxJoint __cdecl dJointCreatePR(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetPRAnchor(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointSetPRAxis1(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetPRAxis1(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetPRAxis2(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetPRAxis2(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetPRParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetPRParam(const PdxJoint dJointID, int parameter);
extern "C" void __cdecl dJointAddPRTorque(const PdxJoint dJointID, float torque);
extern "C" PdxJoint __cdecl dJointCreatePiston(const PdxWorld World, PdxJointGroup dJointGroupID);
extern "C" void __cdecl dJointSetPistonAnchor(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetPistonAnchor(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointGetPistonAnchor2(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetPistonAxis(const PdxJoint dJointID, const float x, const float y, const float z);
extern "C" void __cdecl dJointGetPistonAxis(const PdxJoint dJointID, TdVector3 &result);
extern "C" void __cdecl dJointSetPistonParam(const PdxJoint dJointID, const int parameter, const float Value);
extern "C" float __cdecl dJointGetPistonParam(const PdxJoint dJointID, int parameter);
extern "C" void __cdecl dJointSetPistonAxisDelta(const PdxJoint dJointID, const float x, const float y, const float z, const float ax, const float ay, const float az);
extern "C" void __cdecl dJointAddPistonForce(const PdxJoint dJointID, float force);
extern "C" float __cdecl dJointGetPistonPosition(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetPistonAngle(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetPistonAngleRate(const PdxJoint dJointID);
extern "C" float __cdecl dJointGetPistonRate(const PdxJoint dJointID);
extern "C" PdxGeom __cdecl dCreateGeom(int classnum);
extern "C" void __cdecl dGeomDestroy(const PdxGeom geom);
extern "C" int __cdecl dCreateGeomClass(const TdGeomClass &classptr);
extern "C" int __cdecl dGeomGetClass(const PdxGeom geom);
extern "C" void * __cdecl dGeomGetClassData(PdxGeom o);
extern "C" PdxSpace __cdecl dGeomGetSpace(const PdxGeom geom);
extern "C" int __cdecl dGeomIsSpace(const PdxGeom geom);
extern "C" void __cdecl dGeomSetBody(const PdxGeom geom, PdxBody body);
extern "C" PdxBody __cdecl dGeomGetBody(const PdxGeom geom);
extern "C" void __cdecl dGeomSetPosition(const PdxGeom geom, const float x, const float y, const float z);
extern "C" PdVector3 __cdecl dGeomGetPosition(const PdxGeom geom);
extern "C" void __cdecl dGeomSetRotation(const PdxGeom geom, const TdMatrix3 &R);
extern "C" PdMatrix3 __cdecl dGeomGetRotation(const PdxGeom geom);
extern "C" void __cdecl dGeomSetQuaternion(const PdxGeom geom, const void *TdQuaternion);
extern "C" void __cdecl dGeomGetQuaternion(const PdxGeom geom, TdVector4 &result);
extern "C" void __cdecl dGeomCopyPosition(const PdxGeom geom, const TdVector3 &pos);
extern "C" void __cdecl dGeomCopyRotation(const PdxGeom geom, const TdMatrix3 &R);
extern "C" void __cdecl dGeomCopyQuaternion(const PdxGeom geom, const TdVector4 &quat);
extern "C" void __cdecl dGeomSetData(const PdxGeom geom, void * data);
extern "C" void * __cdecl dGeomGetData(const PdxGeom geom);
extern "C" void __cdecl dGeomEnable(const PdxGeom geom);
extern "C" void __cdecl dGeomDisable(const PdxGeom geom);
extern "C" int __cdecl dGeomIsEnabled(const PdxGeom geom);
extern "C" void __cdecl dGeomGetAABB(const PdxGeom geom, TdAABB &aabb);
extern "C" void __cdecl dGeomSetCategoryBits(const PdxGeom geom, unsigned bits);
extern "C" unsigned __cdecl dGeomGetCategoryBits(const PdxGeom geom);
extern "C" void __cdecl dGeomSetCollideBits(const PdxGeom geom, unsigned bits);
extern "C" unsigned __cdecl dGeomGetCollideBits(const PdxGeom geom);
extern "C" void __cdecl dGeomSetOffsetPosition(const PdxGeom geom, float x, float y, float z);
extern "C" PdVector3 __cdecl dGeomGetOffsetPosition(const PdxGeom geom);
extern "C" void __cdecl dGeomSetOffsetRotation(const PdxGeom geom, const TdMatrix3 &R);
extern "C" PdVector3 __cdecl dGeomGetOffsetRotation(const PdxGeom geom);
extern "C" void __cdecl dGeomSetOffsetQuaternion(const PdxGeom geom, const TdVector4 &q);
extern "C" void __cdecl dGeomGetOffsetQuaternion(const PdxGeom geom, TdVector4 &q);
extern "C" void __cdecl dGeomClearOffset(const PdxGeom geom);
extern "C" void __cdecl dGeomSetOffsetWorldPosition(const PdxGeom geom, float x, float y, float z);
extern "C" void __cdecl dGeomSetOffsetWorldRotation(const PdxGeom geom, const TdMatrix3 &R);
extern "C" void __cdecl dGeomSetOffsetWorldQuaternion(const PdxGeom geom, const TdVector4 &q);
extern "C" void __cdecl dGeomCopyOffsetPosition(const PdxGeom geom, TdVector3 &pos);
extern "C" void __cdecl dGeomCopyOffsetRotation(const PdxGeom geom, TdMatrix3 &R);
extern "C" void __cdecl dGeomIsOffset(const PdxGeom geom);
extern "C" PdxGeom __cdecl dCreateGeomTransform(const PdxSpace Space);
extern "C" void __cdecl dGeomTransformSetGeom(const PdxGeom geom, const PdxGeom obj);
extern "C" PdxGeom __cdecl dGeomTransformGetGeom(const PdxGeom geom);
extern "C" void __cdecl dGeomTransformSetInfo(const PdxGeom geom, int mode);
extern "C" int __cdecl dGeomTransformGetInfo(const PdxGeom geom);
extern "C" void __cdecl dGeomTransformSetCleanup(const PdxGeom geom, const int mode);
extern "C" int __cdecl dGeomTransformGetCleanup(const PdxGeom geom);
extern "C" PdxGeom __cdecl dCreateBox(const PdxSpace Space, const float lx, const float ly, const float lz);
extern "C" void __cdecl dGeomBoxGetLengths(const PdxGeom geom, TdVector3 &result);
extern "C" void __cdecl dGeomBoxSetLengths(const PdxGeom geom, const float lx, const float ly, const float lz);
extern "C" float __cdecl dGeomBoxPointDepth(const PdxGeom geom, const float x, const float y, const float z);
extern "C" PdxGeom __cdecl dCreateCylinder(const PdxSpace Space, float R, float lz);
extern "C" void __cdecl dGeomCylinderSetParams(const PdxGeom geom, float radius, float length);
extern "C" void __cdecl dGeomCylinderGetParams(const PdxGeom geom, float &radius, float &length);
extern "C" PdxGeom __cdecl dCreateCapsule(const PdxSpace Space, const float radius, const float length);
extern "C" void __cdecl dGeomCapsuleSetParams(const PdxGeom geom, const float radius, const float length);
extern "C" void __cdecl dGeomCapsuleGetParams(const PdxGeom geom, float &radius, float &length);
extern "C" float __cdecl dGeomCapsulePointDepth(const PdxGeom geom, const float x, const float y, const float z);
extern "C" PdxGeom __cdecl dCreatePlane(const PdxSpace Space, const float a, const float b, const float c, const float d);
extern "C" void __cdecl dGeomPlaneSetParams(const PdxGeom geom, const float a, const float b, const float c, const float d);
extern "C" void __cdecl dGeomPlaneGetParams(const PdxGeom geom, TdVector4 &result);
extern "C" float __cdecl dGeomPlanePointDepth(const PdxGeom geom, const float x, const float y, const float z);
extern "C" PdxGeom __cdecl dCreateSphere(const PdxSpace Space, const float radius);
extern "C" void __cdecl dGeomSphereSetRadius(const PdxGeom geom, const float radius);
extern "C" float __cdecl dGeomSphereGetRadius(const PdxGeom geom);
extern "C" float __cdecl dGeomSpherePointDepth(const PdxGeom geom, const float x, const float y, const float z);
extern "C" PdxGeom __cdecl dCreateConvex(const PdxSpace Space, PdReal _planes, unsigned _planecount, PdReal _points, unsigned _pointcount, const unsigned _polygons);
extern "C" void __cdecl dGeomSetConvex(const PdxGeom geom, PdReal _planes, unsigned _planecount, PdReal _points, unsigned _pointcount, const unsigned _polygons);
extern "C" PdxGeom __cdecl dCreateHeightfield(const PdxSpace Space, PdxHeightfieldData data, int bPlaceable);
extern "C" PdxHeightfieldData __cdecl dGeomHeightfieldDataCreate(void);
extern "C" void __cdecl dGeomHeightfieldDataDestroy(PdxHeightfieldData data);
extern "C" void __cdecl dGeomHeightfieldSetHeightfieldData(const PdxGeom geom, PdxHeightfieldData data);
extern "C" PdxHeightfieldData __cdecl dGeomHeightfieldGetHeightfieldData(const PdxGeom geom);
extern "C" float __cdecl dGeomHeightfieldDataSetBounds(PdxHeightfieldData data, float minHeight, float MaxHeight);
extern "C" PdxGeom __cdecl dCreateRay(const PdxSpace Space, float length);
extern "C" void __cdecl dGeomRaySet(const PdxGeom geom, float px, float py, float pz, float dx, float dy, float dz);
extern "C" void __cdecl dGeomRayGet(const PdxGeom geom, TdVector3 &start, TdVector3 &dir);
extern "C" void __cdecl dGeomRaySetLength(const PdxGeom geom, float length);
extern "C" float __cdecl dGeomRayGetLength(const PdxGeom geom);
extern "C" void __cdecl dGeomRaySetParams(const PdxGeom geom, int FirstContact, int BackfacCull);
extern "C" void __cdecl dGeomRayGetParams(const PdxGeom geom, int &FirstContact, int &BackfacCull);
extern "C" void __cdecl dGeomRaySetClosestHit(const PdxGeom geom, int closestHit);
extern "C" int __cdecl dGeomRayGetClosestHit(const PdxGeom geom);
extern "C" PdxGeom __cdecl dCreateTriMesh(const PdxSpace Space, PdxTriMeshData data, TdTriCallback callback, TdTriArrayCallback ArrayCallback, TdTriRayCallback RayCallback);
extern "C" void __cdecl dGeomTriMeshSetData(PdxGeom g, PdxTriMeshData data);
extern "C" PdxTriMeshData __cdecl dGeomTriMeshGetData(PdxGeom g);
extern "C" PdxTriMeshData __cdecl dGeomTriMeshGetTriMeshDataID(PdxGeom g);
extern "C" void __cdecl dGeomTriMeshDataUpdate(PdxTriMeshData g);
extern "C" int __cdecl dGeomTriMeshIsTCEnabled(PdxGeom g, int geomClass);
extern "C" void __cdecl dGeomTriMeshEnableTC(PdxGeom g, int geomClass, int enable);
extern "C" void __cdecl dGeomTriMeshClearTCCache(PdxGeom g);
extern "C" int __cdecl dGeomTriMeshGetTriangleCount(PdxGeom g);
extern "C" void __cdecl dGeomTriMeshGetTriangle(PdxGeom g, int index, PdVector3 v0, PdVector3 v1, PdVector3 v2);
extern "C" void __cdecl dGeomTriMeshGetPoint(PdxGeom g, int index, float u, float v, const TdVector3 &result);
extern "C" void * __cdecl dGeomTriMeshGetArrayCallback(PdxGeom g);
extern "C" void * __cdecl dGeomTriMeshGetRayCallback(PdxGeom g);
extern "C" void __cdecl dGeomTriMeshSetArrayCallback(PdxGeom g, void * ArrayCallback);
extern "C" void __cdecl dGeomTriMeshSetRayCallback(PdxGeom g, void * RayCallback);
extern "C" void __cdecl dGeomTriMeshSetCallback(PdxGeom g, void * callback);
extern "C" void * __cdecl dGeomTriMeshGetCallback(PdxGeom g);
extern "C" void __cdecl dGeomTriMeshDataDestroy(PdxTriMeshData g);
extern "C" PdxTriMeshData __cdecl dGeomTriMeshDataCreate(void);
extern "C" void __cdecl dGeomTriMeshDataSet(PdxTriMeshData g, int data_id, void * data);
extern "C" void __cdecl dGeomTriMeshDataBuildSimple(PdxTriMeshData g, PdVector3Array Vertices, int VertexCount, PdIntegerArray Indices, int IndexCount);
extern "C" void __cdecl dGeomTriMeshDataBuildSimple1(PdxTriMeshData g, PdVector3Array Vertices, int VertexCount, PdIntegerArray Indices, int IndexCount, PdVector3Array Normals);
extern "C" void __cdecl dGeomTriMeshDataBuildDouble(PdxTriMeshData g, PdVector3Array Vertices, int VertexStride, int VertexCount, PdIntegerArray Indices, int IndexCount, int TriStride);
extern "C" void __cdecl dGeomTriMeshDataBuildDouble1(PdxTriMeshData g, PdVector3Array Vertices, int VertexStride, int VertexCount, PdIntegerArray Indices, int IndexCount, int TriStride, PdVector3Array Normals);
extern "C" void __cdecl dGeomTriMeshDataBuildSingle(PdxTriMeshData g, PdVector3Array Vertices, int VertexStride, int VertexCount, PdIntegerArray Indices, int IndexCount, int TriStride);
extern "C" void __cdecl dGeomTriMeshDataBuildSingle1(PdxTriMeshData g, PdVector3Array Vertices, int VertexStride, int VertexCount, PdIntegerArray Indices, int IndexCount, int TriStride, PdVector3Array Normals);
extern "C" void __cdecl dInfiniteAABB(PdxGeom geom, TdAABB &aabb);
extern "C" void __cdecl dSpaceDestroy(const PdxSpace Space);
extern "C" PdxSpace __cdecl dSimpleSpaceCreate(PdxSpace Space);
extern "C" PdxSpace __cdecl dHashSpaceCreate(PdxSpace Space);
extern "C" PdxSpace __cdecl dQuadTreeSpaceCreate(const PdxSpace Space, const TdVector3 &Center, const TdVector3 &Extents, const int depth);
extern "C" void __cdecl dSpaceAdd(const PdxSpace Space, const PdxGeom geom);
extern "C" void __cdecl dSpaceRemove(const PdxSpace Space, const PdxGeom geom);
extern "C" void __cdecl dSpaceClean(const PdxSpace Space);
extern "C" int __cdecl dSpaceQuery(const PdxSpace Space, const PdxGeom geom);
extern "C" int __cdecl dSpaceGetNumGeoms(const PdxSpace Space);
extern "C" PdxGeom __cdecl dSpaceGetGeom(const PdxSpace Space, const int I);
extern "C" void __cdecl dHashSpaceSetLevels(const PdxSpace Space, const int minlevel, const int maxlevel);
extern "C" void __cdecl dHashSpaceGetLevels(const PdxSpace Space, int &minlevel, int &maxlevel);
extern "C" void __cdecl dSpaceSetCleanup(PdxSpace Space, const int mode);
extern "C" int __cdecl dSpaceGetCleanup(PdxSpace Space);
extern "C" int __cdecl dCollide(PdxGeom o1, PdxGeom o2, int flags, TdContactGeom &contact, int skip);
extern "C" void __cdecl dSpaceCollide(const PdxSpace Space, void * data, TdNearCallback callback);
extern "C" void __cdecl dSpaceCollide2(PdxGeom o1, PdxGeom o2, void * data, TdNearCallback callback);
extern "C" void __cdecl dMassSetParameters(TdMass &m, float themass, float cgx, float cgy, float cgz, float I11, float I22, float I33, float I12, float I13, float I23);
extern "C" void __cdecl dMassAdd(TdMass &a, TdMass &b);
extern "C" void __cdecl dMassAdjust(TdMass &m, float newmass);
extern "C" void __cdecl dMassTranslate(TdMass &m, float x, float y, float z);
extern "C" void __cdecl dMassRotate(TdMass &m, TdMatrix3 &R);
extern "C" void __cdecl dMassSetZero(TdMass &m);
extern "C" void __cdecl dMassSetBox(TdMass &m, float density, float lx, float ly, float lz);
extern "C" void __cdecl dMassSetBoxTotal(TdMass &m, float total_mass, float lx, float ly, float lz);
extern "C" void __cdecl dMassSetCylinder(TdMass &m, float density, int direction, float radius, float length);
extern "C" void __cdecl dMassSetCylinderTotal(TdMass &m, float total_mass, int direction, float radius, float length);
extern "C" void __cdecl dMassSetCapsule(TdMass &m, float density, int direction, float radius, float length);
extern "C" void __cdecl dMassSetCapsuleTotal(TdMass &m, float total_mass, int direction, float radius, float length);
extern "C" void __cdecl dMassSetSphere(TdMass &m, float density, float radius);
extern "C" void __cdecl dMassSetSphereTotal(TdMass &m, float total_mass, float radius);
extern "C" void __cdecl dMassSetTrimesh(TdMass &m, float density, PdxGeom TriMesh);
extern "C" void __cdecl dMassSetTrimeshTotal(TdMass &m, float total_mass, PdxGeom TriMesh);
extern "C" void __cdecl dQFromAxisAndAngle(TdVector4 &q, const float ax, const float ay, const float az, const float angle);
extern "C" void __cdecl dRFromAxisAndAngle(TdMatrix3 &R, const float ax, const float ay, const float az, const float angle);
extern "C" void __cdecl dRSetIdentity(TdMatrix3 &R);
extern "C" void __cdecl dQSetIdentity(TdVector4 &q);
extern "C" void __cdecl dRFromEulerAngles(TdMatrix3 &R, const float phi, const float theta, const float psi);
extern "C" void __cdecl dRFrom2Axes(TdMatrix3 &R, const float ax, const float ay, const float az, const float bx, const float by, const float bz);
extern "C" void __cdecl dRFromZAxis(TdMatrix3 &R, const float ax, const float ay, const float az);
extern "C" void __cdecl dMultiply0(const PdReal a, const PdReal b, const PdReal c, int p, int q, int R);
extern "C" void __cdecl dMultiply1(const PdReal a, const PdReal b, const PdReal c, int p, int q, int R);
extern "C" void __cdecl dMultiply2(const PdReal a, const PdReal b, const PdReal c, int p, int q, int R);
extern "C" void __cdecl dQMultiply0(TdVector4 &qa, const TdVector4 &qb, const TdVector4 &qc);
extern "C" void __cdecl dQMultiply1(TdVector4 &qa, const TdVector4 &qb, const TdVector4 &qc);
extern "C" void __cdecl dQMultiply2(TdVector4 &qa, const TdVector4 &qb, const TdVector4 &qc);
extern "C" void __cdecl dQMultiply3(TdVector4 &qa, const TdVector4 &qb, const TdVector4 &qc);
extern "C" void __cdecl dRfromQ(TdMatrix3 &R, const TdVector4 &q);
extern "C" void __cdecl dQfromR(TdVector4 &q, const TdMatrix3 &R);
extern "C" void __cdecl dDQfromW(TdVector4 &dq, const TdVector3 &w, const TdVector4 &q);
extern "C" void __cdecl dNormalize3(TdVector3 &a);
extern "C" void __cdecl dNormalize4(TdVector4 &a);
extern "C" void __cdecl dClosestLineSegmentPoints(const TdVector3 &a1, const TdVector3 &a2, const TdVector3 &b1, const TdVector3 &b2, TdVector3 &cp1, TdVector3 &cp2);
extern "C" int __cdecl dBoxTouchesBox(const TdVector3 &_p1, const TdMatrix3 &R1, const TdVector3 &side1, const TdVector3 &_p2, const TdMatrix3 &R2, const TdVector3 &side2);
extern "C" float __cdecl dMaxDifference(PdReal a, PdReal b, int n, int m);
extern "C" void __cdecl dMakeRandomVector(TdVector3 &n1, int a, float f);
extern "C" int __cdecl dAreConnected(PdxBody a, PdxBody b);
extern "C" int __cdecl dAreConnectedExcluding(PdxBody a, PdxBody b, TdJointTypeNumbers joint_type);
extern "C" void __cdecl dMakeRandomMatrix(PdRealArray a, int n, int m, float range);
extern "C" void __cdecl dClearUpperTriangle(PdRealArray a, int n);
extern "C" unsigned __cdecl dRandGetSeed(void);
extern "C" void __cdecl dRandSetSeed(const unsigned s);
extern "C" int __cdecl dRandInt(const int n);
extern "C" float __cdecl dRandReal(void);
extern "C" int __cdecl dTestRand(void);
extern "C" void __cdecl dTestMatrixComparison(void);
extern "C" void __cdecl dTestSolveLCP(void);
static const System::Int8 MaxUserClasses = System::Int8(0x4);
extern DELPHI_PACKAGE int dSphereClass;
extern DELPHI_PACKAGE int dBoxClass;
extern DELPHI_PACKAGE int dCapsuleClass;
extern DELPHI_PACKAGE int dCylinderClass;
extern DELPHI_PACKAGE int dPlaneClass;
extern DELPHI_PACKAGE int dRayClass;
extern DELPHI_PACKAGE int dConvexClass;
extern DELPHI_PACKAGE int dGeomTransformClass;
extern DELPHI_PACKAGE int dTriMeshClass;
extern DELPHI_PACKAGE int dHeightFieldClass;
extern DELPHI_PACKAGE int dFirstSpaceClass;
extern DELPHI_PACKAGE int dSimpleSpaceClass;
extern DELPHI_PACKAGE int dHashSpaceClass;
extern DELPHI_PACKAGE int dSweepAndPruneSpaceClass;
extern DELPHI_PACKAGE int dQuadTreeSpaceClass;
extern DELPHI_PACKAGE int dLastSpaceClass;
extern DELPHI_PACKAGE int dFirstUserClass;
extern DELPHI_PACKAGE int dLastUserClass;
extern DELPHI_PACKAGE int dGeomNumClasses;
extern DELPHI_PACKAGE bool IsODEInitialized;
extern DELPHI_PACKAGE bool DisabledDebugGeom;
extern DELPHI_PACKAGE bool DisabledDebugCollision;
extern DELPHI_PACKAGE float __fastcall dDot(const PdVector3 a, const PdVector3 b)/* overload */;
extern DELPHI_PACKAGE float __fastcall dDot(const TdVector3 &a, const TdVector3 &b)/* overload */;
extern DELPHI_PACKAGE float __fastcall dDOT14(const TdRealArray &a, const TdRealArray &b)/* overload */;
extern DELPHI_PACKAGE float __fastcall dDOT14(const PdRealArray a, const PdRealArray b)/* overload */;
extern DELPHI_PACKAGE void __fastcall dMULTIPLY0_331(TdVector3 &a, const TdMatrix3 &b, const TdVector3 &c);
extern DELPHI_PACKAGE void __fastcall dMULTIPLY0_333(TdMatrix3 &a, const TdMatrix3 &b, const TdMatrix3 &c);
extern DELPHI_PACKAGE TdVector3 __fastcall Vector3ScalarMul(const TdVector3 &a, const float Scalar);
extern DELPHI_PACKAGE TdVector3 __fastcall Vector3ADD(const TdVector3 &a, const TdVector3 &b);
extern DELPHI_PACKAGE TdVector3 __fastcall Vector3SUB(const TdVector3 &a, const TdVector3 &b);
extern DELPHI_PACKAGE float __fastcall Vector3Length(const TdVector3 &a);
extern DELPHI_PACKAGE TdVector3 __fastcall Vector3Cross(const TdVector3 &v1, const TdVector3 &v2);
extern DELPHI_PACKAGE TdVector3 __fastcall Vector3Make(const float x, const float y, const float z);
extern DELPHI_PACKAGE void __fastcall VerifyDelphiODE(PdxBody body, PdxGeom geom);
extern DELPHI_PACKAGE bool __fastcall InitODE(System::WideChar * ADllName);
extern DELPHI_PACKAGE void __fastcall CloseODE(void);
}	/* namespace Odeimport */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_ODEIMPORT)
using namespace Physics::Odeimport;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_OdeimportHPP
