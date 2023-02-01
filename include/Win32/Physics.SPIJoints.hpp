// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.SPIJoints.pas' rev: 35.00 (Windows)

#ifndef Physics_SpijointsHPP
#define Physics_SpijointsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Spijoints
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBaseJoint;
class DELPHICLASS TGLBaseLink;
class DELPHICLASS TGLJoint;
class DELPHICLASS TGLLink;
class DELPHICLASS TGLType1Link;
class DELPHICLASS TGLType2Link;
class DELPHICLASS TGLType3Link;
class DELPHICLASS TGLType4Link;
class DELPHICLASS TGLType5Link;
class DELPHICLASS TGLType6Link;
class DELPHICLASS TGLType7Link;
class DELPHICLASS TGLType8Link;
class DELPHICLASS TGLPrismaticJoint;
class DELPHICLASS TGLRevoluteJoint;
class DELPHICLASS TGLBallAndSocketJoint;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseJoint : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLBaseJoint() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseJoint() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseLink : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLBaseLink() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseLink() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLJoint : public TGLBaseJoint
{
	typedef TGLBaseJoint inherited;
	
public:
	TGLBaseLink* Link1;
	TGLBaseLink* Link2;
public:
	/* TObject.Create */ inline __fastcall TGLJoint() : TGLBaseJoint() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLJoint() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLLink : public TGLBaseLink
{
	typedef TGLBaseLink inherited;
	
public:
	double fLinkLength;
	double fTwistAngle;
	double fLinkAngle;
	double fLinkDistance;
	Gls::Vectortypes::TMatrix4f A;
	__fastcall TGLLink(double LinkLength, double TwistAngle, double LinkAngle, double LinkDistance);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLLink() { }
	
};


class PASCALIMPLEMENTATION TGLType1Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	double Angle;
	__fastcall TGLType1Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType1Link() { }
	
};


class PASCALIMPLEMENTATION TGLType2Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	double Angle;
	__fastcall TGLType2Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType2Link() { }
	
};


class PASCALIMPLEMENTATION TGLType3Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	double Angle;
	__fastcall TGLType3Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType3Link() { }
	
};


class PASCALIMPLEMENTATION TGLType4Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	double Angle;
	__fastcall TGLType4Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType4Link() { }
	
};


class PASCALIMPLEMENTATION TGLType5Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	__fastcall TGLType5Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType5Link() { }
	
};


class PASCALIMPLEMENTATION TGLType6Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	double Angle;
	__fastcall TGLType6Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType6Link() { }
	
};


class PASCALIMPLEMENTATION TGLType7Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	double Angle;
	__fastcall TGLType7Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType7Link() { }
	
};


class PASCALIMPLEMENTATION TGLType8Link : public TGLLink
{
	typedef TGLLink inherited;
	
public:
	double Length;
	__fastcall TGLType8Link(double Length, double Angle);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLType8Link() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPrismaticJoint : public TGLJoint
{
	typedef TGLJoint inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLPrismaticJoint() : TGLJoint() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLPrismaticJoint() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLRevoluteJoint : public TGLJoint
{
	typedef TGLJoint inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLRevoluteJoint() : TGLJoint() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLRevoluteJoint() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBallAndSocketJoint : public TGLJoint
{
	typedef TGLJoint inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLBallAndSocketJoint() : TGLJoint() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBallAndSocketJoint() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Spijoints */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_SPIJOINTS)
using namespace Physics::Spijoints;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_SpijointsHPP
