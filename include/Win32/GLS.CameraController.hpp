// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.CameraController.pas' rev: 35.00 (Windows)

#ifndef Gls_CameracontrollerHPP
#define Gls_CameracontrollerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <System.Contnrs.hpp>
#include <System.Types.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.SmoothNavigator.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Cameracontroller
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLCameraController;
class DELPHICLASS TGLCameraJobList;
class DELPHICLASS TGLCameraJob;
class DELPHICLASS TGLMoveToPosJob;
class DELPHICLASS TGLZoomToDistanceJob;
class DELPHICLASS TGLOrbitToPosJob;
class DELPHICLASS TGLSmoothOrbitToPos;
class DELPHICLASS TGLOrbitToPosAdvJob;
class DELPHICLASS TGLSmoothOrbitToPosAdvJob;
class DELPHICLASS TGLCameraController;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLCameraController : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLCameraController(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLCameraController(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCameraController(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCameraController(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCameraController(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLCameraController() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCameraJobList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	TGLCameraJob* operator[](const int AIndex) { return this->Items[AIndex]; }
	
private:
	TGLCameraController* FController;
	TGLCameraJob* __fastcall GetCameraJob(const int AIndex);
	void __fastcall SetCameraJob(const int AIndex, TGLCameraJob* const Value);
	
public:
	__fastcall TGLCameraJobList(TGLCameraController* AController);
	HIDESBASE int __fastcall Add(TGLCameraJob* ACameraJob);
	__property TGLCameraJob* Items[const int AIndex] = {read=GetCameraJob, write=SetCameraJob/*, default*/};
	HIDESBASE TGLCameraJob* __fastcall First();
	HIDESBASE TGLCameraJob* __fastcall Last();
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLCameraJobList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLCameraJob : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLCameraJobList* FJoblist;
	
protected:
	bool FAbort;
	bool FInit;
	bool FRunning;
	double FElapsedTime;
	double FDeltaTime;
	double FStartTime;
	double FProceedTime;
	
public:
	__fastcall virtual TGLCameraJob(TGLCameraJobList* const AJoblist);
	__fastcall virtual ~TGLCameraJob();
	void __fastcall Abort();
	virtual void __fastcall Step() = 0 ;
	virtual void __fastcall Init() = 0 ;
	__property bool Running = {read=FRunning, write=FRunning, nodefault};
	__property double ElapsedTime = {read=FElapsedTime, write=FElapsedTime};
	__property double StartTime = {read=FStartTime, write=FStartTime};
	__property double ProceedTime = {read=FProceedTime, write=FProceedTime};
};


class PASCALIMPLEMENTATION TGLMoveToPosJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Gls::Vectortypes::TVector4f FInitialPos;
	Gls::Vectortypes::TVector4f FFinalPos;
	
public:
	double X;
	double Y;
	double Z;
	double Time;
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Gls::Vectortypes::TVector4f InitialPos = {read=FInitialPos};
	__property Gls::Vectortypes::TVector4f FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLMoveToPosJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLMoveToPosJob() { }
	
};


class PASCALIMPLEMENTATION TGLZoomToDistanceJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Gls::Vectortypes::TVector4f FInitialPos;
	Gls::Vectortypes::TVector4f FFinalPos;
	
public:
	double Distance;
	double Time;
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Gls::Vectortypes::TVector4f InitialPos = {read=FInitialPos};
	__property Gls::Vectortypes::TVector4f FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLZoomToDistanceJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLZoomToDistanceJob() { }
	
};


class PASCALIMPLEMENTATION TGLOrbitToPosJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Gls::Vectortypes::TVector4f FFinalPos;
	Gls::Vectortypes::TVector2f FRotateSpeed;
	Gls::Vectortypes::TVector4f FCameraUpVector;
	Gls::Vectortypes::TVector4f FTargetPosition;
	double FTime;
	
public:
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Gls::Vectortypes::TVector2f RotateSpeed = {read=FRotateSpeed};
	__property Gls::Vectortypes::TVector4f CameraUpVector = {read=FCameraUpVector};
	__property Gls::Vectortypes::TVector4f TargetPosition = {read=FTargetPosition};
	__property Gls::Vectortypes::TVector4f FinalPos = {read=FFinalPos};
	__property double Time = {read=FTime};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLOrbitToPosJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLOrbitToPosJob() { }
	
};


class PASCALIMPLEMENTATION TGLSmoothOrbitToPos : public TGLOrbitToPosJob
{
	typedef TGLOrbitToPosJob inherited;
	
private:
	float FCutoffAngle;
	bool FNeedToRecalculateZoom;
	Gls::Vectortypes::TMatrix4f FShouldBeMatrix;
	Gls::Smoothnavigator::TGLNavigatorSmoothChangeVector* FSmoothNavigator;
	
public:
	__fastcall virtual TGLSmoothOrbitToPos(TGLCameraJobList* const AJoblist);
	virtual void __fastcall Step();
	__property float CutoffAngle = {read=FCutoffAngle, write=FCutoffAngle};
	__property bool NeedToRecalculateZoom = {read=FNeedToRecalculateZoom, write=FNeedToRecalculateZoom, nodefault};
public:
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLSmoothOrbitToPos() { }
	
};


class PASCALIMPLEMENTATION TGLOrbitToPosAdvJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Gls::Vectortypes::TVector4f FInitialPos;
	Gls::Vectortypes::TVector4f FFinalPos;
	Gls::Vectortypes::TVector4f FInitialUp;
	Gls::Vectortypes::TVector4f FInitialDir;
	Gls::Vectortypes::TVector4f FRotAxis;
	double FAngle;
	
public:
	double X;
	double Y;
	double Z;
	double Time;
	bool PreferUpAxis;
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Gls::Vectortypes::TVector4f InitialPos = {read=FInitialPos};
	__property Gls::Vectortypes::TVector4f InitialUp = {read=FInitialUp};
	__property Gls::Vectortypes::TVector4f InitialDir = {read=FInitialDir};
	__property Gls::Vectortypes::TVector4f FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLOrbitToPosAdvJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLOrbitToPosAdvJob() { }
	
};


class PASCALIMPLEMENTATION TGLSmoothOrbitToPosAdvJob : public TGLOrbitToPosAdvJob
{
	typedef TGLOrbitToPosAdvJob inherited;
	
private:
	Gls::Vectortypes::TVector4f FPreviousPosition;
	Gls::Smoothnavigator::TGLNavigatorSmoothChangeVector* FSmoothNavigator;
	bool FRestoreUpVector;
	
public:
	virtual void __fastcall Step();
	virtual void __fastcall Init();
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLSmoothOrbitToPosAdvJob(TGLCameraJobList* const AJoblist) : TGLOrbitToPosAdvJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLSmoothOrbitToPosAdvJob() { }
	
};


typedef void __fastcall (__closure *TGLCameraJobEvent)(TGLCameraJob* Sender);

class PASCALIMPLEMENTATION TGLCameraController : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLCameraJobList* FCameraJobList;
	Gls::Scene::TGLBaseSceneObject* FCamera;
	Gls::Scene::TGLBaseSceneObject* FCameraTarget;
	TGLCameraJobEvent FOnJobAdded;
	TGLCameraJobEvent FOnJobFinished;
	TGLCameraJobEvent FOnJobStep;
	double FsoSafeDist;
	double FsoTimeToSafePlacement;
	double FsoTimeToOrbit;
	double FsoTimeToZoomBackIn;
	void __fastcall CheckAssignments(bool Extended);
	void __fastcall SetOnJobAdded(const TGLCameraJobEvent Value);
	void __fastcall SetOnJobFinished(const TGLCameraJobEvent Value);
	void __fastcall SetOnJobStep(const TGLCameraJobEvent Value);
	void __fastcall SetCamera(Gls::Scene::TGLBaseSceneObject* const Value);
	void __fastcall SetCameraTarget(Gls::Scene::TGLBaseSceneObject* const Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCameraController(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCameraController();
	TGLMoveToPosJob* __fastcall MoveToPos(double X, double Y, double Z, double Time);
	TGLOrbitToPosJob* __fastcall OrbitToPos(double X, double Y, double Z, double Time);
	TGLSmoothOrbitToPos* __fastcall OrbitToPosSmooth(const Gls::Vectortypes::TVector4f &ATargetPosition, const double ATime, Gls::Smoothnavigator::TGLNavigatorSmoothChangeVector* const ASmoothNavigator, const bool AFNeedToRecalculateZoom, const Gls::Vectortypes::PGLVector ACameraUpVector = (Gls::Vectortypes::PGLVector)(0x0));
	TGLOrbitToPosAdvJob* __fastcall OrbitToPosAdvanced(double X, double Y, double Z, double Time, bool PreferUpAxis = true);
	TGLSmoothOrbitToPosAdvJob* __fastcall OrbitToPosAdvancedSmooth(const double X, const double Y, const double Z, const double Time, Gls::Smoothnavigator::TGLNavigatorSmoothChangeVector* const ASmoothNavigator, const bool PreferUpAxis = true);
	TGLZoomToDistanceJob* __fastcall ZoomToDistance(double Distance, double Time);
	void __fastcall SafeOrbitAndZoomToPos(double X, double Y, double Z);
	void __fastcall StopMovement();
	void __fastcall Step(const double deltaTime, const double newTime);
	__property TGLCameraJobList* CameraJobList = {read=FCameraJobList};
	
__published:
	__property Gls::Scene::TGLBaseSceneObject* Camera = {read=FCamera, write=SetCamera};
	__property Gls::Scene::TGLBaseSceneObject* CameraTarget = {read=FCameraTarget, write=SetCameraTarget};
	__property double soSafeDistance = {read=FsoSafeDist, write=FsoSafeDist};
	__property double soTimeToSafePlacement = {read=FsoTimeToSafePlacement, write=FsoTimeToSafePlacement};
	__property double soTimeToOrbit = {read=FsoTimeToOrbit, write=FsoTimeToOrbit};
	__property double soTimeToZoomBackIn = {read=FsoTimeToZoomBackIn, write=FsoTimeToZoomBackIn};
	__property TGLCameraJobEvent OnJobAdded = {read=FOnJobAdded, write=SetOnJobAdded};
	__property TGLCameraJobEvent OnJobStep = {read=FOnJobStep, write=SetOnJobStep};
	__property TGLCameraJobEvent OnJobFinished = {read=FOnJobFinished, write=SetOnJobFinished};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Cameracontroller */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CAMERACONTROLLER)
using namespace Gls::Cameracontroller;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_CameracontrollerHPP
