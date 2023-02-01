// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Scene.pas' rev: 35.00 (Windows)

#ifndef Gls_SceneHPP
#define Gls_SceneHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Math.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Silhouette.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.State.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Selection.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Logger.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Scene
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IGLInitializable;
typedef System::DelphiInterface<IGLInitializable> _di_IGLInitializable;
class DELPHICLASS TGLInitializableObjectList;
class DELPHICLASS TGLBaseSceneObject;
class DELPHICLASS TGLBaseBehaviour;
class DELPHICLASS TGLBehaviour;
class DELPHICLASS TGLBehaviours;
class DELPHICLASS TGLEffect;
class DELPHICLASS TGLObjectPreEffect;
class DELPHICLASS TGLObjectPostEffect;
class DELPHICLASS TGLObjectAfterEffect;
class DELPHICLASS TGLEffects;
class DELPHICLASS TGLCustomSceneObject;
class DELPHICLASS TGLSceneRootObject;
class DELPHICLASS TGLImmaterialSceneObject;
class DELPHICLASS TGLCameraInvariantObject;
class DELPHICLASS TGLSceneObject;
class DELPHICLASS TGLDirectOpenGL;
class DELPHICLASS TGLRenderPoint;
class DELPHICLASS TGLProxyObject;
class DELPHICLASS TGLLightSource;
class DELPHICLASS TGLCamera;
class DELPHICLASS TGLScene;
class DELPHICLASS TGLFogEnvironment;
class DELPHICLASS TGLSceneBuffer;
class DELPHICLASS TGLNonVisualViewer;
class DELPHICLASS TGLMemoryViewer;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLProxyObjectOption : unsigned char { pooEffects, pooObjects, pooTransformation };

typedef System::Set<TGLProxyObjectOption, TGLProxyObjectOption::pooEffects, TGLProxyObjectOption::pooTransformation> TGLProxyObjectOptions;

enum DECLSPEC_DENUM TGLCameraInvarianceMode : unsigned char { cimNone, cimPosition, cimOrientation };

enum DECLSPEC_DENUM TGLSceneViewerMode : unsigned char { svmDisabled, svmDefault, svmNavigation, svmGizmo };

enum DECLSPEC_DENUM TGLNormalDirection : unsigned char { ndInside, ndOutside };

enum DECLSPEC_DENUM TGLObjectChange : unsigned char { ocTransformation, ocAbsoluteMatrix, ocInvAbsoluteMatrix, ocStructure };

typedef System::Set<TGLObjectChange, TGLObjectChange::ocTransformation, TGLObjectChange::ocStructure> TGLObjectChanges;

enum DECLSPEC_DENUM TGLObjectBBChange : unsigned char { oBBcChild, oBBcStructure };

typedef System::Set<TGLObjectBBChange, TGLObjectBBChange::oBBcChild, TGLObjectBBChange::oBBcStructure> TGLObjectBBChanges;

enum DECLSPEC_DENUM TGLSceneOperation : unsigned char { soAdd, soRemove, soMove, soRename, soSelect, soBeginUpdate, soEndUpdate };

enum DECLSPEC_DENUM TGLContextOption : unsigned char { roSoftwareMode, roDoubleBuffer, roStencilBuffer, roRenderToWindow, roTwoSideLighting, roStereo, roDestinationAlpha, roNoColorBuffer, roNoColorBufferClear, roNoSwapBuffers, roNoDepthBufferClear, roDebugContext, roForwardContext, roOpenGL_ES2_Context };

typedef System::Set<TGLContextOption, TGLContextOption::roSoftwareMode, TGLContextOption::roOpenGL_ES2_Context> TGLContextOptions;

enum DECLSPEC_DENUM TGLLimitType : unsigned char { limClipPlanes, limEvalOrder, limLights, limListNesting, limModelViewStack, limNameStack, limPixelMapTable, limProjectionStack, limTextureSize, limTextureStack, limViewportDims, limAccumAlphaBits, limAccumBlueBits, limAccumGreenBits, limAccumRedBits, limAlphaBits, limAuxBuffers, limBlueBits, limGreenBits, limRedBits, limIndexBits, limStereo, limDoubleBuffer, limSubpixelBits, limDepthBits, limStencilBits, limNbTextureUnits };

_DECLARE_METACLASS(System::TMetaClass, TGLSceneObjectClass);

_DECLARE_METACLASS(System::TMetaClass, TGLBehaviourClass);

_DECLARE_METACLASS(System::TMetaClass, TGLEffectClass);

enum DECLSPEC_DENUM TGLObjectStyle : unsigned char { osDirectDraw, osIgnoreDepthBuffer, osNoVisibilityCulling };

typedef System::Set<TGLObjectStyle, TGLObjectStyle::osDirectDraw, TGLObjectStyle::osNoVisibilityCulling> TGLObjectStyles;

__interface  INTERFACE_UUID("{EA40AE8E-79B3-42F5-ADF1-7A901B665E12}") IGLInitializable  : public System::IInterface 
{
	virtual void __fastcall InitializeObject(System::TObject* ASender, const Gls::Rendercontextinfo::TGLRenderContextInfo &ARci) = 0 ;
};

class PASCALIMPLEMENTATION TGLInitializableObjectList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	_di_IGLInitializable operator[](const int Index) { return this->Items[Index]; }
	
private:
	_di_IGLInitializable __fastcall GetItems(const int Index);
	void __fastcall PutItems(const int Index, const _di_IGLInitializable Value);
	
public:
	HIDESBASE int __fastcall Add(const _di_IGLInitializable Item);
	__property _di_IGLInitializable Items[const int Index] = {read=GetItems, write=PutItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLInitializableObjectList() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLInitializableObjectList() : System::Classes::TList() { }
	
};


class PASCALIMPLEMENTATION TGLBaseSceneObject : public Gls::Coordinates::TGLCoordinatesUpdateAbleComponent
{
	typedef Gls::Coordinates::TGLCoordinatesUpdateAbleComponent inherited;
	
public:
	TGLBaseSceneObject* operator[](int Index) { return this->Children[Index]; }
	
private:
	Gls::Vectortypes::TMatrix4f FAbsoluteMatrix;
	Gls::Vectortypes::TMatrix4f FInvAbsoluteMatrix;
	Gls::Vectortypes::TMatrix4f FLocalMatrix;
	TGLObjectStyles FObjectStyle;
	Gls::Context::TGLListHandle* FListHandle;
	Gls::Coordinates::TGLCoordinates3* FPosition;
	Gls::Coordinates::TGLCoordinates3* FDirection;
	Gls::Coordinates::TGLCoordinates3* FUp;
	Gls::Coordinates::TGLCoordinates3* FScaling;
	TGLObjectChanges FChanges;
	TGLBaseSceneObject* FParent;
	TGLScene* FScene;
	TGLObjectBBChanges FBBChanges;
	Gls::Geometrybb::THmgBoundingBox FBoundingBoxPersonalUnscaled;
	Gls::Geometrybb::THmgBoundingBox FBoundingBoxOfChildren;
	Gls::Geometrybb::THmgBoundingBox FBoundingBoxIncludingChildren;
	Gls::Persistentclasses::TGLPersistentObjectList* FChildren;
	bool FVisible;
	int FUpdateCount;
	bool FShowAxes;
	Gls::Coordinates::TGLCoordinates3* FRotation;
	bool FIsCalculating;
	Gls::Rendercontextinfo::TGLObjectsSorting FObjectsSorting;
	Gls::Rendercontextinfo::TGLVisibilityCulling FVisibilityCulling;
	Gls::Baseclasses::TGLProgressEvent FOnProgress;
	System::Classes::TNotifyEvent FOnAddedToParent;
	TGLBehaviours* FBehaviours;
	TGLEffects* FEffects;
	bool FPickable;
	System::Classes::TNotifyEvent FOnPicked;
	System::TObject* FTagObject;
	float FTagFloat;
	Gls::Persistentclasses::TGLPersistentObjectList* objList;
	Gls::Vectorlists::TGLSingleList* distList;
	TGLBaseSceneObject* __fastcall Get(int Index);
	int __fastcall GetCount();
	int __fastcall GetIndex();
	void __fastcall SetParent(TGLBaseSceneObject* const val);
	void __fastcall SetIndex(int aValue);
	void __fastcall SetDirection(Gls::Coordinates::TGLCoordinates3* AVector);
	void __fastcall SetUp(Gls::Coordinates::TGLCoordinates3* AVector);
	Gls::Vectortypes::PGLMatrix __fastcall GetMatrix();
	void __fastcall SetPosition(Gls::Coordinates::TGLCoordinates3* APosition);
	void __fastcall SetPitchAngle(float AValue);
	void __fastcall SetRollAngle(float AValue);
	void __fastcall SetTurnAngle(float AValue);
	void __fastcall SetRotation(Gls::Coordinates::TGLCoordinates3* aRotation);
	float __fastcall GetPitchAngle();
	float __fastcall GetTurnAngle();
	float __fastcall GetRollAngle();
	void __fastcall SetShowAxes(bool AValue);
	void __fastcall SetScaling(Gls::Coordinates::TGLCoordinates3* AValue);
	void __fastcall SetObjectsSorting(const Gls::Rendercontextinfo::TGLObjectsSorting val);
	void __fastcall SetVisibilityCulling(const Gls::Rendercontextinfo::TGLVisibilityCulling val);
	void __fastcall SetBehaviours(TGLBehaviours* const val);
	TGLBehaviours* __fastcall GetBehaviours();
	void __fastcall SetEffects(TGLEffects* const val);
	TGLEffects* __fastcall GetEffects();
	Gls::Vectortypes::TVector3f __fastcall GetAbsoluteAffineScale();
	Gls::Vectortypes::TVector4f __fastcall GetAbsoluteScale();
	void __fastcall SetAbsoluteAffineScale(const Gls::Vectortypes::TVector3f &Value);
	void __fastcall SetAbsoluteScale(const Gls::Vectortypes::TVector4f &Value);
	Gls::Vectortypes::TMatrix4f __fastcall GetAbsoluteMatrix();
	void __fastcall SetAbsoluteMatrix(const Gls::Vectortypes::TMatrix4f &Value);
	void __fastcall SetBBChanges(const TGLObjectBBChanges Value);
	Gls::Vectortypes::PGLMatrix __fastcall GetDirectAbsoluteMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall GetLocalMatrix();
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall SetScene(TGLScene* const Value);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteBehaviours(System::Classes::TStream* stream);
	void __fastcall ReadBehaviours(System::Classes::TStream* stream);
	void __fastcall WriteEffects(System::Classes::TStream* stream);
	void __fastcall ReadEffects(System::Classes::TStream* stream);
	void __fastcall WriteRotations(System::Classes::TStream* stream);
	void __fastcall ReadRotations(System::Classes::TStream* stream);
	virtual bool __fastcall GetVisible();
	virtual bool __fastcall GetPickable();
	virtual void __fastcall SetVisible(bool aValue);
	virtual void __fastcall SetPickable(bool aValue);
	void __fastcall SetAbsolutePosition(const Gls::Vectortypes::TVector4f &v);
	Gls::Vectortypes::TVector4f __fastcall GetAbsolutePosition();
	void __fastcall SetAbsoluteUp(const Gls::Vectortypes::TVector4f &v);
	Gls::Vectortypes::TVector4f __fastcall GetAbsoluteUp();
	void __fastcall SetAbsoluteDirection(const Gls::Vectortypes::TVector4f &v);
	Gls::Vectortypes::TVector4f __fastcall GetAbsoluteDirection();
	Gls::Vectortypes::TVector3f __fastcall GetAbsoluteAffinePosition();
	void __fastcall SetAbsoluteAffinePosition(const Gls::Vectortypes::TVector3f &Value);
	void __fastcall SetAbsoluteAffineUp(const Gls::Vectortypes::TVector3f &v);
	Gls::Vectortypes::TVector3f __fastcall GetAbsoluteAffineUp();
	void __fastcall SetAbsoluteAffineDirection(const Gls::Vectortypes::TVector3f &v);
	Gls::Vectortypes::TVector3f __fastcall GetAbsoluteAffineDirection();
	void __fastcall RecTransformationChanged();
	void __fastcall DrawAxes(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::Word pattern);
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc AProc, System::Classes::TComponent* Root);
	virtual bool __fastcall Blended();
	void __fastcall RebuildMatrix();
	virtual void __fastcall SetName(const System::Classes::TComponentName NewName);
	DYNAMIC void __fastcall SetParentComponent(System::Classes::TComponent* Value);
	virtual void __fastcall DestroyHandle();
	void __fastcall DestroyHandles();
	void __fastcall DeleteChildCameras();
	virtual void __fastcall DoOnAddedToParent();
	virtual void __fastcall CalculateBoundingBoxPersonalUnscaled(Gls::Geometrybb::THmgBoundingBox &ANewBoundingBox);
	
public:
	__fastcall virtual TGLBaseSceneObject(System::Classes::TComponent* AOwner);
	__fastcall TGLBaseSceneObject(TGLBaseSceneObject* aParentOwner);
	__fastcall virtual ~TGLBaseSceneObject();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TGLObjectStyles ObjectStyle = {read=FObjectStyle, write=FObjectStyle, nodefault};
	unsigned __fastcall GetHandle(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	bool __fastcall ListHandleAllocated();
	void __fastcall SetMatrix(const Gls::Vectortypes::TMatrix4f &aValue);
	__property Gls::Vectortypes::PGLMatrix Matrix = {read=GetMatrix};
	__property Gls::Vectortypes::PGLMatrix LocalMatrix = {read=GetLocalMatrix};
	void __fastcall ForceLocalMatrix(const Gls::Vectortypes::TMatrix4f &aMatrix);
	Gls::Vectortypes::PGLMatrix __fastcall AbsoluteMatrixAsAddress();
	__property Gls::Vectortypes::PGLMatrix DirectAbsoluteMatrix = {read=GetDirectAbsoluteMatrix};
	Gls::Vectortypes::TMatrix4f __fastcall InvAbsoluteMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall InvAbsoluteMatrixAsAddress();
	__property Gls::Vectortypes::TMatrix4f AbsoluteMatrix = {read=GetAbsoluteMatrix, write=SetAbsoluteMatrix};
	__property Gls::Vectortypes::TVector4f AbsoluteDirection = {read=GetAbsoluteDirection, write=SetAbsoluteDirection};
	__property Gls::Vectortypes::TVector3f AbsoluteAffineDirection = {read=GetAbsoluteAffineDirection, write=SetAbsoluteAffineDirection};
	__property Gls::Vectortypes::TVector4f AbsoluteScale = {read=GetAbsoluteScale, write=SetAbsoluteScale};
	__property Gls::Vectortypes::TVector3f AbsoluteAffineScale = {read=GetAbsoluteAffineScale, write=SetAbsoluteAffineScale};
	__property Gls::Vectortypes::TVector4f AbsoluteUp = {read=GetAbsoluteUp, write=SetAbsoluteUp};
	__property Gls::Vectortypes::TVector3f AbsoluteAffineUp = {read=GetAbsoluteAffineUp, write=SetAbsoluteAffineUp};
	Gls::Vectortypes::TVector4f __fastcall AbsoluteRight();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteLeft();
	__property Gls::Vectortypes::TVector4f AbsolutePosition = {read=GetAbsolutePosition, write=SetAbsolutePosition};
	__property Gls::Vectortypes::TVector3f AbsoluteAffinePosition = {read=GetAbsoluteAffinePosition, write=SetAbsoluteAffinePosition};
	Gls::Vectortypes::PGLVector __fastcall AbsolutePositionAsAddress();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteXVector();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteYVector();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteZVector();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteToLocal(const Gls::Vectortypes::TVector4f &v)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall AbsoluteToLocal(const Gls::Vectortypes::TVector3f &v)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall LocalToAbsolute(const Gls::Vectortypes::TVector4f &v)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall LocalToAbsolute(const Gls::Vectortypes::TVector3f &v)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall Right();
	Gls::Vectortypes::TVector4f __fastcall LeftVector();
	Gls::Vectortypes::TVector3f __fastcall AffineRight();
	Gls::Vectortypes::TVector3f __fastcall AffineLeftVector();
	float __fastcall SqrDistanceTo(TGLBaseSceneObject* anObject)/* overload */;
	float __fastcall SqrDistanceTo(const Gls::Vectortypes::TVector4f &pt)/* overload */;
	float __fastcall SqrDistanceTo(const Gls::Vectortypes::TVector3f &pt)/* overload */;
	float __fastcall DistanceTo(TGLBaseSceneObject* anObject)/* overload */;
	float __fastcall DistanceTo(const Gls::Vectortypes::TVector3f &pt)/* overload */;
	float __fastcall DistanceTo(const Gls::Vectortypes::TVector4f &pt)/* overload */;
	virtual Gls::Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition();
	float __fastcall BarycenterSqrDistanceTo(const Gls::Vectortypes::TVector4f &pt);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensions();
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	Gls::Geometrybb::TAABB __fastcall AxisAlignedBoundingBox(const bool AIncludeChilden = true);
	Gls::Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxUnscaled(const bool AIncludeChilden = true);
	Gls::Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxAbsolute(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Gls::Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxEx();
	Gls::Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxAbsoluteEx();
	Gls::Geometrybb::THmgBoundingBox __fastcall BoundingBox(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Gls::Geometrybb::THmgBoundingBox __fastcall BoundingBoxUnscaled(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Gls::Geometrybb::THmgBoundingBox __fastcall BoundingBoxAbsolute(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Gls::Geometrybb::THmgBoundingBox __fastcall BoundingBoxPersonalUnscaledEx();
	Gls::Geometrybb::THmgBoundingBox __fastcall BoundingBoxOfChildrenEx();
	Gls::Geometrybb::THmgBoundingBox __fastcall BoundingBoxIncludingChildrenEx();
	float __fastcall BoundingSphereRadius();
	float __fastcall BoundingSphereRadiusUnscaled();
	virtual bool __fastcall PointInObject(const Gls::Vectortypes::TVector4f &point);
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	virtual Gls::Silhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Gls::Silhouette::TGLSilhouetteParameters &silhouetteParameters);
	__property TGLBaseSceneObject* Children[int Index] = {read=Get/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	__property int Index = {read=GetIndex, write=SetIndex, nodefault};
	virtual TGLBaseSceneObject* __fastcall AddNewChild(TGLSceneObjectClass aChild);
	virtual TGLBaseSceneObject* __fastcall AddNewChildFirst(TGLSceneObjectClass aChild);
	virtual void __fastcall AddChild(TGLBaseSceneObject* aChild);
	TGLBehaviour* __fastcall GetOrCreateBehaviour(TGLBehaviourClass aBehaviour);
	TGLBehaviour* __fastcall AddNewBehaviour(TGLBehaviourClass aBehaviour);
	TGLEffect* __fastcall GetOrCreateEffect(TGLEffectClass aEffect);
	TGLEffect* __fastcall AddNewEffect(TGLEffectClass aEffect);
	bool __fastcall HasSubChildren();
	virtual void __fastcall DeleteChildren();
	HIDESBASE virtual void __fastcall Insert(int aIndex, TGLBaseSceneObject* aChild);
	HIDESBASE virtual void __fastcall Remove(TGLBaseSceneObject* aChild, bool keepChildren);
	int __fastcall IndexOfChild(TGLBaseSceneObject* aChild);
	TGLBaseSceneObject* __fastcall FindChild(const System::UnicodeString aName, bool ownChildrenOnly);
	void __fastcall ExchangeChildrenSafe(int anIndex1, int anIndex2);
	void __fastcall ExchangeChildren(int anIndex1, int anIndex2);
	void __fastcall MoveChildUp(int anIndex);
	void __fastcall MoveChildDown(int anIndex);
	void __fastcall MoveChildFirst(int anIndex);
	void __fastcall MoveChildLast(int anIndex);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	virtual void __fastcall MoveTo(TGLBaseSceneObject* newParent);
	void __fastcall MoveUp();
	void __fastcall MoveDown();
	void __fastcall MoveFirst();
	void __fastcall MoveLast();
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	DYNAMIC System::Classes::TComponent* __fastcall GetParentComponent();
	DYNAMIC bool __fastcall HasParent() _FINAL_ATTRIBUTE;
	bool __fastcall IsUpdating();
	void __fastcall Lift(float ADistance);
	void __fastcall Move(float ADistance);
	void __fastcall Translate(float tx, float ty, float tz);
	void __fastcall MoveObjectAround(TGLBaseSceneObject* anObject, float pitchDelta, float turnDelta);
	void __fastcall MoveObjectAllAround(TGLBaseSceneObject* anObject, float pitchDelta, float turnDelta);
	void __fastcall Pitch(float angle);
	void __fastcall Roll(float angle);
	void __fastcall Turn(float angle);
	void __fastcall ResetRotations();
	void __fastcall ResetAndPitchTurnRoll(const float degX, const float degY, const float degZ);
	void __fastcall RotateAbsolute(const float rx, const float ry, const float rz)/* overload */;
	void __fastcall RotateAbsolute(const Gls::Vectortypes::TVector3f &axis, float angle)/* overload */;
	void __fastcall Slide(float ADistance);
	void __fastcall PointTo(TGLBaseSceneObject* const ATargetObject, const Gls::Vectortypes::TVector4f &AUpVector)/* overload */;
	void __fastcall PointTo(const Gls::Vectortypes::TVector4f &AAbsolutePosition, const Gls::Vectortypes::TVector4f &AUpVector)/* overload */;
	void __fastcall Render(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	void __fastcall RenderChildren(int firstChildIndex, int lastChildIndex, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall StructureChanged();
	void __fastcall ClearStructureChanged();
	virtual void __fastcall CoordinateChanged(Gls::Coordinates::TGLCustomCoordinates* Sender);
	void __fastcall TransformationChanged();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Gls::Coordinates::TGLCoordinates3* Rotation = {read=FRotation, write=SetRotation};
	__property float PitchAngle = {read=GetPitchAngle, write=SetPitchAngle};
	__property float RollAngle = {read=GetRollAngle, write=SetRollAngle};
	__property float TurnAngle = {read=GetTurnAngle, write=SetTurnAngle};
	__property bool ShowAxes = {read=FShowAxes, write=SetShowAxes, default=0};
	__property TGLObjectChanges Changes = {read=FChanges, nodefault};
	__property TGLObjectBBChanges BBChanges = {read=FBBChanges, write=SetBBChanges, nodefault};
	__property TGLBaseSceneObject* Parent = {read=FParent, write=SetParent};
	__property Gls::Coordinates::TGLCoordinates3* Position = {read=FPosition, write=SetPosition};
	__property Gls::Coordinates::TGLCoordinates3* Direction = {read=FDirection, write=SetDirection};
	__property Gls::Coordinates::TGLCoordinates3* Up = {read=FUp, write=SetUp};
	__property Gls::Coordinates::TGLCoordinates3* Scale = {read=FScaling, write=SetScaling};
	__property TGLScene* Scene = {read=FScene};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property bool Pickable = {read=FPickable, write=SetPickable, default=1};
	__property Gls::Rendercontextinfo::TGLObjectsSorting ObjectsSorting = {read=FObjectsSorting, write=SetObjectsSorting, default=0};
	__property Gls::Rendercontextinfo::TGLVisibilityCulling VisibilityCulling = {read=FVisibilityCulling, write=SetVisibilityCulling, default=0};
	__property Gls::Baseclasses::TGLProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property System::Classes::TNotifyEvent OnPicked = {read=FOnPicked, write=FOnPicked};
	__property System::Classes::TNotifyEvent OnAddedToParent = {read=FOnAddedToParent, write=FOnAddedToParent};
	__property TGLBehaviours* Behaviours = {read=GetBehaviours, write=SetBehaviours, stored=false};
	__property TGLEffects* Effects = {read=GetEffects, write=SetEffects, stored=false};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	
__published:
	__property float TagFloat = {read=FTagFloat, write=FTagFloat};
};


class PASCALIMPLEMENTATION TGLBaseBehaviour : public Gls::Xcollection::TXCollectionItem
{
	typedef Gls::Xcollection::TXCollectionItem inherited;
	
protected:
	virtual void __fastcall SetName(const System::UnicodeString val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	TGLBaseSceneObject* __fastcall OwnerBaseSceneObject();
	
public:
	__fastcall virtual TGLBaseBehaviour(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBaseBehaviour();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
};


class PASCALIMPLEMENTATION TGLBehaviour : public TGLBaseBehaviour
{
	typedef TGLBaseBehaviour inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLBehaviour(Gls::Xcollection::TXCollection* aOwner) : TGLBaseBehaviour(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLBehaviour() { }
	
};


class PASCALIMPLEMENTATION TGLBehaviours : public Gls::Xcollection::TXCollection
{
	typedef Gls::Xcollection::TXCollection inherited;
	
public:
	TGLBehaviour* operator[](int index) { return this->Behaviour[index]; }
	
protected:
	TGLBehaviour* __fastcall GetBehaviour(int index);
	
public:
	__fastcall virtual TGLBehaviours(System::Classes::TPersistent* aOwner);
	DYNAMIC System::UnicodeString __fastcall GetNamePath();
	__classmethod virtual Gls::Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLBehaviour* Behaviour[int index] = {read=GetBehaviour/*, default*/};
	virtual bool __fastcall CanAdd(Gls::Xcollection::TXCollectionItemClass aClass);
	void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTimes);
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLBehaviours() { }
	
};


class PASCALIMPLEMENTATION TGLEffect : public TGLBaseBehaviour
{
	typedef TGLBaseBehaviour inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	virtual void __fastcall Render(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLEffect(Gls::Xcollection::TXCollection* aOwner) : TGLBaseBehaviour(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLEffect() { }
	
};


class PASCALIMPLEMENTATION TGLObjectPreEffect : public TGLEffect
{
	typedef TGLEffect inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLObjectPreEffect(Gls::Xcollection::TXCollection* aOwner) : TGLEffect(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLObjectPreEffect() { }
	
};


class PASCALIMPLEMENTATION TGLObjectPostEffect : public TGLEffect
{
	typedef TGLEffect inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLObjectPostEffect(Gls::Xcollection::TXCollection* aOwner) : TGLEffect(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLObjectPostEffect() { }
	
};


class PASCALIMPLEMENTATION TGLObjectAfterEffect : public TGLEffect
{
	typedef TGLEffect inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLObjectAfterEffect(Gls::Xcollection::TXCollection* aOwner) : TGLEffect(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLObjectAfterEffect() { }
	
};


class PASCALIMPLEMENTATION TGLEffects : public Gls::Xcollection::TXCollection
{
	typedef Gls::Xcollection::TXCollection inherited;
	
public:
	TGLEffect* operator[](int index) { return this->ObjectEffect[index]; }
	
protected:
	TGLEffect* __fastcall GetEffect(int index);
	
public:
	__fastcall virtual TGLEffects(System::Classes::TPersistent* aOwner);
	DYNAMIC System::UnicodeString __fastcall GetNamePath();
	__classmethod virtual Gls::Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLEffect* ObjectEffect[int index] = {read=GetEffect/*, default*/};
	virtual bool __fastcall CanAdd(Gls::Xcollection::TXCollectionItemClass aClass);
	void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	void __fastcall RenderPreEffects(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall RenderPostEffects(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLEffects() { }
	
};


class PASCALIMPLEMENTATION TGLCustomSceneObject : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	Gls::Material::TGLMaterial* FMaterial;
	System::UnicodeString FHint;
	
protected:
	virtual bool __fastcall Blended();
	void __fastcall SetGLMaterial(Gls::Material::TGLMaterial* AValue);
	virtual void __fastcall DestroyHandle();
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TGLCustomSceneObject(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomSceneObject();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property Gls::Material::TGLMaterial* Material = {read=FMaterial, write=SetGLMaterial};
	__property System::UnicodeString Hint = {read=FHint, write=FHint};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomSceneObject(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLSceneRootObject : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
public:
	__fastcall virtual TGLSceneRootObject(System::Classes::TComponent* AOwner);
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSceneRootObject(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	/* TGLBaseSceneObject.Destroy */ inline __fastcall virtual ~TGLSceneRootObject() { }
	
};


class PASCALIMPLEMENTATION TGLImmaterialSceneObject : public TGLCustomSceneObject
{
	typedef TGLCustomSceneObject inherited;
	
public:
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property Pickable = {default=1};
	__property OnProgress;
	__property OnPicked;
	__property Behaviours;
	__property Effects;
	__property Hint = {default=0};
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLImmaterialSceneObject(System::Classes::TComponent* AOwner) : TGLCustomSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLImmaterialSceneObject() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLImmaterialSceneObject(TGLBaseSceneObject* aParentOwner) : TGLCustomSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCameraInvariantObject : public TGLImmaterialSceneObject
{
	typedef TGLImmaterialSceneObject inherited;
	
private:
	TGLCameraInvarianceMode FCamInvarianceMode;
	
protected:
	void __fastcall SetCamInvarianceMode(const TGLCameraInvarianceMode val);
	__property TGLCameraInvarianceMode CamInvarianceMode = {read=FCamInvarianceMode, write=SetCamInvarianceMode, nodefault};
	
public:
	__fastcall virtual TGLCameraInvariantObject(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCameraInvariantObject() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCameraInvariantObject(TGLBaseSceneObject* aParentOwner) : TGLImmaterialSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLSceneObject : public TGLCustomSceneObject
{
	typedef TGLCustomSceneObject inherited;
	
__published:
	__property Material;
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property Pickable = {default=1};
	__property OnProgress;
	__property OnPicked;
	__property Behaviours;
	__property Effects;
	__property Hint = {default=0};
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLSceneObject(System::Classes::TComponent* AOwner) : TGLCustomSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSceneObject() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSceneObject(TGLBaseSceneObject* aParentOwner) : TGLCustomSceneObject(aParentOwner) { }
	
};


typedef void __fastcall (__closure *TGLDirectRenderEvent)(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);

class PASCALIMPLEMENTATION TGLDirectOpenGL : public TGLImmaterialSceneObject
{
	typedef TGLImmaterialSceneObject inherited;
	
private:
	bool FUseBuildList;
	TGLDirectRenderEvent FOnRender;
	bool FBlend;
	
protected:
	void __fastcall SetUseBuildList(const bool val);
	virtual bool __fastcall Blended();
	void __fastcall SetBlend(const bool val);
	
public:
	__fastcall virtual TGLDirectOpenGL(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	
__published:
	__property bool UseBuildList = {read=FUseBuildList, write=SetUseBuildList, nodefault};
	__property TGLDirectRenderEvent OnRender = {read=FOnRender, write=FOnRender};
	__property bool Blend = {read=FBlend, write=SetBlend, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLDirectOpenGL() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDirectOpenGL(TGLBaseSceneObject* aParentOwner) : TGLImmaterialSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLRenderPoint : public TGLImmaterialSceneObject
{
	typedef TGLImmaterialSceneObject inherited;
	
	
private:
	typedef System::DynamicArray<TGLDirectRenderEvent> _TGLRenderPoint__1;
	
	typedef System::DynamicArray<System::Classes::TNotifyEvent> _TGLRenderPoint__2;
	
	
private:
	_TGLRenderPoint__1 FCallBacks;
	_TGLRenderPoint__2 FFreeCallBacks;
	
public:
	__fastcall virtual TGLRenderPoint(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLRenderPoint();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall RegisterCallBack(TGLDirectRenderEvent renderEvent, System::Classes::TNotifyEvent renderPointFreed);
	void __fastcall UnRegisterCallBack(TGLDirectRenderEvent renderEvent);
	void __fastcall Clear();
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLRenderPoint(TGLBaseSceneObject* aParentOwner) : TGLImmaterialSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLProxyObject : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	TGLBaseSceneObject* FMasterObject;
	TGLProxyObjectOptions FProxyOptions;
	
protected:
	bool FRendering;
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetMasterObject(TGLBaseSceneObject* const val);
	void __fastcall SetProxyOptions(const TGLProxyObjectOptions val);
	
public:
	__fastcall virtual TGLProxyObject(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLProxyObject();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual Gls::Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition();
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensions();
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	virtual Gls::Silhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Gls::Silhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property TGLProxyObjectOptions ProxyOptions = {read=FProxyOptions, write=SetProxyOptions, default=7};
	__property ObjectsSorting = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property Pickable = {default=1};
	__property OnProgress;
	__property OnPicked;
	__property Behaviours;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLProxyObject(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TGLProxyObjectClass);

enum DECLSPEC_DENUM TGLLightStyle : unsigned char { lsSpot, lsOmni, lsParallel, lsParallelSpot };

class PASCALIMPLEMENTATION TGLLightSource : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	unsigned FLightID;
	Gls::Coordinates::TGLCoordinates3* FSpotDirection;
	float FSpotExponent;
	float FSpotCutOff;
	float FConstAttenuation;
	float FLinearAttenuation;
	float FQuadraticAttenuation;
	bool FShining;
	Gls::Color::TGLColor* FAmbient;
	Gls::Color::TGLColor* FDiffuse;
	Gls::Color::TGLColor* FSpecular;
	TGLLightStyle FLightStyle;
	
protected:
	void __fastcall SetAmbient(Gls::Color::TGLColor* AValue);
	void __fastcall SetDiffuse(Gls::Color::TGLColor* AValue);
	void __fastcall SetSpecular(Gls::Color::TGLColor* AValue);
	void __fastcall SetConstAttenuation(float AValue);
	void __fastcall SetLinearAttenuation(float AValue);
	void __fastcall SetQuadraticAttenuation(float AValue);
	void __fastcall SetShining(bool AValue);
	void __fastcall SetSpotDirection(Gls::Coordinates::TGLCoordinates3* AVector);
	void __fastcall SetSpotExponent(float AValue);
	void __fastcall SetSpotCutOff(const float val);
	void __fastcall SetLightStyle(const TGLLightStyle val);
	
public:
	__fastcall virtual TGLLightSource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLightSource();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	virtual void __fastcall CoordinateChanged(Gls::Coordinates::TGLCustomCoordinates* Sender);
	virtual Gls::Silhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Gls::Silhouette::TGLSilhouetteParameters &silhouetteParameters);
	__property unsigned LightID = {read=FLightID, nodefault};
	bool __fastcall Attenuated();
	
__published:
	__property Gls::Color::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property float ConstAttenuation = {read=FConstAttenuation, write=SetConstAttenuation};
	__property Gls::Color::TGLColor* Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property float LinearAttenuation = {read=FLinearAttenuation, write=SetLinearAttenuation};
	__property float QuadraticAttenuation = {read=FQuadraticAttenuation, write=SetQuadraticAttenuation};
	__property Position;
	__property TGLLightStyle LightStyle = {read=FLightStyle, write=SetLightStyle, default=0};
	__property bool Shining = {read=FShining, write=SetShining, default=1};
	__property Gls::Color::TGLColor* Specular = {read=FSpecular, write=SetSpecular};
	__property float SpotCutOff = {read=FSpotCutOff, write=SetSpotCutOff};
	__property Gls::Coordinates::TGLCoordinates3* SpotDirection = {read=FSpotDirection, write=SetSpotDirection};
	__property float SpotExponent = {read=FSpotExponent, write=SetSpotExponent};
	__property OnProgress;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLightSource(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLCameraStyle : unsigned char { csPerspective, csOrthogonal, csOrtho2D, csCustom, csInfinitePerspective, csPerspectiveKeepFOV };

enum DECLSPEC_DENUM TGLCameraKeepFOVMode : unsigned char { ckmHorizontalFOV, ckmVerticalFOV };

typedef void __fastcall (__closure *TOnCustomPerspective)(const Gls::Vectorgeometry::TRectangle &viewport, int width, int height, int DPI, float &viewPortRadius);

class PASCALIMPLEMENTATION TGLCamera : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	float FFocalLength;
	float FDepthOfView;
	float FNearPlane;
	float FNearPlaneBias;
	float FViewPortRadius;
	TGLBaseSceneObject* FTargetObject;
	Gls::Vectortypes::TVector4f FLastDirection;
	TGLCameraStyle FCameraStyle;
	TGLCameraKeepFOVMode FKeepFOVMode;
	float FSceneScale;
	System::Classes::TNotifyEvent FDeferredApply;
	TOnCustomPerspective FOnCustomPerspective;
	bool FDesign;
	double FFOVY;
	double FFOVX;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetTargetObject(TGLBaseSceneObject* const val);
	void __fastcall SetDepthOfView(float AValue);
	void __fastcall SetFocalLength(float AValue);
	void __fastcall SetCameraStyle(const TGLCameraStyle val);
	void __fastcall SetKeepFOVMode(const TGLCameraKeepFOVMode val);
	void __fastcall SetSceneScale(float value);
	bool __fastcall StoreSceneScale();
	void __fastcall SetNearPlaneBias(float value);
	bool __fastcall StoreNearPlaneBias();
	
public:
	__fastcall virtual TGLCamera(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLCamera();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property float NearPlane = {read=FNearPlane};
	void __fastcall Apply();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	void __fastcall ApplyPerspective(const Gls::Vectorgeometry::TRectangle &AViewport, int AWidth, int AHeight, int ADPI);
	void __fastcall AutoLeveling(float Factor);
	void __fastcall Reset(TGLSceneBuffer* aSceneBuffer);
	void __fastcall ZoomAll(TGLSceneBuffer* aSceneBuffer);
	void __fastcall RotateObject(TGLBaseSceneObject* obj, float pitchDelta, float turnDelta, float rollDelta = 0.000000E+00f);
	void __fastcall RotateTarget(float pitchDelta, float turnDelta, float rollDelta = 0.000000E+00f);
	void __fastcall MoveAroundTarget(float pitchDelta, float turnDelta);
	void __fastcall MoveAllAroundTarget(float pitchDelta, float turnDelta);
	void __fastcall MoveInEyeSpace(float forwardDistance, float rightDistance, float upDistance);
	void __fastcall MoveTargetInEyeSpace(float forwardDistance, float rightDistance, float upDistance);
	Gls::Vectortypes::TVector4f __fastcall AbsoluteEyeSpaceVector(float forwardDistance, float rightDistance, float upDistance);
	void __fastcall AdjustDistanceToTarget(float distanceRatio);
	float __fastcall DistanceToTarget();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteVectorToTarget();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteRightVectorToTarget();
	Gls::Vectortypes::TVector4f __fastcall AbsoluteUpVectorToTarget();
	Gls::Vectortypes::TVector4f __fastcall ScreenDeltaToVector(int deltaX, int deltaY, float ratio, const Gls::Vectortypes::TVector4f &planeNormal);
	Gls::Vectortypes::TVector4f __fastcall ScreenDeltaToVectorXY(int deltaX, int deltaY, float ratio);
	Gls::Vectortypes::TVector4f __fastcall ScreenDeltaToVectorXZ(int deltaX, int deltaY, float ratio);
	Gls::Vectortypes::TVector4f __fastcall ScreenDeltaToVectorYZ(int deltaX, int deltaY, float ratio);
	bool __fastcall PointInFront(const Gls::Vectortypes::TVector4f &point)/* overload */;
	float __fastcall GetFieldOfView(const float AViewportDimension);
	void __fastcall SetFieldOfView(const float AFieldOfView, const float AViewportDimension);
	
__published:
	__property float DepthOfView = {read=FDepthOfView, write=SetDepthOfView};
	__property float FocalLength = {read=FFocalLength, write=SetFocalLength};
	__property float SceneScale = {read=FSceneScale, write=SetSceneScale, stored=StoreSceneScale};
	__property float NearPlaneBias = {read=FNearPlaneBias, write=SetNearPlaneBias, stored=StoreNearPlaneBias};
	__property TGLBaseSceneObject* TargetObject = {read=FTargetObject, write=SetTargetObject};
	__property TGLCameraStyle CameraStyle = {read=FCameraStyle, write=SetCameraStyle, default=0};
	__property TGLCameraKeepFOVMode KeepFOVMode = {read=FKeepFOVMode, write=SetKeepFOVMode, default=0};
	__property TOnCustomPerspective OnCustomPerspective = {read=FOnCustomPerspective, write=FOnCustomPerspective};
	__property Position;
	__property Direction;
	__property Up;
	__property OnProgress;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCamera(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLScene : public Gls::Baseclasses::TGLUpdateAbleComponent
{
	typedef Gls::Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	int FUpdateCount;
	TGLSceneRootObject* FObjects;
	Gls::Context::TGLContext* FBaseContext;
	Gls::Persistentclasses::TGLPersistentObjectList* FLights;
	Gls::Persistentclasses::TGLPersistentObjectList* FBuffers;
	TGLCamera* FCurrentGLCamera;
	TGLSceneBuffer* FCurrentBuffer;
	Gls::Rendercontextinfo::TGLObjectsSorting FObjectsSorting;
	Gls::Rendercontextinfo::TGLVisibilityCulling FVisibilityCulling;
	Gls::Baseclasses::TGLProgressEvent FOnBeforeProgress;
	Gls::Baseclasses::TGLProgressEvent FOnProgress;
	double FCurrentDeltaTime;
	TGLInitializableObjectList* FInitializableObjects;
	
protected:
	void __fastcall AddLight(TGLLightSource* aLight);
	void __fastcall RemoveLight(TGLLightSource* aLight);
	void __fastcall AddLights(TGLBaseSceneObject* anObj);
	void __fastcall RemoveLights(TGLBaseSceneObject* anObj);
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc AProc, System::Classes::TComponent* Root);
	DYNAMIC void __fastcall SetChildOrder(System::Classes::TComponent* AChild, int Order);
	void __fastcall SetObjectsSorting(const Gls::Rendercontextinfo::TGLObjectsSorting val);
	void __fastcall SetVisibilityCulling(const Gls::Rendercontextinfo::TGLVisibilityCulling val);
	virtual void __fastcall ReadState(System::Classes::TReader* Reader);
	
public:
	__fastcall virtual TGLScene(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLScene();
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	bool __fastcall IsUpdating();
	void __fastcall AddBuffer(TGLSceneBuffer* aBuffer);
	void __fastcall RemoveBuffer(TGLSceneBuffer* aBuffer);
	void __fastcall SetupLights(int maxLights);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall Progress(const double deltaTime, const double newTime);
	TGLBaseSceneObject* __fastcall FindSceneObject(const System::UnicodeString AName);
	TGLBaseSceneObject* __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	void __fastcall ShutdownAllLights();
	void __fastcall SaveToFile(const System::UnicodeString fileName);
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToTextFile(const System::UnicodeString fileName);
	void __fastcall LoadFromTextFile(const System::UnicodeString fileName);
	__property TGLCamera* CurrentGLCamera = {read=FCurrentGLCamera};
	__property Gls::Persistentclasses::TGLPersistentObjectList* Lights = {read=FLights};
	__property TGLSceneRootObject* Objects = {read=FObjects};
	__property TGLSceneBuffer* CurrentBuffer = {read=FCurrentBuffer};
	__property TGLInitializableObjectList* InitializableObjects = {read=FInitializableObjects};
	__property double CurrentDeltaTime = {read=FCurrentDeltaTime};
	
__published:
	__property Gls::Rendercontextinfo::TGLObjectsSorting ObjectsSorting = {read=FObjectsSorting, write=SetObjectsSorting, default=3};
	__property Gls::Rendercontextinfo::TGLVisibilityCulling VisibilityCulling = {read=FVisibilityCulling, write=SetVisibilityCulling, default=1};
	__property Gls::Baseclasses::TGLProgressEvent OnBeforeProgress = {read=FOnBeforeProgress, write=FOnBeforeProgress};
	__property Gls::Baseclasses::TGLProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
};


enum DECLSPEC_DENUM TFogMode : unsigned char { fmLinear, fmExp, fmExp2 };

enum DECLSPEC_DENUM TFogDistance : unsigned char { fdDefault, fdEyeRadial, fdEyePlane };

class PASCALIMPLEMENTATION TGLFogEnvironment : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	TGLSceneBuffer* FSceneBuffer;
	Gls::Color::TGLColor* FFogColor;
	float FFogStart;
	float FFogEnd;
	TFogMode FFogMode;
	TFogDistance FFogDistance;
	
protected:
	void __fastcall SetFogColor(Gls::Color::TGLColor* Value);
	void __fastcall SetFogStart(float Value);
	void __fastcall SetFogEnd(float Value);
	void __fastcall SetFogMode(TFogMode Value);
	void __fastcall SetFogDistance(const TFogDistance val);
	
public:
	__fastcall virtual TGLFogEnvironment(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLFogEnvironment();
	void __fastcall ApplyFog();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	bool __fastcall IsAtDefaultValues();
	
__published:
	__property Gls::Color::TGLColor* FogColor = {read=FFogColor, write=SetFogColor};
	__property float FogStart = {read=FFogStart, write=SetFogStart};
	__property float FogEnd = {read=FFogEnd, write=SetFogEnd};
	__property TFogMode FogMode = {read=FFogMode, write=SetFogMode, default=0};
	__property TFogDistance FogDistance = {read=FFogDistance, write=SetFogDistance, default=0};
};


enum DECLSPEC_DENUM TGLDepthPrecision : unsigned char { dpDefault, dp16bits, dp24bits, dp32bits };

enum DECLSPEC_DENUM TGLColorDepth : unsigned char { cdDefault, cd8bits, cd16bits, cd24bits, cdFloat64bits, cdFloat128bits };

enum DECLSPEC_DENUM TGLShadeModel : unsigned char { smDefault, smSmooth, smFlat };

class PASCALIMPLEMENTATION TGLSceneBuffer : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Vectortypes::TMatrix4f> _TGLSceneBuffer__1;
	
	typedef System::DynamicArray<Gls::Vectortypes::TMatrix4f> _TGLSceneBuffer__2;
	
	
private:
	bool FRendering;
	Gls::Context::TGLContext* FRenderingContext;
	Gls::Persistentclasses::TGLPersistentObjectList* FAfterRenderEffects;
	_TGLSceneBuffer__1 FViewMatrixStack;
	_TGLSceneBuffer__2 FProjectionMatrixStack;
	Gls::Vectortypes::TMatrix4f FBaseProjectionMatrix;
	Gls::Vectortypes::TVector4f FCameraAbsolutePosition;
	Gls::Vectorgeometry::TRectangle FViewPort;
	Gls::Selection::TGLBaseSelectTechnique* FSelector;
	bool FFaceCulling;
	bool FFogEnable;
	bool FLighting;
	bool FDepthTest;
	System::Uitypes::TColor FBackgroundColor;
	float FBackgroundAlpha;
	Gls::Color::TGLColor* FAmbientColor;
	Gls::Context::TGLAntiAliasing FAntiAliasing;
	TGLDepthPrecision FDepthPrecision;
	TGLColorDepth FColorDepth;
	TGLContextOptions FContextOptions;
	TGLShadeModel FShadeModel;
	int FRenderDPI;
	TGLFogEnvironment* FFogEnvironment;
	int FAccumBufferBits;
	Gls::Context::TGLContextLayer FLayer;
	TGLCamera* FCamera;
	void *FFreezeBuffer;
	bool FFreezed;
	Gls::Vectorgeometry::TRectangle FFreezedViewPort;
	int FFrameCount;
	float FFramesPerSecond;
	__int64 FFirstPerfCounter;
	float FLastFrameTime;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnStructuralChange;
	System::Classes::TNotifyEvent FOnPrepareGLContext;
	System::Classes::TNotifyEvent FBeforeRender;
	System::Classes::TNotifyEvent FViewerBeforeRender;
	System::Classes::TNotifyEvent FPostRender;
	System::Classes::TNotifyEvent FAfterRender;
	TGLDirectRenderEvent FInitiateRendering;
	TGLDirectRenderEvent FWrapUpRendering;
	void __fastcall SetLayer(const Gls::Context::TGLContextLayer Value);
	
protected:
	void __fastcall SetBackgroundColor(System::Uitypes::TColor AColor);
	void __fastcall SetBackgroundAlpha(float alpha);
	void __fastcall SetAmbientColor(Gls::Color::TGLColor* AColor);
	int __fastcall GetLimit(TGLLimitType Which);
	void __fastcall SetCamera(TGLCamera* ACamera);
	void __fastcall SetContextOptions(TGLContextOptions Options);
	void __fastcall SetDepthTest(bool AValue);
	void __fastcall SetFaceCulling(bool AValue);
	void __fastcall SetLighting(bool AValue);
	void __fastcall SetAntiAliasing(const Gls::Context::TGLAntiAliasing val);
	void __fastcall SetDepthPrecision(const TGLDepthPrecision val);
	void __fastcall SetColorDepth(const TGLColorDepth val);
	void __fastcall SetShadeModel(const TGLShadeModel val);
	void __fastcall SetFogEnable(bool AValue);
	void __fastcall SetGLFogEnvironment(TGLFogEnvironment* AValue);
	bool __fastcall StoreFog();
	void __fastcall SetAccumBufferBits(const int val);
	void __fastcall PrepareRenderingMatrices(const Gls::Vectorgeometry::TRectangle &aViewPort, int resolution, System::Types::PRect pickingRect = (System::Types::PRect)(0x0));
	void __fastcall DoBaseRender(const Gls::Vectorgeometry::TRectangle &aViewPort, int resolution, Gls::Rendercontextinfo::TGLDrawState drawState, TGLBaseSceneObject* baseObject);
	void __fastcall SetupRenderingContext(Gls::Context::TGLContext* context);
	void __fastcall SetupRCOptions(Gls::Context::TGLContext* context);
	void __fastcall PrepareGLContext();
	void __fastcall DoChange();
	void __fastcall DoStructuralChange();
	__property int RenderDPI = {read=FRenderDPI, nodefault};
	__property System::Classes::TNotifyEvent OnPrepareGLContext = {read=FOnPrepareGLContext, write=FOnPrepareGLContext};
	
public:
	__fastcall virtual TGLSceneBuffer(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSceneBuffer();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall CreateRC(HWND AWindowHandle, bool memoryContext, int BufferCount = 0x1)/* overload */;
	void __fastcall ClearBuffers();
	void __fastcall DestroyRC();
	bool __fastcall RCInstantiated();
	void __fastcall Resize(int newLeft, int newTop, int newWidth, int newHeight);
	Gls::Context::TGLContextAcceleration __fastcall Acceleration();
	__property Gls::Vectorgeometry::TRectangle ViewPort = {read=FViewPort};
	void __fastcall PickObjects(const System::Types::TRect &rect, Gls::Selection::TGLPickList* pickList, int objectCountGuess);
	Gls::Selection::TGLPickList* __fastcall GetPickedObjects(const System::Types::TRect &rect, int objectCountGuess = 0x40);
	TGLBaseSceneObject* __fastcall GetPickedObject(int x, int y);
	System::Uitypes::TColor __fastcall GetPixelColor(int x, int y);
	float __fastcall GetPixelDepth(int x, int y);
	float __fastcall PixelDepthToDistance(float aDepth);
	float __fastcall PixelToDistance(int x, int y);
	void __fastcall NotifyMouseMove(System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall Render(TGLBaseSceneObject* baseObject)/* overload */;
	void __fastcall Render()/* overload */;
	void __fastcall RenderScene(TGLScene* aScene, const int viewPortSizeX, const int viewPortSizeY, Gls::Rendercontextinfo::TGLDrawState drawState, TGLBaseSceneObject* baseObject);
	void __fastcall RenderToBitmap(Vcl::Graphics::TBitmap* ABitmap, int DPI = 0x0);
	void __fastcall RenderToFile(const System::UnicodeString AFile, int DPI = 0x0)/* overload */;
	void __fastcall RenderToFile(const System::UnicodeString AFile, int bmpWidth, int bmpHeight)/* overload */;
	Gls::Graphics::TGLImage* __fastcall CreateSnapShot();
	Vcl::Graphics::TBitmap* __fastcall CreateSnapShotBitmap();
	void __fastcall CopyToTexture(Gls::Texture::TGLTexture* aTexture)/* overload */;
	void __fastcall CopyToTexture(Gls::Texture::TGLTexture* aTexture, int xSrc, int ySrc, int AWidth, int AHeight, int xDest, int yDest, unsigned glCubeFace = (unsigned)(0x0))/* overload */;
	void __fastcall SaveAsFloatToFile(const System::UnicodeString aFilename);
	__property System::Classes::TNotifyEvent ViewerBeforeRender = {read=FViewerBeforeRender, write=FViewerBeforeRender, stored=false};
	void __fastcall SetViewPort(int X, int Y, int W, int H);
	int __fastcall Width();
	int __fastcall Height();
	__property bool Freezed = {read=FFreezed, nodefault};
	void __fastcall Freeze();
	void __fastcall Melt();
	void __fastcall ShowInfo(bool Modal = false);
	__property bool Rendering = {read=FRendering, nodefault};
	__property float BackgroundAlpha = {read=FBackgroundAlpha, write=SetBackgroundAlpha};
	Gls::Vectortypes::TMatrix4f __fastcall ProjectionMatrix _DEPRECATED_ATTRIBUTE0 ();
	Gls::Vectortypes::TMatrix4f __fastcall ViewMatrix _DEPRECATED_ATTRIBUTE0 ();
	Gls::Vectortypes::TMatrix4f __fastcall ModelMatrix _DEPRECATED_ATTRIBUTE0 ();
	__property Gls::Vectortypes::TMatrix4f BaseProjectionMatrix = {read=FBaseProjectionMatrix};
	void __fastcall PushViewMatrix _DEPRECATED_ATTRIBUTE0 (const Gls::Vectortypes::TMatrix4f &newMatrix);
	void __fastcall PopViewMatrix _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall PushProjectionMatrix _DEPRECATED_ATTRIBUTE0 (const Gls::Vectortypes::TMatrix4f &newMatrix);
	void __fastcall PopProjectionMatrix _DEPRECATED_ATTRIBUTE0 ();
	Gls::Vectortypes::TVector3f __fastcall OrthoScreenToWorld(int screenX, int screenY)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall ScreenToWorld(const Gls::Vectortypes::TVector3f &aPoint)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall ScreenToWorld(const Gls::Vectortypes::TVector4f &aPoint)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall ScreenToWorld(int screenX, int screenY)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall WorldToScreen(const Gls::Vectortypes::TVector3f &aPoint)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall WorldToScreen(const Gls::Vectortypes::TVector4f &aPoint)/* overload */;
	void __fastcall WorldToScreen(Gls::Vectortypes::PGLVector points, int nbPoints)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall ScreenToVector(const Gls::Vectortypes::TVector3f &aPoint)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall ScreenToVector(const Gls::Vectortypes::TVector4f &aPoint)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall ScreenToVector(const int x, const int y)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall VectorToScreen(const Gls::Vectortypes::TVector3f &VectToCam);
	bool __fastcall ScreenVectorIntersectWithPlane(const Gls::Vectortypes::TVector4f &aScreenPoint, const Gls::Vectortypes::TVector4f &planePoint, const Gls::Vectortypes::TVector4f &planeNormal, Gls::Vectortypes::TVector4f &intersectPoint);
	bool __fastcall ScreenVectorIntersectWithPlaneXY(const Gls::Vectortypes::TVector4f &aScreenPoint, const float z, Gls::Vectortypes::TVector4f &intersectPoint);
	bool __fastcall ScreenVectorIntersectWithPlaneYZ(const Gls::Vectortypes::TVector4f &aScreenPoint, const float x, Gls::Vectortypes::TVector4f &intersectPoint);
	bool __fastcall ScreenVectorIntersectWithPlaneXZ(const Gls::Vectortypes::TVector4f &aScreenPoint, const float y, Gls::Vectortypes::TVector4f &intersectPoint);
	Gls::Vectortypes::TVector3f __fastcall PixelRayToWorld(int x, int y);
	__property float LastFrameTime = {read=FLastFrameTime};
	__property float FramesPerSecond = {read=FFramesPerSecond};
	void __fastcall ResetPerformanceMonitor();
	__property int LimitOf[TGLLimitType Which] = {read=GetLimit};
	__property Gls::Context::TGLContext* RenderingContext = {read=FRenderingContext};
	__property TGLCamera* Camera = {read=FCamera, write=SetCamera};
	__property Gls::Context::TGLContextLayer Layer = {read=FLayer, write=SetLayer, default=2};
	
__published:
	__property TGLFogEnvironment* FogEnvironment = {read=FFogEnvironment, write=SetGLFogEnvironment, stored=StoreFog};
	__property System::Uitypes::TColor BackgroundColor = {read=FBackgroundColor, write=SetBackgroundColor, default=-16777201};
	__property Gls::Color::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property TGLContextOptions ContextOptions = {read=FContextOptions, write=SetContextOptions, default=2058};
	__property int AccumBufferBits = {read=FAccumBufferBits, write=SetAccumBufferBits, default=0};
	__property bool DepthTest = {read=FDepthTest, write=SetDepthTest, default=1};
	__property bool FaceCulling = {read=FFaceCulling, write=SetFaceCulling, default=1};
	__property bool FogEnable = {read=FFogEnable, write=SetFogEnable, default=0};
	__property bool Lighting = {read=FLighting, write=SetLighting, default=1};
	__property Gls::Context::TGLAntiAliasing AntiAliasing = {read=FAntiAliasing, write=SetAntiAliasing, default=0};
	__property TGLDepthPrecision DepthPrecision = {read=FDepthPrecision, write=SetDepthPrecision, default=0};
	__property TGLColorDepth ColorDepth = {read=FColorDepth, write=SetColorDepth, default=0};
	__property TGLShadeModel ShadeModel = {read=FShadeModel, write=SetShadeModel, default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange, stored=false};
	__property System::Classes::TNotifyEvent OnStructuralChange = {read=FOnStructuralChange, write=FOnStructuralChange, stored=false};
	__property System::Classes::TNotifyEvent BeforeRender = {read=FBeforeRender, write=FBeforeRender, stored=false};
	__property TGLDirectRenderEvent InitiateRendering = {read=FInitiateRendering, write=FInitiateRendering, stored=false};
	__property TGLDirectRenderEvent WrapUpRendering = {read=FWrapUpRendering, write=FWrapUpRendering, stored=false};
	__property System::Classes::TNotifyEvent PostRender = {read=FPostRender, write=FPostRender, stored=false};
	__property System::Classes::TNotifyEvent AfterRender = {read=FAfterRender, write=FAfterRender, stored=false};
};


class PASCALIMPLEMENTATION TGLNonVisualViewer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLSceneBuffer* FBuffer;
	int FWidth;
	int FHeight;
	int FCubeMapRotIdx;
	float FCubeMapZNear;
	float FCubeMapZFar;
	Gls::Vectortypes::TVector3f FCubeMapTranslation;
	
protected:
	void __fastcall SetBeforeRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetBeforeRender();
	void __fastcall SetPostRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetPostRender();
	void __fastcall SetAfterRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetAfterRender();
	void __fastcall SetCamera(TGLCamera* const val);
	TGLCamera* __fastcall GetCamera();
	void __fastcall SetBuffer(TGLSceneBuffer* const val);
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetupCubeMapCamera(System::TObject* Sender);
	void __fastcall DoOnPrepareGLContext(System::TObject* Sender);
	virtual void __fastcall PrepareGLContext();
	virtual void __fastcall DoBufferChange(System::TObject* Sender);
	virtual void __fastcall DoBufferStructuralChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLNonVisualViewer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNonVisualViewer();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Render(TGLBaseSceneObject* baseObject = (TGLBaseSceneObject*)(0x0)) = 0 ;
	virtual void __fastcall CopyToTexture(Gls::Texture::TGLTexture* aTexture)/* overload */;
	void __fastcall CopyToTexture(Gls::Texture::TGLTexture* aTexture, int xSrc, int ySrc, int width, int height, int xDest, int yDest)/* overload */;
	virtual void __fastcall CopyToTextureMRT(Gls::Texture::TGLTexture* aTexture, int BufferIndex)/* overload */;
	void __fastcall CopyToTextureMRT(Gls::Texture::TGLTexture* aTexture, int xSrc, int ySrc, int width, int height, int xDest, int yDest, int BufferIndex)/* overload */;
	void __fastcall RenderCubeMapTextures(Gls::Texture::TGLTexture* cubeMapTexture, float zNear = 0.000000E+00f, float zFar = 0.000000E+00f);
	
__published:
	__property TGLCamera* Camera = {read=GetCamera, write=SetCamera};
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=256};
	__property System::Classes::TNotifyEvent BeforeRender = {read=GetBeforeRender, write=SetBeforeRender};
	__property System::Classes::TNotifyEvent PostRender = {read=GetPostRender, write=SetPostRender};
	__property System::Classes::TNotifyEvent AfterRender = {read=GetAfterRender, write=SetAfterRender};
	__property TGLSceneBuffer* Buffer = {read=FBuffer, write=SetBuffer};
};


class PASCALIMPLEMENTATION TGLMemoryViewer : public TGLNonVisualViewer
{
	typedef TGLNonVisualViewer inherited;
	
private:
	int FBufferCount;
	void __fastcall SetBufferCount(const int Value);
	
public:
	__fastcall virtual TGLMemoryViewer(System::Classes::TComponent* AOwner);
	void __fastcall InstantiateRenderingContext();
	virtual void __fastcall Render(TGLBaseSceneObject* baseObject = (TGLBaseSceneObject*)(0x0));
	
__published:
	__property int BufferCount = {read=FBufferCount, write=SetBufferCount, default=1};
public:
	/* TGLNonVisualViewer.Destroy */ inline __fastcall virtual ~TGLMemoryViewer() { }
	
};


typedef void __fastcall (*TInvokeInfoForm)(TGLSceneBuffer* aSceneBuffer, bool Modal);

//-- var, const, procedure ---------------------------------------------------
#define cDefaultProxyOptions (System::Set<TGLProxyObjectOption, TGLProxyObjectOption::pooEffects, TGLProxyObjectOption::pooTransformation>() << TGLProxyObjectOption::pooEffects << TGLProxyObjectOption::pooObjects << TGLProxyObjectOption::pooTransformation )
#define GLSCENE_REVISION L"$Revision: 2023$"
#define GLSCENE_VERSION L"v2.2 %s"
extern DELPHI_PACKAGE __int64 vCounterFrequency;
extern DELPHI_PACKAGE TGLBaseSceneObject* vCurrentRenderingObject;
extern DELPHI_PACKAGE TGLBaseSceneObject* __fastcall GetCurrentRenderingObject(void);
extern DELPHI_PACKAGE void __fastcall AxesBuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::Word pattern, float AxisLen);
extern DELPHI_PACKAGE void __fastcall RegisterInfoForm(TInvokeInfoForm infoForm);
extern DELPHI_PACKAGE void __fastcall InvokeInfoForm(TGLSceneBuffer* aSceneBuffer, bool Modal);
extern DELPHI_PACKAGE void __fastcall RegisterGLBaseSceneObjectNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
extern DELPHI_PACKAGE void __fastcall DeRegisterGLBaseSceneObjectNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
extern DELPHI_PACKAGE void __fastcall RegisterGLBehaviourNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
extern DELPHI_PACKAGE void __fastcall DeRegisterGLBehaviourNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
}	/* namespace Scene */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SCENE)
using namespace Gls::Scene;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SceneHPP
