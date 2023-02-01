// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.GizmoEx.pas' rev: 35.00 (Windows)

#ifndef Gls_GizmoexHPP
#define Gls_GizmoexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Color.hpp>
#include <GLS.Objects.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Material.hpp>
#include <GLS.Strings.hpp>
#include <GLS.GeomObjects.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Canvas.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Screen.hpp>
#include <GLS.State.hpp>
#include <GLS.Selection.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Gizmoex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLGizmoExObjectItem;
class DELPHICLASS TGLGizmoExObjectCollection;
class DELPHICLASS TGLGizmoExActionHistoryItem;
class DELPHICLASS TGLGizmoExActionHistoryCollection;
class DELPHICLASS TGLGizmoExUIFrustrum;
class DELPHICLASS TGLGizmoExUISphere;
class DELPHICLASS TGLGizmoExUIDisk;
class DELPHICLASS TGLGizmoExUITorus;
class DELPHICLASS TGLGizmoExUIPolygon;
class DELPHICLASS TGLGizmoExUIArrowLine;
class DELPHICLASS TGLGizmoExUILines;
class DELPHICLASS TGLGizmoExUIFlatText;
class DELPHICLASS TGLGizmoEx;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExObjectItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Gls::Vectortypes::TVector4f FOldAutoScaling;
	Gls::Scene::TGLBaseSceneObject* FEffectedObject;
	Gls::Scene::TGLBaseSceneObject* FParentOldObject;
	int FIndexOldObject;
	System::UnicodeString FNameOldObject;
	bool FReturnObject;
	Gls::Vectortypes::TMatrix4f FOldMatrix;
	Gls::Scene::TGLBaseSceneObject* FGizmoTmpRoot;
	void __fastcall SetEffectedObject(Gls::Scene::TGLBaseSceneObject* const Value);
	void __fastcall SetOldMatrix(const Gls::Vectortypes::TMatrix4f &Value);
	
protected:
	void __fastcall DoUndo();
	TGLGizmoExObjectCollection* __fastcall GetParent();
	TGLGizmoEx* __fastcall GetGizmo();
	
public:
	__property Gls::Scene::TGLBaseSceneObject* GizmoTmpRoot = {read=FGizmoTmpRoot, write=FGizmoTmpRoot};
	__fastcall virtual TGLGizmoExObjectItem(System::Classes::TCollection* AOwner);
	__fastcall virtual ~TGLGizmoExObjectItem();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall AssignFromObject(Gls::Scene::TGLBaseSceneObject* const AObject, bool AssignAndRemoveObj = false);
	__property Gls::Vectortypes::TMatrix4f OldMatrix = {read=FOldMatrix, write=SetOldMatrix};
	
__published:
	__property Gls::Scene::TGLBaseSceneObject* EffectedObject = {read=FEffectedObject, write=SetEffectedObject};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExObjectCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGizmoExObjectItem* operator[](const int Index) { return this->Items[Index]; }
	
private:
	TGLGizmoExObjectItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLGizmoExObjectItem* const Value);
	
protected:
	TGLGizmoEx* __fastcall GetParent();
	void __fastcall DoUndo();
	
public:
	void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RemoveByObject(Gls::Scene::TGLCustomSceneObject* const AObject);
	HIDESBASE TGLGizmoExObjectItem* __fastcall Add();
	__property TGLGizmoExObjectItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLGizmoExObjectCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGizmoExObjectCollection() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExActionHistoryItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::TObject* FObject;
	TGLGizmoExObjectCollection* FGizmoObjectCollection;
	void __fastcall SetObject(System::TObject* aValue);
	void __fastcall SetGizmoObjectCollection(TGLGizmoExObjectCollection* aValue);
	
public:
	__fastcall virtual TGLGizmoExActionHistoryItem(System::Classes::TCollection* AOwner);
	__fastcall virtual ~TGLGizmoExActionHistoryItem();
	__property System::TObject* BaseObject = {read=FObject, write=SetObject};
	__property TGLGizmoExObjectCollection* GizmoObjectCollection = {read=FGizmoObjectCollection, write=SetGizmoObjectCollection};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExActionHistoryCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGizmoExActionHistoryItem* operator[](const int Index) { return this->Items[Index]; }
	
private:
	int FItemIndex;
	int FItemsMaxCount;
	Gls::Scene::TGLBaseSceneObject* FGizmoTmpRoot;
	TGLGizmoExActionHistoryItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLGizmoExActionHistoryItem* const Value);
	HIDESBASE TGLGizmoExActionHistoryItem* __fastcall Add();
	
public:
	__fastcall TGLGizmoExActionHistoryCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass);
	void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property int ItemIndex = {read=FItemIndex, write=FItemIndex, nodefault};
	TGLGizmoExActionHistoryItem* __fastcall Undo();
	TGLGizmoExActionHistoryItem* __fastcall Redo();
	void __fastcall AddObjects(Gls::Selection::TGLPickList* objs);
	void __fastcall AddObject(System::TObject* obj);
	void __fastcall RemoveObjects(Gls::Selection::TGLPickList* objs);
	__property int MaxCount = {read=FItemsMaxCount, write=FItemsMaxCount, nodefault};
	__property TGLGizmoExActionHistoryItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
	__property Gls::Scene::TGLBaseSceneObject* GizmoTmpRoot = {read=FGizmoTmpRoot, write=FGizmoTmpRoot};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGizmoExActionHistoryCollection() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLGizmoExVisibleInfoLabel : unsigned char { vliName, vliOperation, vliCoords };

typedef System::Set<TGLGizmoExVisibleInfoLabel, TGLGizmoExVisibleInfoLabel::vliName, TGLGizmoExVisibleInfoLabel::vliCoords> TGLGizmoExVisibleInfoLabels;

enum DECLSPEC_DENUM TInfoLabelCoordType : unsigned char { ilcChanging, ilcChangeRate };

enum DECLSPEC_DENUM TGLGizmoExAxis : unsigned char { gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ, gaXYZ };

enum DECLSPEC_DENUM TGLGizmoExSelectionRegion : unsigned char { gsrRectangular, gsrCircular, gsrFence, gsrLasso };

enum DECLSPEC_DENUM TGLGizmoExReferenceCoordinateSystem : unsigned char { rcsView, rcsLocal };

typedef System::DynamicArray<System::Types::TPoint> TGLGizmoExSelRec;

enum DECLSPEC_DENUM TGLGizmoExOperation : unsigned char { gopMove, gopRotate, gopScale, gopNone };

enum DECLSPEC_DENUM TGLGizmoExOperationMode : unsigned char { gomNone, gomSelect, gomMove, gomRotate, gomScale };

typedef void __fastcall (__closure *TGLGizmoExAcceptEvent)(System::TObject* Sender, Gls::Selection::TGLPickList* &objs);

typedef void __fastcall (__closure *TGLGizmoExAxisSelected)(System::TObject* Sender, TGLGizmoExAxis &Axis);

enum DECLSPEC_DENUM TGLGizmoExPickMode : unsigned char { pmGetPickedObjects, pmRayCast };

class PASCALIMPLEMENTATION TGLGizmoExUIFrustrum : public Gls::Geomobjects::TGLFrustrum
{
	typedef Gls::Geomobjects::TGLFrustrum inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIFrustrum(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUIFrustrum() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIFrustrum(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Geomobjects::TGLFrustrum(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUISphere : public Gls::Objects::TGLSphere
{
	typedef Gls::Objects::TGLSphere inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUISphere(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUISphere() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUISphere(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLSphere(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIDisk : public Gls::Geomobjects::TGLDisk
{
	typedef Gls::Geomobjects::TGLDisk inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIDisk(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUIDisk() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIDisk(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Geomobjects::TGLDisk(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUITorus : public Gls::Geomobjects::TGLTorus
{
	typedef Gls::Geomobjects::TGLTorus inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUITorus(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUITorus() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUITorus(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Geomobjects::TGLTorus(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIPolygon : public Gls::Geomobjects::TGLPolygon
{
	typedef Gls::Geomobjects::TGLPolygon inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIPolygon(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLPolygon.Destroy */ inline __fastcall virtual ~TGLGizmoExUIPolygon() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIPolygon(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Geomobjects::TGLPolygon(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIArrowLine : public Gls::Geomobjects::TGLArrowLine
{
	typedef Gls::Geomobjects::TGLArrowLine inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIArrowLine(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUIArrowLine() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIArrowLine(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Geomobjects::TGLArrowLine(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUILines : public Gls::Objects::TGLLines
{
	typedef Gls::Objects::TGLLines inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUILines(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLLines.Destroy */ inline __fastcall virtual ~TGLGizmoExUILines() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUILines(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLLines(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIFlatText : public Gls::Bitmapfont::TGLFlatText
{
	typedef Gls::Bitmapfont::TGLFlatText inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIFlatText(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLFlatText.Destroy */ inline __fastcall virtual ~TGLGizmoExUIFlatText() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIFlatText(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Bitmapfont::TGLFlatText(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoEx : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Gls::Scene::TGLBaseSceneObject* FUIBaseGizmo;
	Gls::Scene::TGLBaseSceneObject* FUIRootHelpers;
	Gls::Scene::TGLBaseSceneObject* FUIRootSelect;
	Gls::Scene::TGLBaseSceneObject* FUIRootMovement;
	Gls::Scene::TGLBaseSceneObject* FUIRootRotate;
	Gls::Scene::TGLBaseSceneObject* FUIRootRotateAxisLabel;
	Gls::Scene::TGLBaseSceneObject* FUIRootScale;
	Gls::Scene::TGLBaseSceneObject* FUIRootAxisLabel;
	Gls::Scene::TGLBaseSceneObject* FUIRootVisibleInfoLabels;
	Gls::Scene::TGLDirectOpenGL* FInterfaceRender;
	Gls::Scene::TGLDirectOpenGL* FInternalRender;
	TGLGizmoExUILines* FUISelectLineX;
	TGLGizmoExUILines* FUISelectLineY;
	TGLGizmoExUILines* FUISelectLineZ;
	TGLGizmoExUIFrustrum* FUIICMovementLineX;
	TGLGizmoExUIFrustrum* FUIICMovementLineY;
	TGLGizmoExUIFrustrum* FUIICMovementLineZ;
	TGLGizmoExUIFrustrum* FUIICMovementLineXY;
	TGLGizmoExUIFrustrum* FUIICMovementLineXZ;
	TGLGizmoExUIFrustrum* FUIICMovementLineYZ;
	TGLGizmoExUIArrowLine* FUIMovementArrowX;
	TGLGizmoExUIArrowLine* FUIMovementArrowY;
	TGLGizmoExUIArrowLine* FUIMovementArrowZ;
	TGLGizmoExUILines* FUIMovementLineX;
	TGLGizmoExUILines* FUIMovementLineY;
	TGLGizmoExUILines* FUIMovementLineZ;
	TGLGizmoExUILines* FUIMovementLineXY;
	TGLGizmoExUILines* FUIMovementLineXZ;
	TGLGizmoExUILines* FUIMovementLineYZ;
	TGLGizmoExUIPolygon* FUIMovementPlaneXY;
	TGLGizmoExUIPolygon* FUIMovementPlaneXZ;
	TGLGizmoExUIPolygon* FUIMovementPlaneYZ;
	TGLGizmoExUILines* FUIRotateLineX;
	TGLGizmoExUILines* FUIRotateLineY;
	TGLGizmoExUILines* FUIRotateLineZ;
	TGLGizmoExUILines* FUIRotateLineXY;
	TGLGizmoExUILines* FUIRotateLineXZ;
	TGLGizmoExUITorus* FUIICRotateTorusX;
	TGLGizmoExUITorus* FUIICRotateTorusY;
	TGLGizmoExUITorus* FUIICRotateTorusZ;
	TGLGizmoExUITorus* FUIICRotateTorusXZ;
	TGLGizmoExUIDisk* FUIRotateDiskXY;
	TGLGizmoExUIDisk* FUIRotateDiskX;
	TGLGizmoExUIDisk* FUIRotateDiskX2;
	TGLGizmoExUIDisk* FUIRotateDiskY;
	TGLGizmoExUIDisk* FUIRotateDiskY2;
	TGLGizmoExUIDisk* FUIRotateDiskZ;
	TGLGizmoExUIDisk* FUIRotateDiskZ2;
	TGLGizmoExUILines* FUIRotateLineArrowX;
	TGLGizmoExUILines* FUIRotateLineArrowY;
	TGLGizmoExUILines* FUIRotateLineArrowZ;
	TGLGizmoExUISphere* FUIICRotateSphereXY;
	TGLGizmoExUIFlatText* FUIRotateAxisLabelX;
	TGLGizmoExUIFlatText* FUIRotateAxisLabelY;
	TGLGizmoExUIFlatText* FUIRotateAxisLabelZ;
	TGLGizmoExUISphere* FUIScaleArrowX;
	TGLGizmoExUISphere* FUIScaleArrowY;
	TGLGizmoExUISphere* FUIScaleArrowZ;
	TGLGizmoExUILines* FUIScaleLineX;
	TGLGizmoExUILines* FUIScaleLineY;
	TGLGizmoExUILines* FUIScaleLineZ;
	TGLGizmoExUILines* FUIScaleLineXY;
	TGLGizmoExUILines* FUIScaleLineYZ;
	TGLGizmoExUILines* FUIScaleLineXZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineX;
	TGLGizmoExUIFrustrum* FUIICScaleLineY;
	TGLGizmoExUIFrustrum* FUIICScaleLineZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineXY;
	TGLGizmoExUIFrustrum* FUIICScaleLineXZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineYZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineXYZ;
	TGLGizmoExUIPolygon* FUIScalePlaneXY;
	TGLGizmoExUIPolygon* FUIScalePlaneXZ;
	TGLGizmoExUIPolygon* FUIScalePlaneYZ;
	TGLGizmoExUIPolygon* FUIScalePlaneXYZ;
	TGLGizmoExUIFlatText* FUIAxisLabelX;
	TGLGizmoExUIFlatText* FUIAxisLabelY;
	TGLGizmoExUIFlatText* FUIAxisLabelZ;
	TGLGizmoExUIFlatText* FUIVisibleInfoLabels;
	Gls::Scene::TGLBaseSceneObject* FRootGizmo;
	Gls::Scene::TGLBaseSceneObject* FRootObjects;
	Gls::Scene::TGLBaseSceneObject* FGizmoTmpRoot;
	Gls::Scene::TGLBaseSceneObject* FSelectedObj;
	TGLGizmoExOperation FOperation;
	TGLGizmoExOperationMode FOperationMode;
	TGLGizmoExAxis FSelAxis;
	TInfoLabelCoordType fInfoLabelCoordType;
	TGLGizmoExReferenceCoordinateSystem FReferenceCoordSystem;
	Gls::Color::TGLColor* FBoundingBoxColor;
	Gls::Color::TGLColor* FSelectedColor;
	Gls::Color::TGLColor* FVisibleInfoLabelsColor;
	Gls::Color::TGLColor* FSelectionRegionColor;
	bool FVisibleInfoLabelsColorChanged;
	bool FAutoZoom;
	bool FExcludeObjects;
	bool FExcludeClassname;
	bool FNoZWrite;
	bool FEnabled;
	float FAutoZoomFactor;
	float FZoomFactor;
	float FMoveCoef;
	float FRotationCoef;
	Gls::Sceneviewer::TGLSceneViewer* FViewer;
	TGLGizmoExVisibleInfoLabels FVisibleVisibleInfoLabels;
	System::Classes::TStrings* FExcludeObjectsList;
	System::Classes::TStrings* FExcludeClassNameList;
	TGLGizmoExSelectionRegion FSelectionRegion;
	bool FEnableMultiSelection;
	bool FShowMultiSelecting;
	TGLGizmoExSelRec FSelectionRec;
	bool FCanAddObjToSelectionList;
	bool FCanRemoveObjFromSelectionList;
	Gls::Selection::TGLPickList* FSelectedObjects;
	bool FAntiAliasedLines;
	bool FShowAxisLabel;
	bool FShowObjectInfos;
	bool FShowBoundingBox;
	bool FCanChangeWithChildren;
	bool moving;
	int mx;
	int my;
	System::Types::TPoint fCursorPos;
	System::Types::TPoint fLastCursorPos;
	Gls::Vectortypes::TVector3f fChangeRate;
	bool FEnableLoopCursorMoving;
	Gls::Vectortypes::TVector4f lastMousePos;
	System::Classes::TNotifyEvent FOnUpdate;
	TGLGizmoExAcceptEvent FOnSelect;
	System::Classes::TNotifyEvent FOnOperationChange;
	System::Classes::TNotifyEvent FOnOperationModeChange;
	System::Classes::TNotifyEvent FOnSelectionLost;
	TGLGizmoExAxisSelected FOnAxisSelected;
	float FScaleCoef;
	float FGizmoThickness;
	TGLGizmoExPickMode FPickMode;
	bool FEnableHistory;
	TGLGizmoExActionHistoryCollection* FHistory;
	int FHistoryStepsCount;
	Gls::Bitmapfont::TGLCustomBitmapFont* FLabelFont;
	void __fastcall SetRootGizmo(Gls::Scene::TGLBaseSceneObject* const AValue);
	void __fastcall SetRootObjects(Gls::Scene::TGLBaseSceneObject* const AValue);
	void __fastcall SetGizmoTmpRoot(Gls::Scene::TGLBaseSceneObject* const AValue);
	void __fastcall SeTGLGizmoExVisibleInfoLabels(const TGLGizmoExVisibleInfoLabels AValue);
	void __fastcall SetBoundingBoxColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetSelectedColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetVisibleInfoLabelsColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetSelectionRegionColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetCanChangeWithChildren(bool AValue);
	void __fastcall SetAALines(bool aValue);
	void __fastcall SetInfoLabelCoordType(TInfoLabelCoordType aValue);
	void __fastcall SetReferenceCoordSystem(TGLGizmoExReferenceCoordinateSystem aValue);
	void __fastcall SetHistoryStepsCount(int aValue);
	void __fastcall SetExcludeObjectsList(System::Classes::TStrings* const AValue);
	void __fastcall SetExcludeClassNameList(System::Classes::TStrings* const AValue);
	Gls::Vectortypes::TVector4f __fastcall MouseWorldPos(const int X, const int Y);
	bool __fastcall CheckObjectInExcludeList(Gls::Scene::TGLBaseSceneObject* const Obj);
	bool __fastcall CheckClassNameInExcludeList(Gls::Scene::TGLBaseSceneObject* const Obj);
	void __fastcall UpdateVisibleInfoLabels();
	void __fastcall SetGLGizmoExThickness(const float Value);
	void __fastcall ActivatingElements(Gls::Selection::TGLPickList* PickList);
	void __fastcall InterfaceRender(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall InternalRender(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	Gls::Selection::TGLPickList* __fastcall InternalGetPickedObjects(const int x1, const int y1, const int x2, const int y2, const int guessCount = 0x8);
	void __fastcall SetViewer(Gls::Sceneviewer::TGLSceneViewer* const Value);
	void __fastcall SetLabelFont(Gls::Bitmapfont::TGLCustomBitmapFont* const Value);
	void __fastcall SetSelectedObj(Gls::Scene::TGLBaseSceneObject* const Value);
	Gls::Scene::TGLBaseSceneObject* __fastcall GetSelectedObj();
	void __fastcall SetNoZWrite(const bool Value);
	void __fastcall SetOperation(const TGLGizmoExOperation Value);
	void __fastcall SetOperationMode(const TGLGizmoExOperationMode Value);
	void __fastcall SetAngleDisk(float aAngle);
	void __fastcall SetEnableLoopCursorMoving(const bool AValue);
	void __fastcall SetEnableMultiSelection(const bool AValue);
	void __fastcall SetSelectionRegion(const TGLGizmoExSelectionRegion AValue);
	void __fastcall SetShowAxisLabel(const bool AValue);
	void __fastcall SetShowObjectInfos(const bool AValue);
	void __fastcall SetShowBoundingBox(const bool AValue);
	void __fastcall SetAutoZoomFactor(const float AValue);
	void __fastcall SetZoomFactor(const float AValue);
	void __fastcall SetSelAxis(TGLGizmoExAxis aValue);
	void __fastcall SetPickMode(TGLGizmoExPickMode APickMode);
	void __fastcall AssignPickList(Gls::Selection::TGLPickList* aList, bool RemoveObj = false);
	void __fastcall AddObjToSelectionList(Gls::Scene::TGLBaseSceneObject* Obj);
	void __fastcall RemoveObjFromSelectionList(Gls::Scene::TGLBaseSceneObject* Obj);
	void __fastcall MultiSelMouseDown(int X, int Y);
	void __fastcall MultiSelMouseUp(int X, int Y);
	void __fastcall MultiSelMouseMove(int X, int Y);
	Gls::Selection::TGLPickList* __fastcall GetPickList();
	void __fastcall SetPickList(Gls::Selection::TGLPickList* aValue);
	__property TGLGizmoExAxis SelAxis = {read=FSelAxis, write=SetSelAxis, nodefault};
	__property TGLGizmoExOperation Operation = {read=FOperation, write=SetOperation, nodefault};
	void __fastcall ClearSelection();
	void __fastcall SetVisible(const bool AValue);
	bool __fastcall GetVisible();
	
public:
	__fastcall virtual TGLGizmoEx(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGizmoEx();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ViewerMouseMove(const int X, const int Y);
	void __fastcall ViewerMouseDown(const int X, const int Y);
	void __fastcall ViewerMouseUp(const int X, const int Y);
	void __fastcall UpdateGizmo()/* overload */;
	virtual void __fastcall LooseSelection();
	void __fastcall UndoAdd(System::TObject* const AObject);
	void __fastcall RemoveSelectedObjects();
	TGLGizmoExActionHistoryItem* __fastcall Undo();
	TGLGizmoExActionHistoryItem* __fastcall Redo();
	__property bool CanAddObjToSelectionList = {read=FCanAddObjToSelectionList, write=FCanAddObjToSelectionList, nodefault};
	__property bool CanRemoveObjFromSelectionList = {read=FCanRemoveObjFromSelectionList, write=FCanRemoveObjFromSelectionList, nodefault};
	void __fastcall LooseCursorSelection();
	__property bool CursorSelectingRegion = {read=FShowMultiSelecting, nodefault};
	__property Gls::Scene::TGLBaseSceneObject* RootObjects = {read=FRootObjects, write=SetRootObjects};
	__property Gls::Scene::TGLBaseSceneObject* RootGizmo = {read=FRootGizmo, write=SetRootGizmo};
	__property Gls::Scene::TGLBaseSceneObject* GizmoTmpRoot = {read=FGizmoTmpRoot, write=SetGizmoTmpRoot};
	
__published:
	__property Gls::Sceneviewer::TGLSceneViewer* Viewer = {read=FViewer, write=SetViewer};
	__property Gls::Color::TGLColor* BoundingBoxColor = {read=FBoundingBoxColor, write=SetBoundingBoxColor};
	__property Gls::Color::TGLColor* SelectedColor = {read=FSelectedColor, write=SetSelectedColor};
	__property Gls::Color::TGLColor* SelectionRegionColor = {read=FSelectionRegionColor, write=SetSelectionRegionColor};
	__property Gls::Scene::TGLBaseSceneObject* SelectedObj = {read=GetSelectedObj, write=SetSelectedObj};
	__property Gls::Selection::TGLPickList* SelectedObjects = {read=GetPickList, write=SetPickList};
	__property TGLGizmoExOperationMode OperationMode = {read=FOperationMode, write=SetOperationMode, default=1};
	__property bool ExcludeObjects = {read=FExcludeObjects, write=FExcludeObjects, nodefault};
	__property System::Classes::TStrings* ExcludeObjectsList = {read=FExcludeObjectsList, write=SetExcludeObjectsList};
	__property bool ExcludeClassname = {read=FExcludeClassname, write=FExcludeClassname, nodefault};
	__property System::Classes::TStrings* ExcludeClassnameList = {read=FExcludeClassNameList, write=SetExcludeClassNameList};
	__property TGLGizmoExVisibleInfoLabels VisibleInfoLabels = {read=FVisibleVisibleInfoLabels, write=SeTGLGizmoExVisibleInfoLabels, nodefault};
	__property Gls::Color::TGLColor* VisibleInfoLabelsColor = {read=FVisibleInfoLabelsColor, write=SetVisibleInfoLabelsColor};
	__property bool AutoZoom = {read=FAutoZoom, write=FAutoZoom, default=1};
	__property float AutoZoomFactor = {read=FAutoZoomFactor, write=SetAutoZoomFactor};
	__property float ZoomFactor = {read=FZoomFactor, write=SetZoomFactor};
	__property float MoveCoef = {read=FMoveCoef, write=FMoveCoef};
	__property float RotationCoef = {read=FRotationCoef, write=FRotationCoef};
	__property float ScaleCoef = {read=FScaleCoef, write=FScaleCoef};
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, default=1};
	__property float GizmoThickness = {read=FGizmoThickness, write=SetGLGizmoExThickness};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property Gls::Bitmapfont::TGLCustomBitmapFont* LabelFont = {read=FLabelFont, write=SetLabelFont, default=0};
	__property System::Classes::TNotifyEvent OnSelectionLost = {read=FOnSelectionLost, write=FOnSelectionLost};
	__property System::Classes::TNotifyEvent OnOperationChange = {read=FOnOperationChange, write=FOnOperationChange};
	__property System::Classes::TNotifyEvent OnOperationModeChange = {read=FOnOperationModeChange, write=FOnOperationModeChange};
	__property TGLGizmoExAcceptEvent OnSelect = {read=FOnSelect, write=FOnSelect};
	__property TGLGizmoExAxisSelected OnAxisSelected = {read=FOnAxisSelected, write=FOnAxisSelected};
	__property System::Classes::TNotifyEvent OnUpdate = {read=FOnUpdate, write=FOnUpdate};
	__property TGLGizmoExPickMode PickMode = {read=FPickMode, write=SetPickMode, default=0};
	__property bool EnableActionHistory = {read=FEnableHistory, write=FEnableHistory, default=1};
	__property int HistoryStepsCount = {read=FHistoryStepsCount, write=SetHistoryStepsCount, nodefault};
	__property bool EnableLoopCursorMoving = {read=FEnableLoopCursorMoving, write=SetEnableLoopCursorMoving, default=1};
	__property bool EnableMultiSelection = {read=FEnableMultiSelection, write=SetEnableMultiSelection, default=1};
	__property bool CanChangeWithChildren = {read=FCanChangeWithChildren, write=SetCanChangeWithChildren, nodefault};
	__property bool AntiAliasedLines = {read=FAntiAliasedLines, write=SetAALines, default=1};
	__property TInfoLabelCoordType InfoLabelCoordType = {read=fInfoLabelCoordType, write=SetInfoLabelCoordType, default=1};
	__property TGLGizmoExSelectionRegion SelectionRegion = {read=FSelectionRegion, write=SetSelectionRegion, default=0};
	__property bool ShowAxisLabel = {read=FShowAxisLabel, write=SetShowAxisLabel, default=1};
	__property bool ShowObjectInfos = {read=FShowObjectInfos, write=SetShowObjectInfos, default=1};
	__property bool ShowBoundingBox = {read=FShowBoundingBox, write=SetShowBoundingBox, default=1};
	__property TGLGizmoExReferenceCoordinateSystem ReferenceCoordSystem = {read=FReferenceCoordSystem, write=SetReferenceCoordSystem, default=0};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gizmoex */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_GIZMOEX)
using namespace Gls::Gizmoex;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_GizmoexHPP
