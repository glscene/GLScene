// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Gizmo.pas' rev: 35.00 (Windows)

#ifndef Gls_GizmoHPP
#define Gls_GizmoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLS.Scene.hpp>
#include <GLS.PersistentClasses.hpp>
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
#include <GLS.State.hpp>
#include <GLS.Selection.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Gizmo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLGizmoUndoItem;
class DELPHICLASS TGLGizmoUndoCollection;
class DELPHICLASS TGLGizmoRayCastHitData;
class DELPHICLASS TGLGizmoPickCube;
class DELPHICLASS TGLGizmoPickTorus;
class DELPHICLASS TGLGizmo;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLGizmoUndoItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FOldLibMaterialName;
	Gls::Coordinates::TGLCoordinates3* FOldAutoScaling;
	Gls::Scene::TGLCustomSceneObject* FEffectedObject;
	Gls::Vectortypes::TMatrix4f FOldMatr;
	Gls::Vectortypes::TMatrix4f FOldMatrix;
	void __fastcall SetEffectedObject(Gls::Scene::TGLCustomSceneObject* const Value);
	void __fastcall SetOldAutoScaling(Gls::Coordinates::TGLCoordinates3* const Value);
	void __fastcall SetOldMatrix(const Gls::Vectortypes::TMatrix4f &Value);
	
protected:
	virtual void __fastcall DoUndo();
	TGLGizmoUndoCollection* __fastcall GetParent();
	TGLGizmo* __fastcall GetGizmo();
	
public:
	__fastcall virtual TGLGizmoUndoItem(System::Classes::TCollection* AOwner);
	__fastcall virtual ~TGLGizmoUndoItem();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall AssignFromObject(Gls::Scene::TGLCustomSceneObject* const AObject);
	__property Gls::Vectortypes::TMatrix4f OldMatrix = {read=FOldMatrix, write=SetOldMatrix};
	
__published:
	__property Gls::Scene::TGLCustomSceneObject* EffectedObject = {read=FEffectedObject, write=SetEffectedObject};
	__property Gls::Coordinates::TGLCoordinates3* OldAutoScaling = {read=FOldAutoScaling, write=SetOldAutoScaling};
	__property System::UnicodeString OldLibMaterialName = {read=FOldLibMaterialName, write=FOldLibMaterialName};
};


class PASCALIMPLEMENTATION TGLGizmoUndoCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGizmoUndoItem* operator[](const int Index) { return this->Items[Index]; }
	
private:
	TGLGizmoUndoItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLGizmoUndoItem* const Value);
	
protected:
	TGLGizmo* __fastcall GetParent();
	
public:
	void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RemoveByObject(Gls::Scene::TGLCustomSceneObject* const AObject);
	HIDESBASE TGLGizmoUndoItem* __fastcall Add();
	__property TGLGizmoUndoItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLGizmoUndoCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGizmoUndoCollection() { }
	
};


enum DECLSPEC_DENUM TGLGizmoElement : unsigned char { geMove, geRotate, geScale, geAxisLabel, geObjectInfos, geBoundingBox };

typedef System::Set<TGLGizmoElement, TGLGizmoElement::geMove, TGLGizmoElement::geBoundingBox> TGLGizmoElements;

enum DECLSPEC_DENUM TGLGizmoVisibleInfoLabel : unsigned char { vliName, vliOperation, vliCoords };

typedef System::Set<TGLGizmoVisibleInfoLabel, TGLGizmoVisibleInfoLabel::vliName, TGLGizmoVisibleInfoLabel::vliCoords> TGLGizmoVisibleInfoLabels;

enum DECLSPEC_DENUM TGLGizmoAxis : unsigned char { gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ };

enum DECLSPEC_DENUM TGLGizmoOperation : unsigned char { gopMove, gopRotate, gopScale, gopNone, gpMoveGizmo, gpRotateGizmo };

typedef void __fastcall (__closure *TGLGizmoAcceptEvent)(System::TObject* Sender, Gls::Scene::TGLBaseSceneObject* &Obj, bool &Accept, Gls::Vectortypes::TVector4f &Dimensions);

typedef void __fastcall (__closure *TGLGizmoUpdateEvent)(System::TObject* Sender, Gls::Scene::TGLBaseSceneObject* Obj, TGLGizmoAxis Axis, TGLGizmoOperation Operation, Gls::Vectortypes::TVector4f &Vector);

enum DECLSPEC_DENUM TGLGizmoPickMode : unsigned char { pmGetPickedObjects, pmRayCast };

class PASCALIMPLEMENTATION TGLGizmoRayCastHitData : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	Gls::Scene::TGLBaseSceneObject* Obj;
	Gls::Vectortypes::TVector4f Point;
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLGizmoRayCastHitData() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLGizmoRayCastHitData() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TGLGizmoPickCube : public Gls::Objects::TGLCube
{
	typedef Gls::Objects::TGLCube inherited;
	
public:
	/* TGLCube.Create */ inline __fastcall virtual TGLGizmoPickCube(System::Classes::TComponent* AOwner) : Gls::Objects::TGLCube(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoPickCube() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoPickCube(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLCube(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoPickTorus : public Gls::Geomobjects::TGLTorus
{
	typedef Gls::Geomobjects::TGLTorus inherited;
	
public:
	/* TGLTorus.Create */ inline __fastcall virtual TGLGizmoPickTorus(System::Classes::TComponent* AOwner) : Gls::Geomobjects::TGLTorus(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoPickTorus() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoPickTorus(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Geomobjects::TGLTorus(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmo : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Gls::Scene::TGLBaseSceneObject* _GZObaseGizmo;
	Gls::Objects::TGLCube* _GZOBoundingcube;
	Gls::Scene::TGLBaseSceneObject* _GZOrootHelpers;
	Gls::Scene::TGLBaseSceneObject* _GZOrootLines;
	Gls::Scene::TGLBaseSceneObject* _GZOrootTorus;
	Gls::Scene::TGLBaseSceneObject* _GZOrootCubes;
	Gls::Scene::TGLBaseSceneObject* _GZORootAxisLabel;
	Gls::Scene::TGLBaseSceneObject* _GZORootVisibleInfoLabels;
	Gls::Objects::TGLLines* _GZOlineX;
	Gls::Objects::TGLLines* _GZOlineY;
	Gls::Objects::TGLLines* _GZOlineZ;
	Gls::Objects::TGLLines* _GZOplaneXY;
	Gls::Objects::TGLLines* _GZOplaneXZ;
	Gls::Objects::TGLLines* _GZOplaneYZ;
	TGLGizmoPickTorus* _GZOTorusX;
	TGLGizmoPickTorus* _GZOTorusY;
	TGLGizmoPickTorus* _GZOTorusZ;
	TGLGizmoPickCube* _GZOCubeX;
	TGLGizmoPickCube* _GZOCubeY;
	TGLGizmoPickCube* _GZOCubeZ;
	Gls::Bitmapfont::TGLFlatText* _GZOAxisLabelX;
	Gls::Bitmapfont::TGLFlatText* _GZOAxisLabelY;
	Gls::Bitmapfont::TGLFlatText* _GZOAxisLabelZ;
	Gls::Bitmapfont::TGLFlatText* _GZOVisibleInfoLabels;
	Gls::Scene::TGLBaseSceneObject* FRootGizmo;
	Gls::Scene::TGLBaseSceneObject* FSelectedObj;
	TGLGizmoOperation FOperation;
	TGLGizmoAxis FSelAxis;
	Gls::Color::TGLColor* FBoundingBoxColor;
	Gls::Color::TGLColor* FSelectedColor;
	Gls::Color::TGLColor* FVisibleInfoLabelsColor;
	bool FBoundingBoxColorChanged;
	bool FVisibleInfoLabelsColorChanged;
	bool FForceOperation;
	bool FForceAxis;
	bool FForceUniformScale;
	bool FAutoZoom;
	bool FExcludeObjects;
	bool FNoZWrite;
	bool FEnabled;
	float FAutoZoomFactor;
	float FZoomFactor;
	float FMoveCoef;
	float FRotationCoef;
	Gls::Sceneviewer::TGLSceneViewer* FViewer;
	TGLGizmoElements FGizmoElements;
	TGLGizmoVisibleInfoLabels FVisibleVisibleInfoLabels;
	System::Classes::TStrings* FExcludeObjectsList;
	bool Moving;
	int Mx;
	int My;
	int Rx;
	int Ry;
	Gls::Scene::TGLDirectOpenGL* dglEnable;
	Gls::Scene::TGLDirectOpenGL* dglDisable;
	Gls::Scene::TGLDirectOpenGL* dgtEnable;
	Gls::Scene::TGLDirectOpenGL* dgtDisable;
	Gls::Scene::TGLDirectOpenGL* dgcEnable;
	Gls::Scene::TGLDirectOpenGL* dgcDisable;
	Gls::Scene::TGLDirectOpenGL* dglaEnable;
	Gls::Scene::TGLDirectOpenGL* dglaDisable;
	Gls::Scene::TGLDirectOpenGL* dgliEnable;
	Gls::Scene::TGLDirectOpenGL* dgliDisable;
	Gls::Vectortypes::TVector4f LastMousePos;
	Gls::Vectortypes::TVector4f ObjDimensions;
	TGLGizmoAcceptEvent FOnBeforeSelect;
	TGLGizmoUpdateEvent FOnBeforeUpdate;
	System::Classes::TNotifyEvent FOnSelectionLost;
	float FScaleCoef;
	float FGizmoThickness;
	TGLGizmoPickMode FPickMode;
	System::Classes::TList* FInternalRaycastHitData;
	TGLGizmoUndoCollection* FUndoHistory;
	Gls::Bitmapfont::TGLCustomBitmapFont* FLabelFont;
	void __fastcall SetRootGizmo(Gls::Scene::TGLBaseSceneObject* const AValue);
	void __fastcall SetGizmoElements(const TGLGizmoElements AValue);
	void __fastcall SeTGLGizmoVisibleInfoLabels(const TGLGizmoVisibleInfoLabels AValue);
	void __fastcall SetBoundingBoxColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetSelectedColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetVisibleInfoLabelsColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetExcludeObjectsList(System::Classes::TStrings* const AValue);
	void __fastcall DirectGlDisable(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &Rci);
	void __fastcall DirectGlEnable(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &Rci);
	Gls::Vectortypes::TVector4f __fastcall MouseWorldPos(const int X, const int Y);
	bool __fastcall CheckObjectInExcludeList(Gls::Scene::TGLBaseSceneObject* const Obj);
	void __fastcall UpdateVisibleInfoLabels();
	void __fastcall SetGLGizmoThickness(const float Value);
	Gls::Selection::TGLPickList* __fastcall InternalGetPickedObjects(const int X1, const int Y1, const int X2, const int Y2, const int GuessCount = 0x8);
	void __fastcall ClearInternalRaycastHitData();
	void __fastcall SetViewer(Gls::Sceneviewer::TGLSceneViewer* const Value);
	void __fastcall SetLabelFont(Gls::Bitmapfont::TGLCustomBitmapFont* const Value);
	void __fastcall SetSelectedObj(Gls::Scene::TGLBaseSceneObject* const Value);
	
public:
	System::Classes::TList* PickableObjectsWithRayCast;
	__fastcall virtual TGLGizmo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGizmo();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ViewerMouseMove(const int X, const int Y);
	void __fastcall ViewerMouseDown(const int X, const int Y);
	void __fastcall ViewerMouseUp(const int X, const int Y);
	void __fastcall UpdateGizmo()/* overload */;
	void __fastcall UpdateGizmo(const Gls::Vectortypes::TVector4f &NewDimensions)/* overload */;
	void __fastcall SetVisible(const bool AValue);
	Gls::Vectortypes::TVector4f __fastcall GetPickedObjectPoint(Gls::Scene::TGLBaseSceneObject* const Obj);
	virtual void __fastcall LooseSelection();
	void __fastcall UndoAdd(Gls::Scene::TGLCustomSceneObject* const AObject);
	__property Gls::Scene::TGLBaseSceneObject* RootGizmo = {read=FRootGizmo, write=SetRootGizmo};
	
__published:
	__property Gls::Sceneviewer::TGLSceneViewer* Viewer = {read=FViewer, write=SetViewer};
	__property TGLGizmoElements GizmoElements = {read=FGizmoElements, write=SetGizmoElements, nodefault};
	__property Gls::Color::TGLColor* BoundingBoxColor = {read=FBoundingBoxColor, write=SetBoundingBoxColor};
	__property Gls::Color::TGLColor* SelectedColor = {read=FSelectedColor, write=SetSelectedColor};
	__property TGLGizmoAxis SelAxis = {read=FSelAxis, write=FSelAxis, nodefault};
	__property bool ForceAxis = {read=FForceAxis, write=FForceAxis, nodefault};
	__property Gls::Scene::TGLBaseSceneObject* SelectedObj = {read=FSelectedObj, write=SetSelectedObj};
	__property TGLGizmoOperation Operation = {read=FOperation, write=FOperation, nodefault};
	__property bool ForceOperation = {read=FForceOperation, write=FForceOperation, nodefault};
	__property bool ForceUniformScale = {read=FForceUniformScale, write=FForceUniformScale, nodefault};
	__property bool ExcludeObjects = {read=FExcludeObjects, write=FExcludeObjects, nodefault};
	__property System::Classes::TStrings* ExcludeObjectsList = {read=FExcludeObjectsList, write=SetExcludeObjectsList};
	__property TGLGizmoVisibleInfoLabels VisibleInfoLabels = {read=FVisibleVisibleInfoLabels, write=SeTGLGizmoVisibleInfoLabels, nodefault};
	__property Gls::Color::TGLColor* VisibleInfoLabelsColor = {read=FVisibleInfoLabelsColor, write=SetVisibleInfoLabelsColor};
	__property bool AutoZoom = {read=FAutoZoom, write=FAutoZoom, nodefault};
	__property float AutoZoomFactor = {read=FAutoZoomFactor, write=FAutoZoomFactor};
	__property float ZoomFactor = {read=FZoomFactor, write=FZoomFactor};
	__property float MoveCoef = {read=FMoveCoef, write=FMoveCoef};
	__property float RotationCoef = {read=FRotationCoef, write=FRotationCoef};
	__property float ScaleCoef = {read=FScaleCoef, write=FScaleCoef};
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
	__property float GizmoThickness = {read=FGizmoThickness, write=SetGLGizmoThickness};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=0};
	__property Gls::Bitmapfont::TGLCustomBitmapFont* LabelFont = {read=FLabelFont, write=SetLabelFont, default=0};
	__property TGLGizmoAcceptEvent OnBeforeSelect = {read=FOnBeforeSelect, write=FOnBeforeSelect};
	__property System::Classes::TNotifyEvent OnSelectionLost = {read=FOnSelectionLost, write=FOnSelectionLost};
	__property TGLGizmoUpdateEvent OnBeforeUpdate = {read=FOnBeforeUpdate, write=FOnBeforeUpdate};
	__property TGLGizmoPickMode PickMode = {read=FPickMode, write=FPickMode, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gizmo */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_GIZMO)
using namespace Gls::Gizmo;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_GizmoHPP
