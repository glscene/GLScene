//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.RenderContextInfo;

(* Stores contextual info useful during rendering methods *)

interface

{$I GLScene.inc}

uses
  GLS.PersistentClasses,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.State,
  GLS.PipelineTransformation,
  GLS.Color;

type

  TDrawState = (dsRendering, dsPicking, dsPrinting);

  TGLSize = record
    cx: Longint;
    cy: Longint;
  end;

  (* Determines if objects are sorted, and how. Sorting is done level by level (and not for all entities), values are :
   osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
   osNone : do not sort objects.
   osRenderFarthestFirst : render objects whose Position is the farthest from the camera first.
   osRenderBlendedLast : opaque objects are not sorted and rendered first, blended ones are rendered afterwards and depth sorted.
   osRenderNearestFirst : render objects whose Position is the nearest to the camera first.  *)
  TGLObjectsSorting = (osInherited, osNone,
    osRenderFarthestFirst, osRenderBlendedLast,
    osRenderNearestFirst);

  (* Determines the visibility culling mode.
     Culling is done level by level, allowed values are:
      vcInherited : use inherited culling value, if selected for the root level, defaults to vcNone
      vcNone : no visibility culling is performed
      vcObjectBased : culling is done on a per-object basis, each object may
        or may not be culled base on its own AxisAlignedDimensions,
        culling has no impact on the visibility of its children
      vcHierarchical : culling is performed hierarchically, using hierarchical
        bounding boxes, if a parent is culled, all of its children, whatever their
        culling options are invisible.
     Depending on the structure of your scene the most efficient culling
     method will be either vcObjectBased or vcHierarchical. Also note that if
     you use many objects with "static" geometry and have a T&amp;L graphics
     board, it may be faster not to cull at all (ie. leave this to the hardware). *)
  TGLVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical);

  TRenderContextClippingInfo = record
    Origin: TGLVector;
    ClippingDirection: TGLVector;
    ViewPortRadius: Single; // viewport bounding radius per distance unit
    NearClippingDistance: Single;
    FarClippingDistance: Single;
    Frustum: TFrustum;
  end;

  // Stores contextual info useful during rendering methods.
  TGLRenderContextInfo = record
    Scene: TObject; //usually TGLScene
    Buffer: TObject; //usually TGLSceneBuffer
    CameraPosition: TGLVector;
    CameraDirection, CameraUp: TGLVector;
    ViewPortSize: TGLSize;
    RenderDPI: Integer;
    MaterialLibrary: TObject; //usually TGLMaterialLibrary;
    LightMapLibrary: TObject; //usually TGLMaterialLibrary;
    FogDisabledCounter: Integer;
    DrawState: TDrawState;
    ObjectsSorting: TGLObjectsSorting;
    VisibilityCulling: TGLVisibilityCulling;
    GLStates: TGLStateCache;
    PipelineTransformation: TGLTransformation;
    Rcci: TRenderContextClippingInfo;
    SceneAmbientColor: TGLColorVector;
    BufferFaceCull: Boolean;
    BufferLighting: Boolean;
    BufferFog: Boolean;
    BufferDepthTest: Boolean;
    ProxySubObject: Boolean;
    IgnoreMaterials: Boolean;
    IgnoreBlendingRequests: Boolean;
    IgnoreDepthRequests: Boolean;
    Amalgamating: Boolean;
    Lights: TGLPersistentObjectList;
    AfterRenderEffects: TGLPersistentObjectList;
    CurrentMaterialLevel: TGLMaterialLevel;
    PrimitiveMask: TGLMeshPrimitives;
    OrderCounter: Integer;
  end;
  PRenderContextInfo = ^TGLRenderContextInfo;

//====================================================================
implementation
//====================================================================

end.




