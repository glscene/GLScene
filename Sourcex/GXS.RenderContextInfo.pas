//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.RenderContextInfo;

(* Stores contextual info useful during rendering methods *)

interface

{$I GXS.Scene.inc}

uses
  GXS.VectorTypes,
  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.State,
  GXS.PipelineTransformation,
  GXS.Color;

type

  TgxDrawState = (dsRendering, dsPicking, dsPrinting);

  TgxSize = record
    cx: Longint;
    cy: Longint;
  end;

  (* Determines if objects are sorted, and how. Sorting is done level by level (and not for all entities), values are :
   osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
   osNone : do not sort objects.
   osRenderFarthestFirst : render objects whose Position is the farthest from the camera first.
   osRenderBlendedLast : opaque objects are not sorted and rendered first, blended ones are rendered afterwards and depth sorted.
   osRenderNearestFirst : render objects whose Position is the nearest to the camera first.  *)
  TgxObjectsSorting = (osInherited, osNone,
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
  TgxVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical);

  TgxRenderContextClippingInfo = record
    origin: TVector4f;
    clippingDirection: TVector4f;
    viewPortRadius: Single; // viewport bounding radius per distance unit
    nearClippingDistance: Single;
    farClippingDistance: Single;
    frustum: TFrustum;
  end;

  // Stores contextual info useful during rendering methods.
  TgxRenderContextInfo = record
    scene: TObject; //usually TgxScene
    buffer: TObject; //usually TgxSceneBuffer
    cameraPosition: TVector4f;
    cameraDirection, cameraUp: TVector4f;
    viewPortSize: TgxSize;
    renderDPI: Integer;
    materialLibrary: TObject; //usually TgxMaterialLibrary;
    lightmapLibrary: TObject; //usually TgxMaterialLibrary;
    fogDisabledCounter: Integer;
    drawState: TgxDrawState;
    objectsSorting: TgxObjectsSorting;
    visibilityCulling: TgxVisibilityCulling;
    gxStates: TgxStateCache;
    PipelineTransformation: TGLTransformation;
    rcci: TgxRenderContextClippingInfo;
    sceneAmbientColor: TgxColorVector;
    bufferFaceCull: Boolean;
    bufferLighting: Boolean;
    bufferFog: Boolean;
    bufferDepthTest: Boolean;
    proxySubObject: Boolean;
    ignoreMaterials: Boolean;
    ignoreBlendingRequests: Boolean;
    ignoreDepthRequests: Boolean;
    amalgamating: Boolean;
    lights: TgxPersistentObjectList;
    afterRenderEffects: TgxPersistentObjectList;
    currentMaterialLevel: TgxMaterialLevel;
    primitiveMask: TgxMeshPrimitives;
    orderCounter: Integer;
  end;
  PRenderContextInfo = ^TgxRenderContextInfo;

//------------------------------------------
implementation
//------------------------------------------

end.

