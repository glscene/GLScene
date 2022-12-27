//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.SmartObjects;

(*
  The smart spatial objects that have sences and artifitial intelligence
  to interact with player, base geometric and other smart objects of GLScene:
  TGLGerm, TGLSmartCells, TGLSmartSwarm, TGLCyborg, TGLCyborgNet
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  VCL.Consts,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorTypesExt,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.Silhouette,
  GLS.Strings,
  GLS.Texture,
  GLS.Material,
  GLS.Mesh,
  GLS.Logger,
  GLS.Octree,
  GLS.GeometryBB,
  GLS.ApplicationFileIO,
  GLS.Context,
  GLS.Color,
  GLS.PipelineTransformation,
  GLS.Selection,
  GLS.RenderContextInfo,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.VectorFileObjects;


type

  TGLCyborgSmartReference = (csrNone, csrWeak, csrStrong);
  TGLCyborgThinkingMode = (ctmSelf, ctmSleep, ctmOutside, ctmZombie, ctmDeath);

  TGLCyborgOption = (coCollide, coContact, coJoin);
  TGLCyborgSenceOrgans = (csoVision, csoHearing, csoSmell, csoTouch, taste);

  TGLCyborgOptions = set of TGLCyborgOption;
  TGLCyborgThinks = class(TCollection);

  // A list of thinking periods for TGLCyborgThinkingMode
  TGLCyborgThinksList = class(TGLPersistentObjectList);

const
  cDefaultCyborgOptions = [coCollide];

type
  (* The Cyborg class specialized as a smart actor with AI.
    The TGLCyborg  provides a quick interface to animated actors based on morph
    or skeleton frames, it is capable of performing frame interpolation and
    thinking blending (via TGLThinkingController components). *)
  TGLCyborg = class(TGLActor)
  private
    FBirthTime, FDeathTime: TDateTime;
    FSmartReference: TGLCyborgSmartReference;
    FThinkingMode: TGLCyborgThinkingMode;
    FControlers: TList;
    FInterval: Integer;
    FOptions: TGLCyborgOptions;
    FThinkings: TGLCyborgThinks;
    FReference: TGLCyborgSmartReference;
  protected
    procedure SetReference(val: TGLCyborgSmartReference);
    procedure SetThinking(const val: TGLCyborgThinkingMode);
    function StoreThinking: Boolean;
    procedure SetOptions(const val: TGLCyborgOptions);
    procedure DoThink; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    procedure LoadFromStream(const Filename: string; aStream: TStream); override;
    procedure SwitchToThinking(anThinking: TGLCyborgThinks; smooth: Boolean = False);
    function CurrentThinking: string;
    // Indicates whether the cyborg is currently thinking
    function IsThinking: Boolean;
  published
    // See TGLCyborgThinkingMode.
    property ThinkingMode: TGLCyborgThinkingMode read FThinkingMode
	  write FThinkingMode default ctmSelf;
    // Reference Frame Animation mode. Allows specifying if the model is primarily morph or skeleton based
    property SmartReference: TGLCyborgSmartReference read FReference
      write FSmartReference default csrNone;

    // Interval between thinking frames, in milliseconds.
    property Interval: Integer read FInterval write FInterval;
    // Cyborg and thinking miscellanious options.
    property Options: TGLCyborgOptions read FOptions write SetOptions default cDefaultCyborgOptions;
    // Collection of thinking sequences.
    ///property Thinkings: TGLCyborgThinks read FThinkings write SetThinking stored StoreThinking;
  end;


    (* Synchronize self thinking with an other thinkers.
      Copies Start/Current/End Frame values, CurrentFrameDelta,
      ThinkingMode and FrameInterpolation.
    procedure Synchronize(ReferenceCyborg: TGLCyborg);
     *)


var
  vGLSmartObjectsAllocate: Boolean = True;
  vGLSmartObjectsEnableByDefault: Boolean = True;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vCyborgsFileFormat: TGLCyborgThinksList;
  vNextRenderGroupID: Integer = 1;


// ------------------------------------------------------------------
{ TGLCyborg }

procedure TGLCyborg.Assign(Source: TPersistent);
begin
  inherited;
//
end;

procedure TGLCyborg.BuildList(var rci: TGLRenderContextInfo);
begin
  inherited;
//
end;

constructor TGLCyborg.Create(aOwner: TComponent);
begin
  inherited;
//
end;

function TGLCyborg.CurrentThinking: string;
begin
//
end;

destructor TGLCyborg.Destroy;
begin
//
  inherited;
end;

procedure TGLCyborg.DoProgress(const progressTime: TGLProgressTimes);
begin
  inherited;
//
end;

procedure TGLCyborg.DoThink;
begin
//
end;

function TGLCyborg.IsThinking: Boolean;
begin
//
end;

procedure TGLCyborg.LoadFromStream(const Filename: string; aStream: TStream);
begin
  inherited;
//
end;

procedure TGLCyborg.SetOptions(const val: TGLCyborgOptions);
begin
//
end;

procedure TGLCyborg.SetReference(val: TGLCyborgSmartReference);
begin
//
end;

procedure TGLCyborg.SetThinking(const val: TGLCyborgThinkingMode);
begin
//
end;


function TGLCyborg.StoreThinking: Boolean;
begin
//
end;

procedure TGLCyborg.SwitchToThinking(anThinking: TGLCyborgThinks; smooth: Boolean);
begin
//
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TGLCyborg{, TGLGerm, TGLCyborgNet, TGLSmartSwarm}]);

finalization

FreeAndNil(vCyborgsFileFormat);

end.

