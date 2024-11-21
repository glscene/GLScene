//
// The graphics engine GXScene
//
unit GXS.SmartObjects;

(*
  The objects that have built-in properties and methods to support sound, vision,
  physics, and finding shortest paths through mezza obstacles, hightfields or terrains. 
  They should have AI to conduct dialogues and make independent decisions. 
  The smart spatial objects are used to interact with other smart objects and cyborgs.

  The registered classes:
   [TGLSmartGerm, TGLSmartCells, TGLSmartSwarm, TGLSmartNet, TgxCyborg, TgxCyborgs]
*)

interface

{$I Stage.Defines.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,

  Stage.OpenGLTokens,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  Stage.VectorTypesExt,
  Stage.PipelineTransform,
  Stage.Strings,

  GXS.BaseClasses,
  GXS.PersistentClasses,
  GXS.VectorLists,
  GXS.Coordinates,
  GXS.GeometryBB,
  GXS.Color,

  GXS.Scene,
  GXS.Silhouette,
  GXS.Texture,
  GXS.Material,
  GXS.Mesh,
  GXS.Octree,
  GXS.Objects,
  GXS.GeomObjects,
  GXS.ApplicationFileIO,
  GXS.Context,
  GXS.Selection,
  GXS.RenderContextInfo,
  GXS.VectorFileObjects,
  GXS.SoundManager,
  GXS.SoundFileObjects;


type

  TgxSmartSwarmMode = (isNone, isRandom, isTetra, isGrid);

  TgxCyborgReference = (crNone, crWeak, crStrong);
  TgxCyborgThinkMode = (ctmSelf, ctmSleep, ctmOutside, ctmZombie, ctmDeath);

  TgxCyborgOption = (coCollide, coContact, coJoin);
  TgxCyborgSenceOrgans = (csoVision, csoHearing, csoSmell, csoTouch, taste);

  TgxCyborgOptions = set of TgxCyborgOption;
  TgxCyborgThinks = class(TCollection);

  // A list of thinking periods for TgxCyborgThinkMode
  TgxCyborgThinksList = class(TgxPersistentObjectList);

const
  cDefaultCyborgOptions = [coCollide];

type
  (* The Cyborg class specialized as a smart actor with AI.
    The TgxCyborg  provides a quick interface to animated actors based on morph
    or skeleton frames, it is capable of performing frame interpolation and
    thinking blending (via TGLThinkingController components). *)
  TgxCyborg = class(TgxActor)
  private
    FBirthTime, FDeathTime: TDateTime;
    FReference: TgxCyborgReference;
    FThinkMode: TgxCyborgThinkMode;
    FControlers: TList;
    FInterval: Integer;
    FOptions: TgxCyborgOptions;
    FThinkings: TgxCyborgThinks;
  protected
    procedure SetReference(val: TgxCyborgReference);
    procedure SetThinking(const val: TgxCyborgThinkMode);
    function StoreThinking: Boolean;
    procedure SetOptions(const val: TgxCyborgOptions);
    procedure DoThink; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    procedure LoadFromStream(const Filename: string; aStream: TStream); override;
    procedure SwitchToThinking(anThinking: TgxCyborgThinks; smooth: Boolean = False);
    function CurrentThinking: string;
    // Indicates whether the cyborg is currently thinking
    function IsThinking: Boolean;
  published
    // See TgxCyborgThinkMode.
    property ThinkingMode: TgxCyborgThinkMode read FThinkMode
	  write FThinkMode default ctmSelf;
    // Reference Frame Animation mode. Allows specifying if the model is primarily morph or skeleton based
    property SmartReference: TgxCyborgReference read FReference
      write FReference default crNone;

    // Interval between thinking frames, in milliseconds.
    property Interval: Integer read FInterval write FInterval;
    // Cyborg and thinking miscellanious options.
    property Options: TgxCyborgOptions read FOptions write SetOptions default cDefaultCyborgOptions;
    // Collection of thinking sequences.
    ///property Thinkings: TgxCyborgThinks read FThinkings write SetThinking stored StoreThinking;
  end;

  (*
     Synchronize self thinking with an other thinkers in the swarm
     Copies Ai/Current/End values,ThinkingMode and GridInterpolation.
     procedure Synchronize(IntelSwarm: TGLIntelSwarm);
   *)
  TgxSwartSwarm = class(TgxPoints)
  private
    FBirthTime, FDeathTime: TDateTime;
    FReference: TgxCyborgReference;
    FThinkMode: TgxSmartSwarmMode;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ThinkingMode: TgxSmartSwarmMode read FThinkMode;
  end;

var
  vGLSmartObjectsAllocate: Boolean = True;
  vGLSmartObjectsEnableByDefault: Boolean = True;

implementation // ----------------------------------------------------------

var
  vCyborgsFileFormat: TgxCyborgThinksList;
  vNextRenderGroupID: Integer = 1;


// ------------------------------------------------------------------
{ TgxCyborg }

procedure TgxCyborg.Assign(Source: TPersistent);
begin
  inherited;
//
end;

procedure TgxCyborg.BuildList(var rci: TgxRenderContextInfo);
begin
  inherited;
//
end;

constructor TgxCyborg.Create(aOwner: TComponent);
begin
  inherited;
//
end;

function TgxCyborg.CurrentThinking: string;
begin
//
end;

destructor TgxCyborg.Destroy;
begin
//
  inherited;
end;

procedure TgxCyborg.DoProgress(const progressTime: TgxProgressTimes);
begin
  inherited;
//
end;

procedure TgxCyborg.DoThink;
begin
//
end;

function TgxCyborg.IsThinking: Boolean;
begin
//
end;

procedure TgxCyborg.LoadFromStream(const Filename: string; aStream: TStream);
begin
  inherited;
//
end;

procedure TgxCyborg.SetOptions(const val: TgxCyborgOptions);
begin
//
end;

procedure TgxCyborg.SetReference(val: TgxCyborgReference);
begin
//
end;

procedure TgxCyborg.SetThinking(const val: TgxCyborgThinkMode);
begin
//
end;


function TgxCyborg.StoreThinking: Boolean;
begin
//
end;

procedure TgxCyborg.SwitchToThinking(anThinking: TgxCyborgThinks; smooth: Boolean);
begin
//
end;


// ------------------------------------------------------------------
{ TgxSwartSwarm }

procedure TgxSwartSwarm.Assign(Source: TPersistent);
begin
  inherited;
  //
end;

constructor TgxSwartSwarm.Create(aOwner: TComponent);
begin
  inherited;
  //
end;

destructor TgxSwartSwarm.Destroy;
begin
  //
  inherited;
end;

initialization
// ------------------------------------------------------------------

  RegisterClasses([TgxCyborg (*, TGLSmartSwarm*)]);

finalization

FreeAndNil(vCyborgsFileFormat);

end.

