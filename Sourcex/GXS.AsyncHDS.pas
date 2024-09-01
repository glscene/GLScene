//       /
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.AsyncHDS;

(*
  Implements a HDS Filter that generates HeightData tiles in a seperate thread.

  This component is a TgxHeightDataSourceFilter, which uses a TgxHeightDataSourceThread,
  to asyncronously search the HeightData cache for any queued tiles.
  When found, it then prepares the queued tile in its own TgxHeightDataThread.

  This allows the GUI to remain responsive, and prevents freezes when new tiles are
  being prepared.  Although this keeps the framerate up, it may cause holes in the
  terrain to show, if the HeightDataThreads cant keep up with the TerrainRenderer's
  requests for new tiles.
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,
  GXS.HeightData;

type
  TgxAsyncHDS = class;
  TIdleEvent = procedure(Sender: TgxAsyncHDS; TilesUpdated: boolean) of object;
  TNewTilePreparedEvent = procedure(Sender: TgxAsyncHDS;
     HeightData: TgxHeightData) of object;  // a tile was updated (called INSIDE the sub-thread?)

  (* Determines if/how dirty tiles are displayed and when they are released.
     When a tile is maked as dirty, a replacement is queued immediately.
     However, the replacement cant be used until the HDThread has finished preparing it.
     Dirty tiles can be deleted as soon as they are no longer used/displayed.
     Possible states for a TUseDirtyTiles.
       hdsNever :            Dirty tiles get released immediately, leaving a hole in the terrain, until the replacement is hdsReady.
       hdsUntilReplaced :    Dirty tiles are used, until the HDThread has finished preparing the queued replacement.
       hdsUntilAllReplaced : Waits until the HDSThread has finished preparing ALL queued tiles,
                             before allowing the renderer to switch over to the new set of tiles.
                             (This prevents a fading checkerbox effect.) *)
  TUseDirtyTiles=(dtNever,dtUntilReplaced,dtUntilAllReplaced);

  TgxAsyncHDS = class(TgxHeightDataSourceFilter)
  private
    FOnIdleEvent: TIdleEvent;
    FOnNewTilePrepared: TNewTilePreparedEvent;
    FUseDirtyTiles: TUseDirtyTiles;
    FTilesUpdated: boolean;
  public
    // TilesUpdated:boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforePreparingData(HeightData: TgxHeightData); override;
    procedure StartPreparingData(HeightData: TgxHeightData); override;
    procedure ThreadIsIdle; override;
    procedure NewTilePrepared(HeightData: TgxHeightData);
    function ThreadCount: integer;
	  (*  Wait for all running threads to finish.
          Should only be called after setting Active to false,
          to prevent new threads from starting. *)
    procedure WaitFor(TimeOut: integer = 2000);
    // procedure NotifyChange(Sender : TObject); override;
      (* This function prevents the user from trying to write directly to this variable.
        FTilesUpdated if NOT threadsafe and should only be reset with TilesUpdatedFlagReset. *)
    function TilesUpdated: boolean;  // Returns true if tiles have been updated since the flag was last reset
    procedure TilesUpdatedFlagReset; // sets the TilesUpdatedFlag to false; (is ThreadSafe)
  published
    property OnIdle: TIdleEvent read FOnIdleEvent write FOnIdleEvent;
    property OnNewTilePrepared: TNewTilePreparedEvent read FOnNewTilePrepared  write FOnNewTilePrepared;
    property UseDirtyTiles: TUseDirtyTiles read FUseDirtyTiles write FUseDirtyTiles;
    property MaxThreads; // sets the maximum number of simultaineous threads that will prepare tiles.(>1 is rarely needed)
    property Active;  // set to false, to ignore new queued tiles.(Partially processed tiles will still be completed)
  end;

  TgxAsyncHDThread = class(TgxHeightDataThread)
  public
    Owner: TgxAsyncHDS;
    HDS: TgxHeightDataSource;
    Procedure Execute; override;
    Procedure Sync;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxAsyncHDS ------------------
// ------------------

constructor TgxAsyncHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxThreads := 1;
  FUseDirtyTiles := dtNever;
  FTilesUpdated := true;
end;

destructor TgxAsyncHDS.Destroy;
begin
  inherited Destroy;
end;

procedure TgxAsyncHDS.BeforePreparingData(HeightData: TgxHeightData);
begin
  if FUseDirtyTiles = dtNever then
  begin
    if HeightData.OldVersion <> nil then
    begin
      HeightData.OldVersion.DontUse := true;
      HeightData.DontUse := false;
    end;
  end;
  if assigned(HeightDataSource) then
    HeightDataSource.BeforePreparingData(HeightData);
end;

procedure TgxAsyncHDS.StartPreparingData(HeightData: TgxHeightData);
var
  HDThread: TgxAsyncHDThread;
  HDS: TgxHeightDataSource;
begin
  HDS := HeightDataSource;
  // ---if there is no linked HDS then return an empty tile--
  if not assigned(HDS) then
  begin
    HeightData.DataState := hdsNone;
    exit;
  end;
  if (Active = false) then
    exit;

  // ---If not using threads then prepare the HD tile directly---  (everything else freezes until done)
  if MaxThreads = 0 then
  begin
    HDS.StartPreparingData(HeightData);
    if HeightData.DataState = hdsPreparing then
      HeightData.DataState := hdsReady
    else
      HeightData.DataState := hdsNone;
  end
  else
  begin // --MaxThreads>0 : start the thread and go back to start the next one--
    HeightData.DataState := hdsPreparing; // prevent other threads from preparing this HD.
    HDThread := TgxAsyncHDThread.Create(true);
    HDThread.Owner := self;
    HDThread.HDS := self.HeightDataSource;
    HDThread.HeightData := HeightData;
    HeightData.Thread := HDThread;
    HDThread.FreeOnTerminate := false;
    HDThread.Start;
  end;
end;

procedure TgxAsyncHDS.ThreadIsIdle;
var
  i: integer;
  lst: TList;
  HD: TgxHeightData;
begin
  // ----------- dtUntilAllReplaced -------------
  // Switch to the new version of ALL dirty tiles
  lst := self.Data.LockList;
  try
    if FUseDirtyTiles = dtUntilAllReplaced then
    begin
      i := lst.Count;
      while (i > 0) do
      begin
        dec(i);
        HD := TgxHeightData(lst.Items[i]);
        if (HD.DataState in [hdsReady, hdsNone]) and (HD.DontUse) and (HD.OldVersion <> nil) then
        begin
          HD.DontUse := false;
          HD.OldVersion.DontUse := true;
          FTilesUpdated := true;
        end;
      end;
    end; // Until All Replaced
    if assigned(FOnIdleEvent) then
      FOnIdleEvent(self, FTilesUpdated);
  finally
    self.Data.UnlockList;
  end;
  // --------------------------------------------
end;

procedure TgxAsyncHDS.NewTilePrepared(HeightData: TgxHeightData);
var
  HD: TgxHeightData;
begin
  if assigned(HeightDataSource) then
    HeightDataSource.AfterPreparingData(HeightData);
  with self.Data.LockList do
  begin
    try
      HD := HeightData;
      // --------------- dtUntilReplaced -------------
      // Tell terrain renderer to display the new tile
      if (FUseDirtyTiles = dtUntilReplaced) and (HD.DontUse) and (HD.OldVersion <> nil) then
      begin
        HD.DontUse := false; // No longer ignore the new tile
        HD.OldVersion.DontUse := true; // Start ignoring the old tile
      end;
      // ---------------------------------------------
      if HD.DontUse = false then
        FTilesUpdated := true;
      if assigned(FOnNewTilePrepared) then
        FOnNewTilePrepared(self, HeightData); // OnNewTilePrepared Event
    finally
      self.Data.UnlockList;
    end;
  end;
end;

function TgxAsyncHDS.ThreadCount: integer;
var
  lst: TList;
  i, TdCtr: integer;
  HD: TgxHeightData;
begin
  lst := self.Data.LockList;
  i := 0;
  TdCtr := 0;
  while (i < lst.Count) and (TdCtr < self.MaxThreads) do
  begin
    HD := TgxHeightData(lst.Items[i]);
    if HD.Thread <> nil then
      Inc(TdCtr);
    Inc(i);
  end;
  self.Data.UnlockList;
  result := TdCtr;
end;

procedure TgxAsyncHDS.WaitFor(TimeOut: integer = 2000);
var
  OutTime: TDateTime;
begin
  Assert(self.Active = false);
  OutTime := now + TimeOut;
  While ((now < OutTime) and (ThreadCount > 0)) do
  begin
    sleep(0);
  end;
  Assert(ThreadCount = 0);
end;

{
  procedure TgxAsyncHDS.NotifyChange(Sender : TObject);
  begin
  TilesChanged:=true;
  end;
}

function TgxAsyncHDS.TilesUpdated: boolean;
begin
  result := FTilesUpdated;
end;

// Set the TilesUpdatedFlag to false. (is Threadsafe)
procedure TgxAsyncHDS.TilesUpdatedFlagReset;
begin
  if not assigned(self) then
    exit; // prevents AV on Application termination.
  with Data.LockList do
    try
      FTilesUpdated := false;
    finally
      Data.UnlockList;
    end;
end;

// -------------------HD Thread----------------
Procedure TgxAsyncHDThread.Execute;
Begin
  HDS.StartPreparingData(HeightData);
  HeightData.Thread := nil;
  Synchronize(Sync);
end;

Procedure TgxAsyncHDThread.Sync;
begin
  Owner.NewTilePrepared(HeightData);
  if HeightData.DataState = hdsPreparing then
    HeightData.DataState := hdsReady;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClass(TgxAsyncHDS);

end.
