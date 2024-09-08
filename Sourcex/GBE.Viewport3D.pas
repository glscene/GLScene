unit GBE.Viewport3D;
(*
  The TGBEViewport3D inherits from TViewport3D. It adds the ability to retrieve in TBitmap form the images
  from each of the cameras placed in the 3D scene. This then allows to display these images in other
  areas of the interface without having to duplicate the TViewport3D or the 3D scenes to calculate.
  Based on code by Gregory Bersegeay
*)
interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Types,
  System.Math.Vectors,
  System.DateUtils,
  System.UITypes,

  FMX.Types,
  FMX.Controls,
  FMX.Viewport3D,
  FMX.Graphics,
  FMX.Types3D,
  FMX.Controls3D,
  FMX.Layouts;

type
  THelpOpenControl3D = class(TControl3D);

  TGBEViewport3D = class(TViewport3D)
  private
    FDrawing, fActiveFPS: boolean;
    fFPS, fComputeFPS: integer;
    FMyBitmap: TBitmap;
    FMyTexture: TTexture;
    fMyContext: TContext3D;
    fBackgroundColor: cardinal;
  protected
    FMyRenderingList: TList<TControl3D>; // List of 3D objects to display
    FMyViewList: TDictionary<TCamera, TBitmap>;
    // List of views (one view per camera)
    fheureDebut: TTime;
    procedure RebuildRenderingList;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getBitmapFromView(camera: TCamera): TBitmap;
    procedure DoAddView(camera: TCamera);
    procedure DoRemoveView(camera: TCamera);
    procedure DoClearListView;
    property MyContext: TContext3D read fMyContext write fMyContext;
    property BackgroundColor: cardinal read fBackgroundColor
      write fBackgroundColor;
  published
    property ActiveFPS: boolean read fActiveFPS write fActiveFPS;
    property FPS: integer read fFPS;
  end;

procedure Register;

implementation // --------------------------------------------------------------

// TGBEViewport3D1

constructor TGBEViewport3D.Create(AOwner: TComponent);
begin
  inherited;
  FMyViewList := TDictionary<TCamera, TBitmap>.Create;
  BackgroundColor := TAlphaColorRec.Null;
  fFPS := 0;
  fComputeFPS := 0;
  fActiveFPS := false;
  fheureDebut := now;
end;

destructor TGBEViewport3D.Destroy;
begin
  FreeAndNil(fMyContext);
  FreeAndNil(FMyBitmap);
  FreeAndNil(FMyTexture);
  FreeAndNil(FMyRenderingList);
  FreeAndNil(FMyViewList);
  inherited;
end;

procedure TGBEViewport3D.DoAddView(camera: TCamera);
begin
  FMyViewList.Add(camera, TBitmap.Create);
end;

procedure TGBEViewport3D.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then
    RebuildRenderingList;
end;

procedure TGBEViewport3D.DoRemoveView(camera: TCamera);
begin
  FMyViewList.Remove(camera);
end;

procedure TGBEViewport3D.DoClearListView;
begin
  FMyViewList.Clear;
end;

function TGBEViewport3D.getBitmapFromView(camera: TCamera): TBitmap;
begin
  result := TBitmap.Create;
  if not(FDrawing) then
    FMyViewList.TryGetValue(camera, result);
end;

procedure TGBEViewport3D.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then
  begin
    RebuildRenderingList;
    Repaint;
  end;
end;

procedure TGBEViewport3D.Paint;
var
  i: integer;
  Control: TControl3D;
  New: TMatrix3D;
  theCamera: TCamera;
  duree: int64;
begin
  inherited;
  if FDrawing then
    exit;

  FDrawing := true;

  try
    if fActiveFPS then
    begin
      inc(fComputeFPS);
      duree := SecondsBetween(now, fheureDebut);
      if duree > 0 then
      begin
        fFPS := fComputeFPS div duree;
        fComputeFPS := 0;
        fheureDebut := now;
      end;
    end;

    for theCamera in FMyViewList.Keys do
    begin

      if Assigned(fMyContext) then
      begin
        if fMyContext.BeginScene then
        begin
          try
            New := theCamera.CameraMatrix;

            fMyContext.Clear([TClearTarget.Color, TClearTarget.Depth],
              BackgroundColor, 1.0, 0);
            fMyContext.SetCameraMatrix(theCamera.CameraMatrix);

            if Assigned(FMyRenderingList) and (FMyRenderingList.Count > 0) then
            begin
              for i := 0 to FMyRenderingList.Count - 1 do
              begin
                if FMyRenderingList[i].Visible or (FMyRenderingList[i].Tag <> 2)
                  or (not FMyRenderingList[i].Visible and
                  (csDesigning in ComponentState) and not FMyRenderingList[i]
                  .Locked) then
                begin
                  Control := TControl3D(FMyRenderingList[i]);
                  Control.Context.SetCameraMatrix(New);
                  if (csDesigning in ComponentState) and (not Control.Visible)
                  then
                    continue;
                  THelpOpenControl3D(Control).RenderInternal;
                end;
              end;
            end;

          finally
            fMyContext.EndScene;
          end;
        end;
      end;

      fMyContext.CopyToBitmap(FMyBitmap, Rect(0, 0, FMyBitmap.Width,
        FMyBitmap.Height));
      FMyViewList.Items[theCamera].Width := FMyBitmap.Width;
      FMyViewList.Items[theCamera].Height := FMyBitmap.Height;
      FMyViewList.Items[theCamera].CopyFromBitmap(FMyBitmap);
    end;

  finally
    FDrawing := false;
  end;
end;

procedure TGBEViewport3D.RebuildRenderingList;
var
  i: integer;
begin
  if Assigned(children) and (FUpdating = 0) then
  begin
    if not Assigned(FMyRenderingList) then
      FMyRenderingList := TList<TControl3D>.Create;
    FMyRenderingList.Clear;
    for i := 0 to children.Count - 1 do
    begin
      if children[i] is TControl3D then
      begin
        FMyRenderingList.Add((children[i] as TControl3D));
      end;
    end;
  end;
end;

procedure TGBEViewport3D.Resize;
begin
  inherited;
  FreeAndNil(FMyBitmap);
  FreeAndNil(FMyTexture);
  FreeAndNil(fMyContext);

  FMyTexture := TTexture.Create;
  FMyTexture.Style := [TTextureStyle.RenderTarget];
  FMyTexture.SetSize(Round(Width), Round(Height));
  fMyContext := TContextManager.CreateFromTexture(FMyTexture,
    TMultisample.FourSamples, true);
  FMyBitmap := TBitmap.Create(fMyContext.Width, fMyContext.Height);
end;

// ----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEViewport3D]);
end;

end.
