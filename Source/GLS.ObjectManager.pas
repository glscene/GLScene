//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.ObjectManager;

(*
  The object manager is used for registering classes together with a category,
  description + icon, so that they can be displayed visually.  This can then
  be used by run-time or design-time scene editors for choosing which
  scene objects to place into a scene.

  TODO: add some notification code, so that when a scene object is registered/
  unregistered, any editor that is using the object manager can be notified.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  VCL.Graphics,
  VCL.Controls,
  VCL.Menus,

  GLS.Scene,
  GLS.Utils;

type
  PSceneObjectEntry = ^TGLSceneObjectEntry;

  // holds a relation between an scene object class, its global identification,
  // its location in the object stock and its icon reference
  TGLSceneObjectEntry = record
    ObjectClass: TGLSceneObjectClass;
    Name: string; // type name of the object
    Category: string; // category of object
    Index, // index into "FSceneObjectList"
    ImageIndex: Integer; // index into "FObjectIcons"
  end;

  TGLObjectManager = class(TComponent)
  private
    FSceneObjectList: TList;
    FObjectIcons: TImageList; // a list of icons for scene objects
    FOverlayIndex, // indices into the object icon list
    FSceneRootIndex, FCameraRootIndex, FLightsourceRootIndex,
      FObjectRootIndex: Integer;
  protected
    procedure DestroySceneObjectList;
    function FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
      const ASceneObject: string = ''): PSceneObjectEntry;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDefaultObjectIcons(ResourceModule: Cardinal);
    function GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
    function GetImageIndex(ASceneObject: TGLSceneObjectClass): Integer;
    function GetCategory(ASceneObject: TGLSceneObjectClass): string;
    procedure GetRegisteredSceneObjects(ObjectList: TStringList);
    procedure PopulateMenuWithRegisteredSceneObjects(AMenuItem: TMenuItem;
      aClickEvent: TNotifyEvent);
    // Registers a stock object and adds it to the stock object list
    procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
      const aName, aCategory: string); overload;
    procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
      const aName, aCategory: string; aBitmap: TBitmap); overload;
    procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
      const aName, aCategory: string; ResourceModule: Cardinal;
      ResourceName: string = ''); overload;
    // Unregisters a stock object and removes it from the stock object list
    procedure UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
    property ObjectIcons: TImageList read FObjectIcons;
    property SceneRootIndex: Integer read FSceneRootIndex;
    property LightsourceRootIndex: Integer read FLightsourceRootIndex;
    property CameraRootIndex: Integer read FCameraRootIndex;
    property ObjectRootIndex: Integer read FObjectRootIndex;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

constructor TGLObjectManager.Create(AOwner: TComponent);
begin
  inherited;
  FSceneObjectList := TList.Create;
  // FObjectIcons Width + Height are set when you add the first bitmap
  FObjectIcons := TImageList.CreateSize(16, 16);
end;

destructor TGLObjectManager.Destroy;
begin
  DestroySceneObjectList;
  FObjectIcons.Free;
  inherited Destroy;
end;

function TGLObjectManager.FindSceneObjectClass(AObjectClass
  : TGLSceneObjectClass; const ASceneObject: string = ''): PSceneObjectEntry;
var
  I: Integer;
  Found: Boolean;
begin
  Result := nil;
  Found := False;
  with FSceneObjectList do
  begin
    for I := 0 to Count - 1 do
      with TGLSceneObjectEntry(Items[I]^) do
        if (ObjectClass = AObjectClass) and (Length(ASceneObject) = 0) or
          (CompareText(Name, ASceneObject) = 0) then
        begin
          Found := True;
          Break;
        end;
    if Found then
      Result := Items[I];
  end;
end;

function TGLObjectManager.GetClassFromIndex(Index: Integer)
  : TGLSceneObjectClass;
begin
  if Index < 0 then
    Index := 0;
  if Index > FSceneObjectList.Count - 1 then
    Index := FSceneObjectList.Count - 1;
  Result := TGLSceneObjectEntry(FSceneObjectList.Items[Index + 1]^).ObjectClass;
end;

function TGLObjectManager.GetImageIndex(ASceneObject
  : TGLSceneObjectClass): Integer;
var
  classEntry: PSceneObjectEntry;
begin
  classEntry := FindSceneObjectClass(ASceneObject);
  if Assigned(classEntry) then
    Result := classEntry^.ImageIndex
  else
    Result := 0;
end;

function TGLObjectManager.GetCategory(ASceneObject
  : TGLSceneObjectClass): string;
var
  classEntry: PSceneObjectEntry;
begin
  classEntry := FindSceneObjectClass(ASceneObject);
  if Assigned(classEntry) then
    Result := classEntry^.Category
  else
    Result := '';
end;

procedure TGLObjectManager.GetRegisteredSceneObjects(ObjectList: TStringList);
var
  I: Integer;
begin
  if Assigned(ObjectList) then
    with ObjectList do
    begin
      Clear;
      for I := 0 to FSceneObjectList.Count - 1 do
        with TGLSceneObjectEntry(FSceneObjectList.Items[I]^) do
          AddObject(Name, Pointer(ObjectClass));
    end;
end;

procedure TGLObjectManager.PopulateMenuWithRegisteredSceneObjects
  (AMenuItem: TMenuItem; aClickEvent: TNotifyEvent);
var
  ObjectList: TStringList;
  I, j: Integer;
  Item, CurrentParent: TMenuItem;
  CurrentCategory: string;
  Soc: TGLSceneObjectClass;
begin
  ObjectList := TStringList.Create;
  try
    GetRegisteredSceneObjects(ObjectList);
    for I := 0 to ObjectList.Count - 1 do
      if ObjectList[I] <> '' then
      begin
        CurrentCategory :=
          GetCategory(TGLSceneObjectClass(ObjectList.Objects[I]));
        if CurrentCategory = '' then
          CurrentParent := AMenuItem
        else
        begin
          CurrentParent := NewItem(CurrentCategory, 0, False, True, nil, 0, '');
          AMenuItem.Add(CurrentParent);
        end;
        for j := I to ObjectList.Count - 1 do
          if ObjectList[j] <> '' then
          begin
            Soc := TGLSceneObjectClass(ObjectList.Objects[j]);
            if CurrentCategory = GetCategory(Soc) then
            begin
              Item := NewItem(ObjectList[j], 0, False, True,
                aClickEvent, 0, '');
              Item.ImageIndex := GetImageIndex(Soc);
              CurrentParent.Add(Item);
              ObjectList[j] := '';
              if CurrentCategory = '' then
                Break;
            end;
          end;
      end;
  finally
    ObjectList.Free;
  end;
end;

procedure TGLObjectManager.RegisterSceneObject(ASceneObject
  : TGLSceneObjectClass; const aName, aCategory: string);
var
  resBitmapName: string;
  bmp: TBitmap;
begin
  // Since no resource name was provided, assume it's the same as class name
  resBitmapName := ASceneObject.ClassName;
  bmp := TBitmap.Create;
  try
    // Try loading bitmap from module that class is in
    GLLoadBitmapFromInstance(FindClassHInstance(ASceneObject), bmp,
      resBitmapName);
    if bmp.Width = 0 then
      GLLoadBitmapFromInstance(HInstance, bmp, resBitmapName);
    // If resource was found, register scene object with bitmap
    if bmp.Width <> 0 then
    begin
      RegisterSceneObject(ASceneObject, aName, aCategory, bmp);
    end
    else
      // Resource not found, so register without bitmap
      RegisterSceneObject(ASceneObject, aName, aCategory, nil);
  finally
    bmp.Free;
  end;
end;

procedure TGLObjectManager.RegisterSceneObject(ASceneObject
  : TGLSceneObjectClass; const aName, aCategory: string; aBitmap: TBitmap);
var
  newEntry: PSceneObjectEntry;
  bmp: TBitmap;
begin
  if Assigned(RegisterNoIconProc) then
    RegisterNoIcon([ASceneObject]);
  with FSceneObjectList do
  begin
    // make sure no class is registered twice
    if Assigned(FindSceneObjectClass(ASceneObject, aName)) then
      Exit;
    New(newEntry);
    try
      with newEntry^ do
      begin
        // object stock stuff
        // registered objects list stuff
        ObjectClass := ASceneObject;
        newEntry^.Name := aName;
        newEntry^.Category := aCategory;
        Index := FSceneObjectList.Count;
        if Assigned(aBitmap) then
        begin
          bmp := TBitmap.Create;
          try
            // If we just add the bitmap, and it has different dimensions, then
            // all icons will be cleared, so ensure this doesn't happen
            bmp.PixelFormat := pf24bit;
            bmp.Width := FObjectIcons.Width;
            bmp.Height := FObjectIcons.Height;
            bmp.Canvas.Draw(0, 0, aBitmap);
            FObjectIcons.AddMasked(bmp, bmp.Canvas.Pixels[0, 0]);
            ImageIndex := FObjectIcons.Count - 1;
          finally
            bmp.Free;
          end;
        end
        else
          ImageIndex := 0;
      end;
      Add(newEntry);
    finally
      //
    end;
  end;
end;

procedure TGLObjectManager.RegisterSceneObject(ASceneObject
  : TGLSceneObjectClass; const aName, aCategory: string;
  ResourceModule: Cardinal; ResourceName: string = '');
var
  bmp: TBitmap;
  resBitmapName: string;
begin
  if ResourceName = '' then
    resBitmapName := ASceneObject.ClassName
  else
    resBitmapName := ResourceName;
  bmp := TBitmap.Create;
  try
    // Load resource
    if (ResourceModule <> 0) then
      GLLoadBitmapFromInstance(ResourceModule, bmp, resBitmapName);
    // If the resource was found, then register scene object using the bitmap
    if bmp.Width > 0 then
      RegisterSceneObject(ASceneObject, aName, aCategory, bmp)
    else
      // Register the scene object with no icon
      RegisterSceneObject(ASceneObject, aName, aCategory, nil);
  finally
    bmp.Free;
  end;
end;

procedure TGLObjectManager.UnRegisterSceneObject(ASceneObject
  : TGLSceneObjectClass);
var
  oldEntry: PSceneObjectEntry;
begin
  // find the class in the scene object list
  oldEntry := FindSceneObjectClass(ASceneObject);
  // found?
  if Assigned(oldEntry) then
  begin
    // remove its entry from the list of registered objects
    FSceneObjectList.Remove(oldEntry);
    // finally free the memory for the entry
    Dispose(oldEntry);
  end;
end;

procedure TGLObjectManager.CreateDefaultObjectIcons(ResourceModule: Cardinal);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  with FObjectIcons, bmp.Canvas do
  begin
    try
      // There's a more direct way for loading images into the image list, but
      // the image quality suffers too much
{$IFDEF WIN32}
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_cross');
      FOverlayIndex := AddMasked(bmp, Pixels[0, 0]);
      Overlay(FOverlayIndex, 0); // used as indicator for disabled objects
{$ENDIF}
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_root');
      FSceneRootIndex := AddMasked(bmp, Pixels[0, 0]);
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_camera');
      FCameraRootIndex := AddMasked(bmp, Pixels[0, 0]);
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_lights');
      FLightsourceRootIndex := AddMasked(bmp, Pixels[0, 0]);
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_objects');
      FObjectRootIndex := AddMasked(bmp, Pixels[0, 0]);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TGLObjectManager.DestroySceneObjectList;
var
  I: Integer;
begin
  with FSceneObjectList do
  begin
    for I := 0 to Count - 1 do
      Dispose(PSceneObjectEntry(Items[I]));
    Free;
  end;
end;

// -------------------------------------------
initialization

// -------------------------------------------

end.
