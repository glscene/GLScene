//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ObjectManager;

(*
   The object manager is used for registering classes together with a category,
   description + icon, so that they can be displayed visually.  This can then
   be used by run-time or design-time scene editors for choosing which
   scene objects to place into a scene.

   TODO: add some notification code, so that when a scene object is registered/
   unregistered, any editor that is using the object manager can be notified.
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  FMX.Graphics,
  FMX.Controls,
  FMX.Menus,
  GXS.Utils,
  GXS.Scene;

type

  PSceneObjectEntry = ^TgxSceneObjectEntry;
  // holds a relation between an scene object class, its global identification,
  // its location in the object stock and its icon reference
  TgxSceneObjectEntry = record
    ObjectClass: TgxSceneObjectClass;
    Name: string; // type name of the object
    Category: string; // category of object
    Index, // index into "FSceneObjectList"
    ImageIndex: Integer; // index into "FObjectIcons"
  end;

  TgxObjectManager = class(TComponent)
  private

    FSceneObjectList: TList;
    FObjectIcons: TStyleBook; // In VCL FObjectIcons: TImageList; <- a list of icons for scene objects
{$IFDEF MSWINDOWS}
    FOverlayIndex, // indices into the object icon list
{$ENDIF}
    FSceneRootIndex,
      FCameraRootIndex,
      FLightsourceRootIndex,
      FObjectRootIndex: Integer;
  protected
    
    procedure DestroySceneObjectList;
    function FindSceneObjectClass(AObjectClass: TgxSceneObjectClass;
      const ASceneObject: string = ''): PSceneObjectEntry;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDefaultObjectIcons(ResourceModule: Cardinal);
    function GetClassFromIndex(Index: Integer): TgxSceneObjectClass;
    function GetImageIndex(ASceneObject: TgxSceneObjectClass): Integer;
    function GetCategory(ASceneObject: TgxSceneObjectClass): string;
    procedure GetRegisteredSceneObjects(ObjectList: TStringList);
    procedure PopulateMenuWithRegisteredSceneObjects(AMenuItem: TMenuItem; AClickEvent: TNotifyEvent);
    // Registers a stock object and adds it to the stock object list
    procedure RegisterSceneObject(ASceneObject: TgxSceneObjectClass; const AName, ACategory: string); overload;
    procedure RegisterSceneObject(ASceneObject: TgxSceneObjectClass; const AName, ACategory: string; ABitmap: TBitmap); overload;
    procedure RegisterSceneObject(ASceneObject: TgxSceneObjectClass; const AName, ACategory: string; ResourceModule: Cardinal; ResourceName: string = ''); overload;
    // Unregisters a stock object and removes it from the stock object list
    procedure UnRegisterSceneObject(ASceneObject: TgxSceneObjectClass);
    property ObjectIcons: TStyleBook read FObjectIcons; //In VCL TImageList
    property SceneRootIndex: Integer read FSceneRootIndex;
    property LightsourceRootIndex: Integer read FLightsourceRootIndex;
    property CameraRootIndex: Integer read FCameraRootIndex;
    property ObjectRootIndex: Integer read FObjectRootIndex;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

//----------------- TgxObjectManager ---------------------------------------------

constructor TgxObjectManager.Create(AOwner: TComponent);
begin
  inherited;
  FSceneObjectList := TList.Create;
  // FObjectIcons Width + Height are set when you add the first bitmap
  FObjectIcons := TStyleBook.Create(AOwner);  //In VCL TImageList.CreateSize(16, 16);
end;

destructor TgxObjectManager.Destroy;
begin
  DestroySceneObjectList;
  FObjectIcons.Free;
  inherited Destroy;
end;

function TgxObjectManager.FindSceneObjectClass(AObjectClass: TgxSceneObjectClass;
  const aSceneObject: string = ''): PSceneObjectEntry;
var
  I: Integer;
  Found: Boolean;
begin
  Result := nil;
  Found := False;
  with FSceneObjectList do
  begin
    for I := 0 to Count - 1 do
      with TgxSceneObjectEntry(Items[I]^) do
        if (ObjectClass = AObjectClass) and (Length(ASceneObject) = 0)
          or (CompareText(Name, ASceneObject) = 0) then
        begin
          Found := True;
          Break;
        end;
    if Found then
      Result := Items[I];
  end;
end;

function TgxObjectManager.GetClassFromIndex(Index: Integer): TgxSceneObjectClass;
begin
  if Index < 0 then
    Index := 0;
  if Index > FSceneObjectList.Count - 1 then
    Index := FSceneObjectList.Count - 1;
  Result := TgxSceneObjectEntry(FSceneObjectList.Items[Index + 1]^).ObjectClass;
end;

function TgxObjectManager.GetImageIndex(ASceneObject: TgxSceneObjectClass): Integer;
var
  classEntry: PSceneObjectEntry;
begin
  classEntry := FindSceneObjectClass(ASceneObject);
  if Assigned(classEntry) then
    Result := classEntry^.ImageIndex
  else
    Result := 0;
end;

function TgxObjectManager.GetCategory(ASceneObject: TgxSceneObjectClass): string;
var
  classEntry: PSceneObjectEntry;
begin
  classEntry := FindSceneObjectClass(ASceneObject);
  if Assigned(classEntry) then
    Result := classEntry^.Category
  else
    Result := '';
end;

procedure TgxObjectManager.GetRegisteredSceneObjects(objectList: TStringList);
var
  i: Integer;
begin
  if Assigned(objectList) then
    with objectList do
    begin
      Clear;
      for i := 0 to FSceneObjectList.Count - 1 do
        with TgxSceneObjectEntry(FSceneObjectList.Items[I]^) do
          AddObject(Name, Pointer(ObjectClass));
    end;
end;

procedure TgxObjectManager.PopulateMenuWithRegisteredSceneObjects(AMenuItem: TMenuItem;
  AClickEvent: TNotifyEvent);
var
  ObjectList: TStringList;
  i, j: Integer;
  Item, CurrentParent: TMenuItem;
  CurrentCategory: string;
  Soc: TgxSceneObjectClass;
begin
  ObjectList := TStringList.Create;
  try
    GetRegisteredSceneObjects(ObjectList);
    for i := 0 to ObjectList.Count - 1 do
      if ObjectList[i] <> '' then
      begin
        CurrentCategory := GetCategory(TgxSceneObjectClass(ObjectList.Objects[i]));
        if CurrentCategory = '' then
          CurrentParent := AMenuItem
        else
        begin
          CurrentParent := TMenuItem.Create(nil);
          CurrentParent.Text := ObjectList[j];
          //in VCL CurrentParent := NewItem(CurrentCategory, 0, False, True, nil, 0, '');
          AMenuItem.AddObject(CurrentParent);
        end;
        for j := i to ObjectList.Count - 1 do
          if ObjectList[j] <> '' then
          begin
            Soc := TgxSceneObjectClass(ObjectList.Objects[j]);
            if CurrentCategory = GetCategory(Soc) then
            begin
              Item := TMenuItem.Create(nil);
              Item.Text := ObjectList[j];
              //in VCL Item := NewItem(ObjectList[j], 0, False, True, AClickEvent, 0, '');
              { TODO : E2003 Undeclared identifier: 'ImageIndex' }
              (*Item.ImageIndex := GetImageIndex(Soc);*)
              CurrentParent.AddObject(Item);
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

procedure TgxObjectManager.RegisterSceneObject(ASceneObject: TgxSceneObjectClass;
  const aName, aCategory: string);
var
  resBitmapName: string;
  bmp: TBitmap;
begin
  // Since no resource name was provided, assume it's the same as class name
  resBitmapName := ASceneObject.ClassName;
  bmp := TBitmap.Create;
  try
    // Try loading bitmap from module that class is in
    GLLoadBitmapFromInstance(FindClassHInstance(ASceneObject), bmp, resBitmapName);
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

procedure TgxObjectManager.RegisterSceneObject(ASceneObject: TgxSceneObjectClass; const AName, ACategory: string; ABitmap: TBitmap);
var
  NewEntry: PSceneObjectEntry;
  bmp: TBitmap;
begin
  if Assigned(RegisterNoIconProc) then
    RegisterNoIcon([aSceneObject]);
  with FSceneObjectList do
  begin
    // make sure no class is registered twice
    if Assigned(FindSceneObjectClass(ASceneObject, AName)) then
      Exit;
    New(NewEntry);
    try
      with NewEntry^ do
      begin
        // object stock stuff
        // registered objects list stuff
        ObjectClass := ASceneObject;
        NewEntry^.Name := aName;
        NewEntry^.Category := aCategory;
        Index := FSceneObjectList.Count;
        if Assigned(aBitmap) then
        begin
          bmp := TBitmap.Create;
          try
            // If we just add the bitmap, and it has different dimensions, then
            // all icons will be cleared, so ensure this doesn't happen
            { TODO : E2129 Cannot assign to a read-only property }
            (*bmp.PixelFormat := TPixelFormat.RGBA; //in VCL glpf24bit;*)
            { TODO : E2003 Undeclared identifier: 'Width', 'Height'}
            (*
            bmp.Width := FObjectIcons.Width;
            bmp.Height := FObjectIcons.Height;
            *)
            { TODO : E2003 Undeclared identifiers: 'SrcRect' etc.}
            (*bmp.Canvas.DrawBitmap(ABitmap, SrcRect, DstRect, AOpacity, HighSpeed);*)
            //in VCL bmp.Canvas.Draw(0, 0, ABitmap);
            { TODO : E2003 Undeclared identifier: 'AddMasked' }
            (*FObjectIcons.AddMasked(bmp, bmp.Canvas.Pixels[0, 0]);*)
            ImageIndex := FObjectIcons.Index - 1; //in VCL FObjectIcons.Count
          finally
            bmp.free;
          end;
        end
        else
          ImageIndex := 0;
      end;
      Add(NewEntry);
    finally
      //
    end;
  end;
end;

procedure TgxObjectManager.RegisterSceneObject(ASceneObject: TgxSceneObjectClass; const aName, aCategory: string; ResourceModule: Cardinal; ResourceName: string = '');
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

procedure TgxObjectManager.UnRegisterSceneObject(ASceneObject: TgxSceneObjectClass);
var
  oldEntry: PSceneObjectEntry;
begin
  // find the class in the scene object list
  OldEntry := FindSceneObjectClass(ASceneObject);
  // found?
  if assigned(OldEntry) then
  begin
    // remove its entry from the list of registered objects
    FSceneObjectList.Remove(OldEntry);
    // finally free the memory for the entry
    Dispose(OldEntry);
  end;
end;

procedure TgxObjectManager.CreateDefaultObjectIcons(ResourceModule: Cardinal);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  with FObjectIcons, bmp.Canvas do
  begin
    try
      // There's a more direct way for loading images into the image list, but
      // the image quality suffers too much
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_root');
      { TODO : E2003 Undeclared identifier: 'AddMasked' }
      (*FSceneRootIndex := AddMasked(bmp, Pixels[0, 0]);*)
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_camera');
      { TODO : E2003 Undeclared identifier: 'AddMasked' }
      (*FCameraRootIndex := AddMasked(bmp, Pixels[0, 0]);*)
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_lights');
      { TODO : E2003 Undeclared identifier: 'AddMasked' }
      (*FLightsourceRootIndex := AddMasked(bmp, Pixels[0, 0]);*)
      GLLoadBitmapFromInstance(ResourceModule, bmp, 'gls_objects');
      { TODO : E2003 Undeclared identifier: 'AddMasked' }
      (*FObjectRootIndex := AddMasked(bmp, Pixels[0, 0]);*)
    finally
      bmp.Free;
    end;
  end;
end;

procedure TgxObjectManager.DestroySceneObjectList;
var
  i: Integer;
begin
  with FSceneObjectList do
  begin
    for i := 0 to Count - 1 do
      Dispose(PSceneObjectEntry(Items[I]));
    Free;
  end;
end;

initialization

end.

