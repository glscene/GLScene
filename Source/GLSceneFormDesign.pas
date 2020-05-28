//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
   GLScene Form designer
}

unit GLSceneFormDesign;

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  VCL.Forms,
  ToolsAPI;

type

  TGLBaseSceneFormWizard = class(
      TNotifierObject,
      IOTAWizard,
      IOTAFormWizard,
      IOTACreator,
      IOTAModuleCreator,
      IOTARepositoryWizard,
      IOTARepositoryWizard60,
      IOTARepositoryWizard80)
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
  protected
    // IOTAWizard methods
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState;
    procedure Execute; virtual;
    // IOTAFormWizard methods
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    // IOTACreator methods
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator methods
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(
      const FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewImplSource(
      const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewIntfSource(const ModuleIdent, FormIdent,
      AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  public
    constructor CreateAndExecute(const AUnitIdent, AClassName, AFileName: string);
    // IOTARepositoryWizard
    function GetDesigner: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
  end;


  TGLSimpleSceneFormWizard = class(TGLBaseSceneFormWizard)
  protected
    function GetIDString: string; override;
    function GetName: string; override;
    function NewFormFile(
      const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(
      const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;


  TGLExtendedSceneFormWizard = class(TGLBaseSceneFormWizard)
  protected
    function GetIDString: string; override;
    function GetName: string; override;
    function NewFormFile(
      const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(
      const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;


  TGLBaseSceneProjectCreator = class(
    TInterfacedObject,
    IOTACreator,
    IOTAProjectCreator,
    IOTAProjectCreator50)
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
  public
    constructor Create(const AClassName, AUnitIdent, AFileName: string);
    // IOTACreator methods
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject); virtual;
  end;


  TGLBaseSceneProjectWizard = class(
      TNotifierObject,
      IOTAWizard,
      IOTARepositoryWizard,
      IOTARepositoryWizard60,
      IOTARepositoryWizard80,
      IOTAProjectWizard)
  public
    // IOTAWizard methods
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState;
    procedure Execute; virtual;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    function GetDesigner: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
  end;


  TGLSimpleSceneProjectWizard = class(TGLBaseSceneProjectWizard)
  public
    function GetIDString: string; override;
    function GetName: string; override;
    procedure Execute; override;
  end;


  TGLSimpleSceneProjectCreator = class(TGLBaseSceneProjectCreator)
  public
    procedure NewDefaultProjectModule(const Project: IOTAProject); override;
  end;


  TGLExtendedSceneProjectWizard = class(TGLBaseSceneProjectWizard)
  public
    function GetIDString: string; override;
    function GetName: string; override;
    procedure Execute; override;
  end;


  TGLExtendedSceneProjectCreator = class(TGLBaseSceneProjectCreator)
  public
    procedure NewDefaultProjectModule(const Project: IOTAProject); override;
  end;

resourcestring
  //-------------------------Projects------------------------------------------
  rBaseProjectLocalizedName = 'GLScene Base Application';
  rBaseProjectLocalizedDescription = 'Template of GLScene Base Application with TGLSceneForm';

  rSimpleProjectLocalizedName = 'GLScene Simple Application';
  rSimpleProjectLocalizedDescription = 'Template of GLScene Simple Application with TGLSceneForm';

  rExtendedProjectLocalizedName = 'GLScene Extended Application';
  rExtendedProjectLocalizedDescription = 'Template of GLScene Extended Application with TGLSceneForm';

  //--------------------------Units + LFM--------------------------------------
  //Base
  rBaseFormLocalizedName = 'GLSBaseForm';
  rBaseFormLocalizedDescription = 'GLSceneForm - a special form, which combines '+
                                  'the properties of the viewer and the normal '+
                                  'form and allows you to render directly into '+
                                  'the application window';
  //Simple
  rSimpleFormLocalizedName = 'GLSSimpleForm';
  rSimpleFormLocalizedDescription = 'GLSSimpleForm - a special form, which '+
                                    'combines the properties viewer and the '+
                                    'usual form and contains the basic set of '+
                                    'components to create a simple application.';
  //Extended
  rExtendedFormLocalizedName = 'GLSExtendedForm';
  rExtendedFormLocalizedDescription = 'GLSExtendedForm - a special form, which '+
                                       'combines the properties viewer and the '+
                                       'usual form, and contains an expanded set '+
                                       'of components needed to create more '+
                                       'complex applications.';

procedure Register;


//------------------------------------------------------------
implementation
//------------------------------------------------------------

{$R *.res}

uses
  GLSceneForm,
  GLScene,
  GLCadencer,

  DesignIntf,
  DesignEditors;


const
  LineEnding = #10#13;
  sCategoryGLSceneNew = 'Borland.Delphi.GLScene.New';

procedure Register;
begin
  RegisterCustomModule(TGLSceneForm, TCustomModule);
  RegisterPackageWizard(TGLBaseSceneFormWizard.Create);
  RegisterPackageWizard(TGLSimpleSceneFormWizard.Create);
  RegisterPackageWizard(TGLExtendedSceneFormWizard.Create);
  RegisterPackageWizard(TGLBaseSceneProjectWizard.Create);
  RegisterPackageWizard(TGLSimpleSceneProjectWizard.Create);
  RegisterPackageWizard(TGLExtendedSceneProjectWizard.Create);

end;

type

  TDelphiFile = class(TInterfacedObject)
  private
    FModuleName: string;
    FFormName: string;
    FAncestorName: string;
  public
    constructor Create(const ModuleName, FormName, AncestorName: string);
    function GetAge: TDateTime;
  end;

  TBaseUnitFile = class(TDelphiFile, IOTAFile)
  protected
    function GetSource: string;
  end;

  TBaseFormFile = class(TDelphiFile, IOTAFile)
  protected
    function GetSource: string;
  end;

  TSimpleUnitFile = class(TDelphiFile, IOTAFile)
  protected
    function GetSource: string;
  end;

  TSimpleFormFile = class(TDelphiFile, IOTAFile)
  protected
    function GetSource: string;
  end;

  TExtendedUnitFile = class(TDelphiFile, IOTAFile)
  protected
    function GetSource: string;
  end;

  TExtendedFormFile = class(TDelphiFile, IOTAFile)
  protected
    function GetSource: string;
  end;

  TBaseProjectFile = class(TDelphiFile, IOTAFile)
  protected
    FProjectName: String;
    function GetSource: string;
  public
    constructor CreateProject(const AProjectName, ModuleName, FormName: string);
  end;

var
  vCategory: IOTAGalleryCategory = nil;

procedure InitModule;
var
  Manager: IOTAGalleryCategoryManager;
  LCategory: IOTAGalleryCategory;
begin
  Manager := BorlandIDEServices as IOTAGalleryCategoryManager;
  if Assigned(Manager) then
  begin
    LCategory := Manager.FindCategory(sCategoryDelphiNew);
    if Assigned(LCategory) then
      vCategory := Manager.AddCategory(LCategory, sCategoryGLSceneNew,
        'GLScene VCL', 0);
  end;
end;

procedure DoneModule;
var
  Manager: IOTAGalleryCategoryManager;
begin
  Manager := BorlandIDEServices as IOTAGalleryCategoryManager;
  if Assigned(Manager) then
  begin
    if Assigned(vCategory) then
      Manager.DeleteCategory(vCategory);
  end;
end;

function GetActiveProjectGroup: IOTAProjectGroup;
var
  ModuleServices: IOTAModuleServices;
  I: Integer;
begin
  Result := NIL;
  ModuleServices := BorlandIDEServices As IOTAModuleServices;
  for I := 0 to ModuleServices.ModuleCount - 1 Do
    if Succeeded(ModuleServices.Modules[I].QueryInterface(IOTAProjectGroup,
      Result)) then
        break;
end;

// TBaseFile
//

constructor TDelphiFile.Create(const ModuleName, FormName, AncestorName: string);
begin
  inherited Create;
  FModuleName := ModuleName;
  FFormName := FormName;
  FAncestorName := AncestorName;
end;

function TDelphiFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TBaseUnitFile.GetSource: string;
const
  sSource =
    'unit %0:s;' + LineEnding +
    '' + LineEnding +
    'interface' + LineEnding +
    '' + LineEnding +
    'uses' + LineEnding +
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,' + LineEnding +
    '  GLScene, GLSceneForm;' + LineEnding +
    '' + LineEnding +
    'type' + LineEnding +
    '  T%1:s = class(TGLSceneForm)' + LineEnding +
    '  private' + LineEnding +
    '    { Private declarations }' + LineEnding +
    '  public' + LineEnding +
    '    { Public declarations }' + LineEnding +
    '  end;' + LineEnding +
    '' + LineEnding +
    'var' + LineEnding +
    '  %1:s : T%1:s;' + LineEnding +
    '' + LineEnding +
    'implementation' + LineEnding +
    '' + LineEnding +
    '{$R *.dfm}' + LineEnding +
    '' + LineEnding +
    '' + LineEnding +
    'end.';
begin
  Result := Format(sSource, [FModuleName, FFormName]);
end;

function TBaseFormFile.GetSource: string;
const
  FormText =
    'object %0:s: T%0:s' + LineEnding +
    '  Left = 246' + LineEnding   +
    '  Height = 600' + LineEnding   +
    '  Top = 74' + LineEnding   +
    '  Width = 800' + LineEnding   +
    '  Buffer.BackgroundColor = 2064383' + LineEnding   +
    'end';
begin
  Result := Format(FormText, [FFormName]);
end;

function TSimpleUnitFile.GetSource: string;
const
  sSource =
    'unit %0:s;' + LineEnding +
    '' + LineEnding +
    'interface' + LineEnding +
    '' + LineEnding +
    'uses' + LineEnding +
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,' + LineEnding +
    '  GLScene, GLSceneForm, GLCadencer;' + LineEnding +
    '' + LineEnding +
    'type' + LineEnding +
    '  T%1:s = class(TGLSceneForm)' + LineEnding +
    '    GLScene1: TGLScene;' + LineEnding +
    '    GLCadencer1: TGLCadencer;' + LineEnding +
    '    GLCamera1: TGLCamera;' + LineEnding +
    '    GLLightSource1: TGLLightSource;' + LineEnding +
    '    GLDummyCube1: TGLDummyCube;' + LineEnding +
    '    GLCube1: TGLCube;' + LineEnding +
    '    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + LineEnding +
    '  private' + LineEnding +
    '    { Private declarations }' + LineEnding +
    '  public' + LineEnding +
    '    { Public declarations }' + LineEnding +
    '  end;' + LineEnding +
    '' + LineEnding +
    'var' + LineEnding +
    '  %1:s : T%1:s;' + LineEnding +
    '' + LineEnding +
    'implementation' + LineEnding +
    '' + LineEnding +
    '{$R *.dfm}' + LineEnding +
    '' + LineEnding +
    'procedure T%1:s.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + LineEnding +
    'begin' + LineEnding +
    '  Invalidate;' + LineEnding +
    'end;' + LineEnding +
    '' + LineEnding +
    'end.';
begin
  Result := Format(sSource, [FModuleName, FFormName]);
end;

function TSimpleFormFile.GetSource: string;
const
  FormText =
    'object %0:s: T%0:s' + LineEnding +
    '  Left = 246' + LineEnding   +
    '  Height = 600' + LineEnding   +
    '  Top = 74' + LineEnding   +
    '  Width = 800' + LineEnding   +
    '  Buffer.BackgroundColor = 2064383' + LineEnding   +
    '  Camera = GLCamera1' + LineEnding   +
    '  object GLScene1: TGLScene' + LineEnding   +
    '    Left = 24' + LineEnding   +
    '    Top = 24' + LineEnding   +
    '    object GLCamera1: TGLCamera' + LineEnding   +
    '      DepthOfView = 100.000000000000000000' + LineEnding   +
    '      FocalLength = 50.000000000000000000' + LineEnding   +
    '      TargetObject = GLDummyCube1' + LineEnding   +
    '      Position.Coordinates = {0000803F00000040000040400000803F}' + LineEnding   +
    '      object GLLightSource1: TGLLightSource' + LineEnding   +
    '        ConstAttenuation = 1.000000000000000000' + LineEnding   +
    '        SpotCutOff = 180.000000000000000000' + LineEnding   +
    '      end' + LineEnding   +
    '    end' + LineEnding   +
    '    object GLDummyCube1: TGLDummyCube' + LineEnding   +
    '    end ' + LineEnding   +
    '    object GLCube1: TGLCube' + LineEnding   +
    '      TagFloat = 0' + LineEnding   +
    '      PitchAngle = 0' + LineEnding   +
    '      RollAngle = 0' + LineEnding   +
    '      TurnAngle = 0' + LineEnding   +
    '    end' + LineEnding   +
    '  end' + LineEnding   +
    '  object GLCadencer1: TGLCadencer' + LineEnding   +
    '    Scene = GLScene1' + LineEnding   +
    '    OnProgress = GLCadencer1Progress' + LineEnding   +
    '    Left = 64' + LineEnding   +
    '    Top = 24' + LineEnding   +
    '  end' + LineEnding   +
    'end';
begin
  Result := Format(FormText, [FFormName]);
end;

function TExtendedUnitFile.GetSource: string;
const
  sSource =
    'unit %0:s;' + LineEnding +
    '' + LineEnding +
    'interface' + LineEnding +
    '' + LineEnding +
    'uses' + LineEnding +
    '  Windows, Messages, Classes, SysUtils, Forms, Controls, Graphics,' + LineEnding +
    '  Dialogs, GLScene, GLSceneForm, GLCadencer, GLMaterial, GLObjects, ' + LineEnding +
    '  GLHUDObjects, GLWindowsFont, GLSkydome, GLGeomObjects, GLShadowPlane,' + LineEnding +
    '  GLViewer, GLBaseClasses, GLFilePNG;' + LineEnding +

    '' + LineEnding +
    'type' + LineEnding +
    '  T%1:s = class(TGLSceneForm)' + LineEnding +
    '    GLScene1: TGLScene;' + LineEnding +
    '    GLCadencer1: TGLCadencer;' + LineEnding +
    '    GLCamera1: TGLCamera;' + LineEnding +
    '    GLLightSource1: TGLLightSource;' + LineEnding +
    '    GLBackground: TGLDummyCube;' + LineEnding +
    '    GLWorld: TGLDummyCube;' + LineEnding +
    '    GLCameraTarget: TGLDummyCube;' + LineEnding +
    '    GLInterface: TGLDummyCube;' + LineEnding +
    '    GLCone1: TGLCone;' + LineEnding +
    '    ShadowObjects: TGLDummyCube;' + LineEnding +
    '    GLShadowPlane1: TGLShadowPlane;' + LineEnding +
    '    GLWorldObject1: TGLCube;' + LineEnding +
    '    GLWorldObject4: TGLCylinder;' + LineEnding +
    '    GLInterfaceSprite1: TGLHUDSprite;' + LineEnding +
    '    GLInterfaceText1: TGLHUDText;' + LineEnding +
    '    GLSkyDome1: TGLSkyDome;' + LineEnding +
    '    GLWorldObject3: TGLSphere;' + LineEnding +
    '    GLWindowsBitmapFont1: TGLWindowsBitmapFont;' + LineEnding +
    '    GLMaterialLibrary1: TGLMaterialLibrary;' + LineEnding +
    '    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + LineEnding +
    '  private' + LineEnding +
    '    { Private declarations }' + LineEnding +
    '  public' + LineEnding +
    '    { Public declarations }' + LineEnding +
    '  end;' + LineEnding +
    '' + LineEnding +
    'var' + LineEnding +
    '  %1:s : T%1:s;' + LineEnding +
    '' + LineEnding +
    'implementation' + LineEnding +
    '' + LineEnding +
    '{$R *.dfm}' + LineEnding +
    '' + LineEnding +
    'procedure T%1:s.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + LineEnding +
    'begin' + LineEnding +
    '  Invalidate;' + LineEnding +
    'end;' + LineEnding +
    '' + LineEnding +
    'end.';
begin
  Result := Format(sSource, [FModuleName, FFormName]);
end;

function TExtendedFormFile.GetSource: string;
const
  FormText =
    'object %0:s: T%0:s' + LineEnding +
    '  Left = 246' + LineEnding +
    '  Top = 74' + LineEnding +
    '  Caption = ''GLScene''' + LineEnding +
    '  ClientHeight = 562' + LineEnding +
    '  ClientWidth = 784' + LineEnding +
    '  Color = clBtnFace' + LineEnding +
    '  Font.Charset = DEFAULT_CHARSET' + LineEnding +
    '  Font.Color = clWindowText' + LineEnding +
    '  Font.Height = -11' + LineEnding +
    '  Font.Name = ''Tahoma''' + LineEnding +
    '  Font.Style = []' + LineEnding +
    '  OldCreateOrder = False' + LineEnding +
    '  Camera = GLCamera1' + LineEnding +
    '  Buffer.BackgroundColor = clWhite' + LineEnding +
    '  Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]' + LineEnding +
    '  Buffer.AntiAliasing = aa8x' + LineEnding +
    '  FieldOfView = 161.075363159179700000' + LineEnding +
    '  PixelsPerInch = 96' + LineEnding +
    '  TextHeight = 13' + LineEnding +
    '  object GLScene1: TGLScene' + LineEnding +
    '    Left = 72' + LineEnding +
    '    Top = 39' + LineEnding +
    '    object GLBackground: TGLDummyCube' + LineEnding +
    '      CubeSize = 1.000000000000000000' + LineEnding +
    '      object GLSkyDome1: TGLSkyDome' + LineEnding +
    '        Direction.Coordinates = {000000000000803F2EBD3BB300000000}' + LineEnding +
    '        PitchAngle = 90.000000000000000000' + LineEnding +
    '        Position.Coordinates = {000000000000F0C1000000000000803F}' + LineEnding +
    '        Up.Coordinates = {000000002EBD3BB3000080BF00000000}' + LineEnding +
    '        Bands = <' + LineEnding +
    '          item' + LineEnding +
    '            StartColor.Color = {0000803F0000803F0000803F0000803F}' + LineEnding +
    '            StopAngle = 15.000000000000000000' + LineEnding +
    '            Slices = 20' + LineEnding +
    '          end' + LineEnding +
    '          item' + LineEnding +
    '            StartAngle = 15.000000000000000000' + LineEnding +
    '            StopAngle = 90.000000000000000000' + LineEnding +
    '            StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}' + LineEnding +
    '            Slices = 20' + LineEnding +
    '            Stacks = 4' + LineEnding +
    '          end>' + LineEnding +
    '        Stars = <>' + LineEnding +
    '      end' + LineEnding +
    '    end' + LineEnding +
    '    object GLWorld: TGLDummyCube' + LineEnding +
    '      CubeSize = 1.000000000000000000' + LineEnding +
    '      object GLLightSource1: TGLLightSource' + LineEnding +
    '        ConstAttenuation = 1.000000000000000000' + LineEnding +
    '        Diffuse.Color = {DBDA5A3FDBDA5A3FDBDA5A3F0000803F}' + LineEnding +
    '        Position.Coordinates = {000000400000C040000000400000803F}' + LineEnding +
    '        SpotCutOff = 180.000000000000000000' + LineEnding +
    '      end' + LineEnding +
    '      object GLCamera1: TGLCamera' + LineEnding +
    '        DepthOfView = 100.000000000000000000' + LineEnding +
    '        FocalLength = 50.000000000000000000' + LineEnding +
    '        TargetObject = GLCameraTarget' + LineEnding +
    '        Position.Coordinates = {0000C0400000C0400000C0400000803F}' + LineEnding +
    '      end' + LineEnding +
    '      object GLCameraTarget: TGLDummyCube' + LineEnding +
    '        CubeSize = 1.000000000000000000' + LineEnding +
    '      end' + LineEnding +
    '      object ShadowObjects: TGLDummyCube' + LineEnding +
    '        CubeSize = 1.000000000000000000' + LineEnding +
    '        object GLWorldObject4: TGLCylinder' + LineEnding +
    '          Material.FrontProperties.Diffuse.Color = {CDCC4C3F8584043FC1C0403D0000803F}' + LineEnding +
    '          Position.Coordinates = {0000000000000000000000C00000803F}' + LineEnding +
    '          BottomRadius = 1.000000000000000000' + LineEnding +
    '          Height = 2.000000000000000000' + LineEnding +
    '          TopRadius = 1.000000000000000000' + LineEnding +
    '        end' + LineEnding +
    '        object GLWorldObject3: TGLSphere' + LineEnding +
    '          Material.FrontProperties.Diffuse.Color = {8180803CCDCC4C3FCDCC4C3F0000803F}' + LineEnding +
    '          Position.Coordinates = {000080BF00000040000000000000803F}' + LineEnding +
    '          Radius = 1.000000000000000000' + LineEnding +
    '        end' + LineEnding +
    '        object GLWorldObject1: TGLCube' + LineEnding +
    '          Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}' + LineEnding +
    '          Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}' + LineEnding +
    '          Position.Coordinates = {0000000000000000000040400000803F}' + LineEnding +
    '        end' + LineEnding +
    '        object GLCone1: TGLCone' + LineEnding +
    '          Direction.Coordinates = {00000000000080BF2EBD3BB300000000}' + LineEnding +
    '          PitchAngle = -90.000000000000000000' + LineEnding +
    '          Position.Coordinates = {000000400000803F000080BF0000803F}' + LineEnding +
    '          Up.Coordinates = {000000002EBD3BB30000803F00000000}' + LineEnding +
    '          BottomRadius = 0.500000000000000000' + LineEnding +
    '          Height = 1.000000000000000000' + LineEnding +
    '        end' + LineEnding +
    '      end' + LineEnding +
    '      object GLShadowPlane1: TGLShadowPlane' + LineEnding +
    '        Direction.Coordinates = {000000000000803F2EBD3BB300000000}' + LineEnding +
    '        PitchAngle = 90.000000000000000000' + LineEnding +
    '        Position.Coordinates = {00000000000000C0000000000000803F}' + LineEnding +
    '        Up.Coordinates = {000000002EBD3BB3000080BF00000000}' + LineEnding +
    '        Height = 10.000000000000000000' + LineEnding +
    '        Width = 10.000000000000000000' + LineEnding +
    '        ShadowingObject = ShadowObjects' + LineEnding +
    '        ShadowedLight = GLLightSource1' + LineEnding +
    '        ShadowColor.Color = {9A99993E9A99993E9A99993E0000803F}' + LineEnding +
    '      end' + LineEnding +
    '    end' + LineEnding +
    '    object GLInterface: TGLDummyCube' + LineEnding +
    '      CubeSize = 1.000000000000000000' + LineEnding +
    '      object GLInterfaceText1: TGLHUDText' + LineEnding +
    '        Position.Coordinates = {0000F04100002042000000000000803F}' + LineEnding +
    '        BitmapFont = GLWindowsBitmapFont1' + LineEnding +
    '        Text = ''GLScene Project''' + LineEnding +
    '        ModulateColor.Color = {D0CF4F3FDBDA5A3FF6F5753F0000803F}' + LineEnding +
    '      end' + LineEnding +
    '      object GLInterfaceSprite1: TGLHUDSprite' + LineEnding +
    '        Material.MaterialLibrary = GLMaterialLibrary1' + LineEnding +
    '        Material.LibMaterialName = ''LibMaterial''' + LineEnding +
    '        Position.Coordinates = {008027440000FA43000000000000803F}' + LineEnding +
    '        Width = 200.000000000000000000' + LineEnding +
    '        Height = 100.000000000000000000' + LineEnding +
    '      end' + LineEnding +
    '    end' + LineEnding +
    '  end' + LineEnding +
    '  object GLCadencer1: TGLCadencer' + LineEnding +
    '    Scene = GLScene1' + LineEnding +
    '    OnProgress = GLCadencer1Progress' + LineEnding +
    '    Left = 112' + LineEnding +
    '    Top = 39' + LineEnding +
    '  end' + LineEnding +
    '  object GLMaterialLibrary1: TGLMaterialLibrary' + LineEnding +
    '    Materials = <' + LineEnding +
    '      item' + LineEnding +
    '        Name = ''LibMaterial''' + LineEnding +
    '        Material.BlendingMode = bmModulate' + LineEnding +
    '        Material.Texture.Image.Picture.Data = {' + LineEnding +
    '          07544269746D617066370000424D66370000000000003604000028000000A600' + LineEnding +
    '          00004E0000000100080000000000303300007600000076000000000100000000' + LineEnding +
    '          0000000000000101010002020200030303000404040005050500060606000707' + LineEnding +
    '          070008080800090909000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F' + LineEnding +
    '          0F00101010001111110012121200131313001414140015151500161616001717' + LineEnding +
    '          170018181800191919001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F' + LineEnding +
    '          1F00202020002121210022222200232323002424240025252500262626002727' + LineEnding +
    '          270028282800292929002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F' + LineEnding +
    '          2F00303030003131310032323200333333003434340035353500363636003737' + LineEnding +
    '          370038383800393939003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F' + LineEnding +
    '          3F00404040004141410042424200434343004444440045454500464646004747' + LineEnding +
    '          470048484800494949004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F' + LineEnding +
    '          4F00505050005151510052525200535353005454540055555500565656005757' + LineEnding +
    '          570058585800595959005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E005F5F' + LineEnding +
    '          5F00606060006161610062626200636363006464640065656500666666006767' + LineEnding +
    '          670068686800696969006A6A6A006B6B6B006C6C6C006D6D6D006E6E6E006F6F' + LineEnding +
    '          6F00707070007171710072727200737373007474740075757500767676007777' + LineEnding +
    '          770078787800797979007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F' + LineEnding +
    '          7F00808080008181810082828200838383008484840085858500868686008787' + LineEnding +
    '          870088888800898989008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008F8F' + LineEnding +
    '          8F00909090009191910092929200939393009494940095959500969696009797' + LineEnding +
    '          970098989800999999009A9A9A009B9B9B009C9C9C009D9D9D009E9E9E009F9F' + LineEnding +
    '          9F00A0A0A000A1A1A100A2A2A200A3A3A300A4A4A400A5A5A500A6A6A600A7A7' + LineEnding +
    '          A700A8A8A800A9A9A900AAAAAA00ABABAB00ACACAC00ADADAD00AEAEAE00AFAF' + LineEnding +
    '          AF00B0B0B000B1B1B100B2B2B200B3B3B300B4B4B400B5B5B500B6B6B600B7B7' + LineEnding +
    '          B700B8B8B800B9B9B900BABABA00BBBBBB00BCBCBC00BDBDBD00BEBEBE00BFBF' + LineEnding +
    '          BF00C0C0C000C1C1C100C2C2C200C3C3C300C4C4C400C5C5C500C6C6C600C7C7' + LineEnding +
    '          C700C8C8C800C9C9C900CACACA00CBCBCB00CCCCCC00CDCDCD00CECECE00CFCF' + LineEnding +
    '          CF00D0D0D000D1D1D100D2D2D200D3D3D300D4D4D400D5D5D500D6D6D600D7D7' + LineEnding +
    '          D700D8D8D800D9D9D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDF' + LineEnding +
    '          DF00E0E0E000E1E1E100E2E2E200E3E3E300E4E4E400E5E5E500E6E6E600E7E7' + LineEnding +
    '          E700E8E8E800E9E9E900EAEAEA00EBEBEB00ECECEC00EDEDED00EEEEEE00EFEF' + LineEnding +
    '          EF00F0F0F000F1F1F100F2F2F200F3F3F300F4F4F400F5F5F500F6F6F600F7F7' + LineEnding +
    '          F700F8F8F800F9F9F900FAFAFA00FBFBFB00FCFCFC00FDFDFD00FEFEFE00FFFF' + LineEnding +
    '          FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECE2FCFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFEFDFE8FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF974C5BADF9FFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1E0E7FEFDFDD706000004' + LineEnding +
    '          B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD8555E' + LineEnding +
    '          D3CBDAD90000520041FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFF7C003D284EFFB599E40222FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF38000038FFFFFBAE0051FFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA3000082EAFED63C00' + LineEnding +
    '          B9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCF' + LineEnding +
    '          00061766D254005CEDE8E9EFF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFC80000019C450046A18170848CA7DAE8F9FFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9C00003C3200010E00000B4CA1A47C8CAF' + LineEnding +
    '          E7FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1530003180000000000' + LineEnding +
    '          00000F0008388A7F7AB6F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEC009' + LineEnding +
    '          0000000000003D2F0000109A11005F6B1F479FFAFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFE74D0000000000000012F2B30800B3AC000CFFB31417ACFEFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFEA80100000C5571641920DADF6A50E9E90151FF' + LineEnding +
    '          FFB30015D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF40000346C8FFF9DEB97E5D' + LineEnding +
    '          4788C4C5C188DDE9F9FF590050FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD502007C' + LineEnding +
    '          4497F5C1590700154A7699AAA5A39D829ED6BD0002DEFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFA400337E45FDBD22002F868127018ACCD4D07E7B8859A810008BEDF6' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFF8700983CCEE63A044A1A1C0000001D061C72CBBC' + LineEnding +
    '          7673771C0082D1D9F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8613B252FBAC004D06001C64' + LineEnding +
    '          717144021F9DC6C6D8D95691448FC6CAD5FBFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFAFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDF5F2F8FFFF9775AD65' + LineEnding +
    '          FF6B1236007FE6D5D2DFE8A31726D0CDC5CAE22E66927EC3CCD1FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFE5CBB3B4DFFFFFFFFFFFFFFFE59AFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8471C9FFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9DEC49C' + LineEnding +
    '          8798C7E6BAB18257FF513B0B6EE988321F3E9EC98F00AFB7140763091175AC76' + LineEnding +
    '          A2D4E1FFFFFFFFFFFFFFFFFFFFFFEA7D19000030DCFFFFF9B8BBF9C619B7DDFF' + LineEnding +
    '          D3B2D7DBEEE2E5FFC9B2C1F3CCCCE2FFEACCCCE5CCF9D6F396750BEBD1EADDF3' + LineEnding +
    '          D6FFC6B2E8FFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFF6C35B2A52B0A17076B09B8328F2583913D9AE01000100039DD12DF28A' + LineEnding +
    '          000000007AFA816C5D7AD8FAFFF9FFFFFFFFFFFFFFFFD00000122B0051FFFF46' + LineEnding +
    '          42424CC6006009B9197DAD4CAA717F71126B08C60955A0FF991C55AA00E233B0' + LineEnding +
    '          124600AA1C9955C6339A257CCCFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFECB3FDFFFFE572E1CD28010E4B6471553D79578F8BB381082FE2860000' + LineEnding +
    '          A0500042E9E8FFE53C060C61E3CCC6AF340F3AB4F1C1CAEFE9F8FFFFFFFCE410' + LineEnding +
    '          00189F0006FFE802F2F202B316FF197113324E4CA66B7F09B3C611C61CFFFFFF' + LineEnding +
    '          9942C6EE00DF2D8D16F900AA1C9951C033711C2D81FFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFF7B48D6FFEF6057B65F0043B9F9FFFFF2902B69C971' + LineEnding +
    '          72D30C118BCB0B02A9AD001AF6FFFFFCCAC6C6C6C6D4EFFEDA6608086C6D0D80' + LineEnding +
    '          638BD9B2D4D78D7E5A99480033FFE807F9F907AC0B3D1CD43041794C2D11AF05' + LineEnding +
    '          C3AA7FC61CFFFFFF992266D1003B11DD223805AA1C99132A63C02D33AFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFD6BFFCFFFFEC0A0653EC6FE3DA802E000103B0' + LineEnding +
    '          FFFDCF9570174880738996030FAAD1CECF65001EF9FFFFF3C6C6C6C6D2FFFFFF' + LineEnding +
    '          E2C6BC420012000400002D231361040001110408C3FFFF44585835FFFFEBF5FF' + LineEnding +
    '          F6E5FFFFF8E5FF6E228488C61CFFFFFF992571C6FFF2EEFFF8E8FFF57DFFFFE5' + LineEnding +
    '          F9FFF2EBFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF888E6FDDFEA00000009178' + LineEnding +
    '          E7FE6A68000002C9FFF94F000000003E736C93A01A07435228000049F2F8DEC8' + LineEnding +
    '          C6C6C6C6E1FFFFFFE0C6D2EDB8320542420C00000000011665410CB2FFFFFFF0' + LineEnding +
    '          9793EEFFFFFFFFFFFFFFFFFFFFFFFFFFB68DAAECB3FFFFFFDDAAAADDFFFFFFFF' + LineEnding +
    '          FFFFFFF584FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF747CEB' + LineEnding +
    '          7B8A7644000012BBFFFF75DA1922C0FFFFFE41A6DFCF7D0F3177608DBA965C53' + LineEnding +
    '          492303A1D2C7C6D9ECD1C8C6F2FFFFFFD3C6CDE1E7F0B84C599EAE8F8A92B9B2' + LineEnding +
    '          6752C3DFE9FAFFFDE6E0DFDFDFDFE0DFDFDFD9D9D9D9E4DFDFDFE5F4FFFFFFFF' + LineEnding +
    '          FFFFFAE7FFE8DFDFDFDFDFE0DFDFDDD9D9D9E5FFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFF81F5FFFFD2734916000045EDFF8B6400BDFFFFFFFFFCFFFFFD7000' + LineEnding +
    '          2F206D7288918F5C080688C7C6DCF4FFFFFFF7C67BE8DBCDAE846554546991D8' + LineEnding +
    '          E8AA7A797F7C6E819C724D424A71D4D594936645464A5C827F7D5D3A312D6883' + LineEnding +
    '          644B8999F9FFFFFFFFF8CF75C1D8875646464A6B7F7F795337312EC1F8F9FFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFA3D5FFFFFFFFC35E43070083F8A30251FFFFFF' + LineEnding +
    '          FFFFFFFEE5A23C0009832824637B7E3000127CBFC6DCFFFFFFFFFFDA4A5B541B' + LineEnding +
    '          3395D1E2D17A091280F3FFFFFFF4FD6E0112AAFCFFD9685804FFD403794355FF' + LineEnding +
    '          FFFFFFFFB3147FFFC935FFFFFFFFFFFFFBFB75009DADEE9B07A7048DFFFFFFFF' + LineEnding +
    '          F98B02AF8EECFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCC8DF2FFFFFFFFFA96531D' + LineEnding +
    '          0ED95400D2FFFFFFFFFFF0AD37000000092E495656492F0D0000001070C3DCFD' + LineEnding +
    '          FFFFFFF562000170CDCFFEFFFFFFBD060044F4FFE1AACE9502CCFFFFFFFFFF5F' + LineEnding +
    '          09FFE3094A2755FFFFFFFFFFFF887FFFD338FFFFFFFFFFF5D2D2D020AAF2FFA1' + LineEnding +
    '          0868008DFFFFFFFFFFFC489A5CFFFFD0F1FFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF040EA' + LineEnding +
    '          FFFFFFFFFFFFE6581F6C5049FFFFFFFFFDD25002000F536359371F2226192E4D' + LineEnding +
    '          5B5A2400002093CCE1ECEDE5730072CFD1FEFFFFFFFFFE52000058F2B4141F28' + LineEnding +
    '          5BFFFFFFFFFFFFE40AEEE212000FAFFFFFFFF8FFFFDA7FFFCE38FFFFFFFFFDD3' + LineEnding +
    '          761C4D4BACFFFFA00D002CCEFFFFFFF8FFFF98912CF4F15C1CD3FFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFC37EFEFFFFFFFFFFFFEA55088BB6FFFFFFFAB51C00045B621400' + LineEnding +
    '          0000CAFFFFEC00000003457020000068C6C6C6C6741BD5CCFAFFF8FFFFFFF370' + LineEnding +
    '          107600AC43000000C0FFFFFFFFFFFFFD4DE2E205000EC2FFFFFFE9D7FFFFDFFF' + LineEnding +
    '          D338FFF4F4E4D38D03000200A3FCFB9E00003DD9FFFFFFD2EBFFEFE7521C2F2D' + LineEnding +
    '          01ACFCF3FBFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE4C8CED3' + LineEnding +
    '          D9DEE3E8EDF2F8FCFFFFFFFFFFFFFFAE94FEFFFFFFFFFFFFAD0022FEFFFFFDAA' + LineEnding +
    '          0F001E751C0000000000BAFFFFDC000000000003594D00005CC8CDCC6F53E8F7' + LineEnding +
    '          EEFFFFECFFF7CF46189B006802020202E8FFFFFFFFFFFFFFD2ECE205050364FF' + LineEnding +
    '          FFFCD5CEFFFFFFFFD33BFCFA8DC3660700000018A8FEF38E0009099DFFFFF3CE' + LineEnding +
    '          E8FFFFFFCB6B64441A56452CB4FFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFF4C03122332425362728392A1B5C8D1D6DBDFB8A7B3C2FEFFFFFFFFFF' + LineEnding +
    '          A40063FFEEF4B70D003558020000003F5A00A1FFFFCF00526B110000002D6605' + LineEnding +
    '          0056E4D581B7FFFFFFFFFFF7DDC46A0100000032001C1500EAF9FFFFFFFFFFFF' + LineEnding +
    '          FFFFE205010352F6ECE5889BFFFFF5F8D12889DCCF8A0B0000000C6999D0FC90' + LineEnding +
    '          00000489F4EBE058CCFFFFFFF996666EBD47034CA4D2FEFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFF87010000000000000000000000000D1F31423178' + LineEnding +
    '          1B9ACDE3F6FFFFD56D1BB0FD5B641F00324B0000003BCCFFE29DE5FFFFF3B6CF' + LineEnding +
    '          FFF37D060000136400005C30D3FFFFFFFFF3DAC2731600000000002A00857319' + LineEnding +
    '          FFFFFFFFFFFFFFFBFFFFE2050004329F917E76A1FFFFEEFDB62F4220590C0000' + LineEnding +
    '          000F586698C0F9A000000356AA7F805BCCFFFFFFFFFFFFFFFFECC84600009FFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBC6955000000000000000000' + LineEnding +
    '          000000000000000000002E71B2D1E67F0891F2F60F00001A5B00000071FBFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFCC1300001E5000000090FFFCFCDEBF701C00000000' + LineEnding +
    '          0000005F0000000CFFFFFFFFFFFFFFF8F8FFE40E00004EE2CEAF4E99FFFFF9EC' + LineEnding +
    '          B0227F640300000020B8E36094CBFFAA01000081E2C39730CCFFFFFFEE945E71' + LineEnding +
    '          CE500750A7D2FFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC440000' + LineEnding +
    '          00000000000000000000000000000000000000000232784A003BFFFFAF020769' + LineEnding +
    '          1B0C0028DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA4C000B4F35300049FAECCEB6' + LineEnding +
    '          842100000000000000005E8F00001423EFFFFFFFFFFFFFE9F4FFE20B000155FF' + LineEnding +
    '          FFFF9782FFFFF2E9CF35372000000026CEF3E76CA9F3FCA50000008DFFFFFC56' + LineEnding +
    '          CCFFFFFFC05A5E451857492AB7FFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFDF0B00000000000000000000000000000000000000000000000070' + LineEnding +
    '          006BFFFFFE465413C9DD3B09CCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F3BDFFC' + LineEnding +
    '          3B5C0BDBE799911200000000000000000058F6EF15000000BDFFFFFFFFFFFFD5' + LineEnding +
    '          6AFFE20600005EFFFFFFE185F8D6D5C0BB13260000001FC2D5FFF86C93ECF3A0' + LineEnding +
    '          01000097FFFFFFA4BDE9E5F24B19333101ACFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFF820000000000000000000000000000000000' + LineEnding +
    '          0000000000000077069AFFFFE1105467FFFFFBD8EAD7CCD5D0E1F9FFFFE8BFC1' + LineEnding +
    '          CDCBB8BCBCBAC6E6CC0F5286DE5600000000000000000011A0FFFFFF87000000' + LineEnding +
    '          6EFFFFFFFFFFF9AB57FFE20500016EFFFFFFFFFFE5639C71680000000011C3CC' + LineEnding +
    '          C2C1FF6CA6FFFFA000000C9AFFFFFFFFFFD747DA26F5FF610CCEFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6220000000000000000' + LineEnding +
    '          0000000000000000000000000000001987BFFFFF87531ACFFEFFFCCC652D4C68' + LineEnding +
    '          083895EDFFB8033952633B171C124481E4346025D73900000000000000066BE5' + LineEnding +
    '          FFFFFFFFFB3E000024FBFFFFFFFFD86655FFE1065A3255FFFFFFFFEDC91F0725' + LineEnding +
    '          0E0583010066E5FFD18AFE6CAAFFFFA00378018DFFFFFFFDE4B239EE5AFFFFC9' + LineEnding +
    '          E2FFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3' + LineEnding +
    '          020000000000000000000000000000000000000000083C001CEBFFFD3168003A' + LineEnding +
    '          97E7C33213060A9E0000005BF1BB006154755300050B244B2A00382A9B110000' + LineEnding +
    '          0005000059DFFFFFFFFFFFFFFFEF49000067E7F0E9D9A10F48E3D0045E334CDC' + LineEnding +
    '          DCE1D2A14C10A69A37057D011AA4DDF7F0DADE6A9BE4DD9705880380DCDCDFC6' + LineEnding +
    '          913149FFA5EBFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFF715A31000000000000000000000100000000000000017F00' + LineEnding +
    '          00E8FFE70E59000000532C030C19AAF65E03000080B101003CAAA6979B977D00' + LineEnding +
    '          0000076C51004D39000351E2FFFFFFFFFFFFFFFFFFFFFFB03E002E7B916167B2' + LineEnding +
    '          64A068555552425555555B38384271585051552CE36B90F2C8AA7A5561717961' + LineEnding +
    '          5555484855556B42383884FFFFF8FFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2600F0000000C1D2F4253647CB18F39' + LineEnding +
    '          130100000000692600FCECA43F2900005B55090316DFFFDAB47C050013B5270D' + LineEnding +
    '          1FFFFFFFFFFFFF90380000782B00664D08BCFFFFFFFFFFFFE1E1FFFFFFFFFFFF' + LineEnding +
    '          FFF2CCC6D2FBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6B0FFC3F8C3C6F9' + LineEnding +
    '          C2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBF8F3EDE9E5DCC9B37E5E6071797E' + LineEnding +
    '          83878B91AA3FF7DDD7D8CBBCA476017A36853F0961090013FF6E49478AFFFF92' + LineEnding +
    '          10553A3C0077401133FFFFFFFFFFFFFF7F0000770000000096FFFFFFFFFFFFFB' + LineEnding +
    '          BF2DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          F974EDFFFFFFFFF6F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFD1A8917A6A6C72767C818B' + LineEnding +
    '          A0B8CFE5FAFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFFFDB0000541E000067000038' + LineEnding +
    '          FF3F0D0BBBFFFF871F8469450052020D59FFFFFFFFFFFFFFA500006D00000000' + LineEnding +
    '          DEF4F5FBFFFFFFE1712FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFD9FFFFFFFFD7FFFFFFFEFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFF65BBD1' + LineEnding +
    '          E8FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE00000' + LineEnding +
    '          00974C0065000040FF236046DDFFFFD7ACAAC5AAAAA001003BFFFFFFFFFFFFFF' + LineEnding +
    '          AC00006100020100A7CBCDD3E8FCE6B51338FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFADC4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFD410000D17A007100002CFF290000B8FFFFFFFFFFCDE1F8B10000' + LineEnding +
    '          2BFFFFFFFFFFFFFF98000071001487112AB5C6C6C6CEA0240038FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFF96FA1E3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFEA842000B097005322000BE24B000064FCFFFF' + LineEnding +
    '          FFE3232B8DAE00001CFFFFFFFFFFFEC4620000660027CDCD641A3A4B592F1A68' + LineEnding +
    '          972FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFF888EEFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC06766996C000294A0000' + LineEnding +
    '          323100000CACE7F2D96100001CAC00001CFFFFFFFFFFEB0B060000670045F7F7' + LineEnding +
    '          CAF5D6BCAEBACBF8F5B9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFC79' + LineEnding +
    '          D3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F00' + LineEnding +
    '          1989F3050371000002773B000003459A27000002ACB1000035FFFFFFFFFFEB59' + LineEnding +
    '          0500144F0082FFFFD2CBDCD3CCD2C6ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFB6DE5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFF52000019FF3B006F0364DCFFE745003294969100069AFFB20022' + LineEnding +
    '          41FFFFFFFFFFFFFFE23B4D0C00C1FFFAC6C6C6E4FEE9C6E9FFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFF65CEEFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFEF5EADFD6C50D0000BC7800303C8ED1DCC7DFA8543E7D' + LineEnding +
    '          4D6BD7FFFFBB000025FFFFFFFAF5FFFFF3235B0019E0E7CEC6C6CAFCFFE7C6EA' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEC5EF6FFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFF2C6D0DBDBD1A5764F2B100F893D0072C603006426' + LineEnding +
    '          C7C6C180D2FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFAF74E2FD8D29340065C6D1EC' + LineEnding +
    '          F7E0CBFDFFD4CAF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFE273FBFFFFFFFFFFFFFFFFFFFFFDF3E9DF91B60D261D0500000000000000' + LineEnding +
    '          417084FF6300303169A622008BDFFFFFFFFFFFFFFFFFFFFFFFFFFFFF51002A96' + LineEnding +
    '          15640011CBC6E1FFFFFFD2E5E4C6D4FAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFD47495ADFFFFFFFBF1E5DAC2946C441F165B000000' + LineEnding +
    '          0000000000000000000000C0EC13005814030011B5C6E5F5FFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFF0750000006012008DF1D1C6D7F0F8E2C7C6CBF2FFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC066C4E4D9BD8D6840180000' + LineEnding +
    '          0000000000000000000000000000000000000052CE7C0003650900001FA4C6C8' + LineEnding +
    '          EDE6F4FFFFFDE8ECFFFAD85C00000049300023EDFFF4D0C6C6C6C6C6D0F5FFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFCF2E8DECDA27D5936' + LineEnding +
    '          14000000000000000000000000000000000000000000000507000006A7180000' + LineEnding +
    '          0B5A0C0000044EA2C93390FFFFF93660CE781A0000004E3E02000190FFFFFBED' + LineEnding +
    '          DEDDE1EBF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFBEFE4D9C598' + LineEnding +
    '          714924030000000000000000000000000000000000000000000000000000000D' + LineEnding +
    '          8573596FA08A7CD3B1185C260000000018006CFFFFFF100705000000096C2E66' + LineEnding +
    '          EFA05DEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFD9926C471F0100000000000000000000000000000000000000000000000000' + LineEnding +
    '          00000000000000000009200C00A0FFFFFFD7264054070000000080FFFFFB1F00' + LineEnding +
    '          000000356B1384FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFF9300000000000000000000000000000000000000000000' + LineEnding +
    '          0000000000000000000000000000000000000000000ED8FFFFFFF1691951542A' + LineEnding +
    '          01003C777A701600124B5F3338C9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFF92200000000000000000000000000' + LineEnding +
    '          0000000000000000000000000000000000000000000000000000000000002DF5' + LineEnding +
    '          FFFFFFD69F00002D5B5F6266686968675213004BFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFAA065F000000' + LineEnding +
    '          0000000000000000000000000000000000000000000000000000000000000000' + LineEnding +
    '          0000000000000FA5FBFFFFEFA800000000000000000000000000005CFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFF4955000000000000000000000000000000000000000000000000000000' + LineEnding +
    '          000000000000000000004B6F6D716A1077FFFFFFB3003A6F46260D0000001C3E' + LineEnding +
    '          6E840057FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFF54E00000000000000000000000000000000000000' + LineEnding +
    '          0000000000000000000000000000000000000000000000002FFFFFFFD7ACAAC6' + LineEnding +
    '          C6C6D9FFFFFFFFFFFFFCAAF5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFB6600000000000000000000' + LineEnding +
    '          00000000000000000000000000000000000000000000000000001D670C000000' + LineEnding +
    '          7DFFFFF8C6CDFFFFE1C6ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFF7D00' + LineEnding +
    '          0000000000000000000000000000000000000000000000000000000000000000' + LineEnding +
    '          1B7FD9F6E58F7BB3FFFFFFE1C6D9FFFFD7CAFEFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFA70A0000000000000000000000000000000000000000000000' + LineEnding +
    '          0000000000001C7CDCF8FFFFFFFFFFFFFFFFFCCAC6EEFFFDC9D9FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFC6120000000000000000000000000000' + LineEnding +
    '          0000000000000000000000043689DBF8ECFBFFFFFFFFFFFFFFFFE7C6D3FFFFED' + LineEnding +
    '          C6CFF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD9220013130000' + LineEnding +
    '          000000000000000000000000000000002993B5CBE2F9FFD7C6C9DEF6FFFFFFFF' + LineEnding +
    '          EFD6C6C6E8FFFFD3C6D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFEE424A49000000000000000000000000000000000001ACEBFFF8DEF8DCC6' + LineEnding +
    '          D3C7C6C6D5EAE3CDC6C6CECCC7EDE5C6C7F4FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFF9978D887171715555553E38382D1C1C160000002B' + LineEnding +
    '          3178FECDC6C7C6DCFFF5D8C7C6C6C6C6C9E1FCF7CCC7C6CBF0FFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFE7DFEFFFE9DFC6C6F6FFFFFFFBEEC6D5F1FDFFFFFFE3C6D0FA' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECC6CAE8FDFFFFFFC6DCFF' + LineEnding +
    '          FFFFF7DBC6C7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDDC6' + LineEnding +
    '          C6CBDFEFFAC6DCF6EBD9C6C6C6D4F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101FFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFEDFC6C7C6C6C6C6C6C6C6C7CED6ECFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          C101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4E9FBEDD5C6C6C6E8FEFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFC101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFC6C6' + LineEnding +
    '          ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + LineEnding +
    '          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC101}' + LineEnding +
    '        Material.Texture.Disabled = False' + LineEnding +
    '        Tag = 0' + LineEnding +
    '      end>' + LineEnding +
    '    Left = 74' + LineEnding +
    '    Top = 88' + LineEnding +
    '  end' + LineEnding +
    '  object GLWindowsBitmapFont1: TGLWindowsBitmapFont' + LineEnding +
    '    Font.Charset = DEFAULT_CHARSET' + LineEnding +
    '    Font.Color = clWhite' + LineEnding +
    '    Font.Height = -32' + LineEnding +
    '    Font.Name = ''Verdana''' + LineEnding +
    '    Font.Pitch = fpVariable' + LineEnding +
    '    Font.Style = []' + LineEnding +
    '    Left = 112' + LineEnding +
    '    Top = 88' + LineEnding +
    '  end' + LineEnding +
    'end' + LineEnding;
begin
  Result := Format(FormText, [FFormName]);
end;

constructor TBaseProjectFile.CreateProject(const AProjectName, ModuleName,
  FormName: string);
begin
  FProjectName := AProjectName;
  FModuleName := ModuleName;
  FFormName := FormName;
end;

function TBaseProjectFile.GetSource: string;
const
  ProjText =
    'program %0:s;' + LineEnding +
    '' + LineEnding +
    'uses' + LineEnding +
    '  Forms,' + LineEnding +
    '  %1:s in ''%1:s.pas'';' + LineEnding +
    '' + LineEnding +
    '{$R *.res}' + LineEnding +
    '' + LineEnding +
    'begin' + LineEnding +
    '  ReportMemoryLeaksOnShutdown := True;' + LineEnding +
    '  Application.Initialize;' + LineEnding +
    '  Application.MainFormOnTaskbar := True;' + LineEnding +
{This line inserted automatically by IDE
    '  Application.CreateForm(T%2:s, %2:s);' + LineEnding + }
    '  Application.Run;' + LineEnding +
    'end.' + LineEnding;
begin
  Result := Format(ProjText, [FProjectName, FModuleName, FFormName]);
end;


function TGLBaseSceneFormWizard.GetIDString: string;
begin
  Result := 'GLScene.GLBaseSceneForm';
end;

function TGLBaseSceneFormWizard.GetName: string;
begin
  Result := 'Base GLScene Form';
end;

function TGLBaseSceneFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TGLBaseSceneFormWizard.Execute;
begin
  FClassName := 'GLMainForm';
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(
    'Unit', FUnitIdent, FClassName, FFileName);
  (BorlandIDEServices as IOTAModuleServices).CreateModule(Self);
end;

constructor TGLBaseSceneFormWizard.CreateAndExecute(
  const AUnitIdent, AClassName, AFileName: string);
begin
  inherited Create;
  FUnitIdent := AUnitIdent;
  FClassName := AClassName;
  FFileName := AFileName;
end;

function TGLBaseSceneFormWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TGLBaseSceneFormWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := vCategory;
end;

function TGLBaseSceneFormWizard.GetPersonality: string;
begin
  Result := ToolsAPI.sDelphiPersonality;
end;

function TGLBaseSceneFormWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(hInstance, 'GLSCENEFORMGLYPH');
end;

function TGLBaseSceneFormWizard.GetPage: string;
begin
  Result := 'GLScene VCL';
end;

function TGLBaseSceneFormWizard.GetAuthor: string;
begin
  Result := 'YarUnderoaker';
end;

function TGLBaseSceneFormWizard.GetComment: string;
begin
  Result := 'Creates a new GLScene form.'
end;

function TGLBaseSceneFormWizard.GetCreatorType: string;
begin
  Result := sForm;
end;

function TGLBaseSceneFormWizard.GetExisting: Boolean;
begin
  Result := False;
end;

function TGLBaseSceneFormWizard.GetFileSystem: string;
begin
  Result := '';
end;

function TGLBaseSceneFormWizard.GetOwner: IOTAModule;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to Pred(IModuleServices.ModuleCount) do
  begin
    IModule := IModuleServices.Modules[I];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup.ActiveProject;
      Break;
    end
    else if IModule.QueryInterface(IOTAProject, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup;
      Break;
    end;
  end;
end;

function TGLBaseSceneFormWizard.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TGLBaseSceneFormWizard.GetAncestorName: string;
begin
  Result := 'GLSceneForm';
end;

function TGLBaseSceneFormWizard.GetImplFileName: string;
var
  CurrDir: array[0..MAX_PATH] of Char;
begin
  // Note: full path name required!
  GetCurrentDirectory(SizeOf(CurrDir), CurrDir);
  Result := Format('%s\%s.pas', [CurrDir, FUnitIdent]);
end;

function TGLBaseSceneFormWizard.GetIntfFileName: string;
begin
  Result := '';
end;

function TGLBaseSceneFormWizard.GetFormName: string;
begin
  Result := FClassName;
end;

function TGLBaseSceneFormWizard.GetMainForm: Boolean;
begin
  Result := True;
end;

function TGLBaseSceneFormWizard.GetShowForm: Boolean;
begin
  Result := True;
end;

function TGLBaseSceneFormWizard.GetShowSource: Boolean;
begin
  Result := True;
end;

function TGLBaseSceneFormWizard.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TBaseFormFile.Create('', FormIdent, AncestorIdent);
end;

function TGLBaseSceneFormWizard.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TBaseUnitFile.Create(ModuleIdent, FormIdent, AncestorIdent);
end;

function TGLBaseSceneFormWizard.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TGLBaseSceneFormWizard.FormCreated(const FormEditor: IOTAFormEditor);
var
  OContainer: IOTAComponent;
  NContainer: INTAComponent;
  Component: TComponent;

  procedure RefClean;
  begin
    Component := nil;
    NContainer := nil;
    OContainer := nil;
  end;
begin
  // Form Setup
  RefClean;
  OContainer := FormEditor.GetRootComponent;
  OContainer.QueryInterface(INTAComponent, NContainer);
  Component := NContainer.GetComponent;
  with (Component as TForm) do
  begin
    BorderStyle := bsSizeable;
    Caption := 'GLScene';
    Position := poMainFormCenter;
  end;
  RefClean;
end;

function TGLSimpleSceneFormWizard.GetIDString: string;
begin
  Result := 'GLScene.GLSimpleSceneForm';
end;

function TGLSimpleSceneFormWizard.GetName: string;
begin
  Result := 'Simple GLScene Form';
end;

function TGLSimpleSceneFormWizard.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TSimpleFormFile.Create('', FormIdent, AncestorIdent);
end;

function TGLSimpleSceneFormWizard.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TSimpleUnitFile.Create(ModuleIdent, FormIdent, AncestorIdent);
end;

function TGLExtendedSceneFormWizard.GetIDString: string;
begin
  Result := 'GLScene.GLExtendedSceneForm';
end;

function TGLExtendedSceneFormWizard.GetName: string;
begin
  Result := 'Extended GLScene Form';
end;

function TGLExtendedSceneFormWizard.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TExtendedFormFile.Create('', FormIdent, AncestorIdent);
end;

function TGLExtendedSceneFormWizard.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TExtendedUnitFile.Create(ModuleIdent, FormIdent, AncestorIdent);
end;

// ------------------
// ------------------ TGLBaseSceneProjectWizard ------------------
// ------------------

function TGLBaseSceneProjectWizard.GetIDString: string;
begin
  Result := 'GLScene.GLBaseSceneProject';
end;

function TGLBaseSceneProjectWizard.GetName: string;
begin
  Result := 'Base GLScene Project';
end;

function TGLBaseSceneProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TGLBaseSceneProjectWizard.Execute;
var
  LUnitIdent, LClassName, LFileName: string;
begin
  LUnitIdent := '';
  LClassName := 'GLMainForm';
  LFileName := '';
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(
    'Unit', LUnitIdent, LClassName, LFileName);
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    TGLBaseSceneProjectCreator.Create(LClassName, LUnitIdent, LFileName));
end;

function TGLBaseSceneProjectWizard.GetAuthor: string;
begin
  Result := 'YarUnderoaker';
end;

function TGLBaseSceneProjectWizard.GetComment: string;
begin
  Result := 'Creates a new GLScene project.'
end;

function TGLBaseSceneProjectWizard.GetPage: string;
begin
  Result := 'GLScene VCL';
end;

function TGLBaseSceneProjectWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(hInstance, 'GLSCENEFORMGLYPH');
end;

function TGLBaseSceneProjectWizard.GetDesigner: string;
begin
  Result := ToolsAPI.dVCL;
end;

function TGLBaseSceneProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := vCategory;
end;

function TGLBaseSceneProjectWizard.GetPersonality: string;
begin
  Result := ToolsAPI.sDelphiPersonality;
end;

// ------------------
// ------------------ TGLBaseSceneProjectCreator ------------------
// ------------------

constructor TGLBaseSceneProjectCreator.Create(
  const AClassName, AUnitIdent, AFileName: string);
begin
  FClassName := AClassName;
  FUnitIdent := AUnitIdent;
  FFileName := AFileName;
end;

function TGLBaseSceneProjectCreator.GetCreatorType: string;
begin
  Result := ToolsAPI.sApplication;
end;

function TGLBaseSceneProjectCreator.GetOwner: IOTAModule;
begin
  Result := GetActiveProjectGroup;
end;

function TGLBaseSceneProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TGLBaseSceneProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TGLBaseSceneProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TGLBaseSceneProjectCreator.GetFileName: string;
var
  i: Integer;
  j: Integer;
  ProjGroup: IOTAProjectGroup;
  Found: Boolean;
  TempFileName: String;
  TempFileName2: String;
begin
  Result := GetCurrentDir + '\' + 'Project%d' + '.dpr';
  ProjGroup := GetActiveProjectGroup;
  if ProjGroup <> nil then
  begin
    for J := 0 to ProjGroup.ProjectCount - 1 do
    begin
      Found := False;
      TempFileName2 := Format(Result, [J + 1]);
      for I := 0 to ProjGroup.ProjectCount - 1 do
      begin
        try
          TempFileName := ProjGroup.Projects[I].FileName;
          if AnsiCompareFileName(ExtractFileName(TempFileName),
            ExtractFileName(TempFileName2)) = 0 then
          begin
            Found := True;
            Break;
          end;
        except on E: Exception do if not (E is EIntfCastError) then raise;
        end;
      end;
      if not Found then
      begin
        Result := TempFileName2;
        Exit;
      end;
    end;
    Result := Format(Result, [ProjGroup.ProjectCount + 1]);
  end
  else Result := Format(Result, [1]);
end;

function TGLBaseSceneProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TGLBaseSceneProjectCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

procedure TGLBaseSceneProjectCreator.NewDefaultModule;
begin
end;

procedure TGLBaseSceneProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  with (BorlandIDEServices as IOTAModuleServices) do
    CreateModule(TGLBaseSceneFormWizard.CreateAndExecute(
    FUnitIdent,
    FClassName,
    FFileName));
end;

function TGLBaseSceneProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TGLBaseSceneProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TGLBaseSceneProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TBaseProjectFile.CreateProject(ProjectName, FUnitIdent, FClassName) as IOTAFile;
end;

// ------------------
// ------------------ TGLSimpleSceneProjectWizard ------------------
// ------------------

function TGLSimpleSceneProjectWizard.GetIDString: string;
begin
  Result := 'GLScene.GLSimpleSceneProject';
end;

function TGLSimpleSceneProjectWizard.GetName: string;
begin
  Result := 'Simple GLScene Project';
end;

procedure TGLSimpleSceneProjectWizard.Execute;
var
  LUnitIdent, LClassName, LFileName: string;
begin
  LUnitIdent := '';
  LClassName := 'GLMainForm';
  LFileName := '';
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(
    'Unit', LUnitIdent, LClassName, LFileName);
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    TGLSimpleSceneProjectCreator.Create(LClassName, LUnitIdent, LFileName));
end;

// ------------------
// ------------------ TGLSimpleSceneProjectCreator ------------------
// ------------------

procedure TGLSimpleSceneProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  with (BorlandIDEServices as IOTAModuleServices) do
    CreateModule(TGLSimpleSceneFormWizard.CreateAndExecute(
    FUnitIdent,
    FClassName,
    FFileName));
end;

// ------------------
// ------------------ TGLExtendedSceneProjectWizard ------------------
// ------------------

function TGLExtendedSceneProjectWizard.GetIDString: string;
begin
  Result := 'GLScene.GLExtendedSceneProject';
end;

function TGLExtendedSceneProjectWizard.GetName: string;
begin
  Result := 'Extended GLScene Project';
end;

procedure TGLExtendedSceneProjectWizard.Execute;
var
  LUnitIdent, LClassName, LFileName: string;
begin
  LUnitIdent := '';
  LClassName := 'GLMainForm';
  LFileName := '';
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(
    'Unit', LUnitIdent, LClassName, LFileName);
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    TGLExtendedSceneProjectCreator.Create(LClassName, LUnitIdent, LFileName));
end;

// ------------------
// ------------------ TGLExtendedSceneProjectCreator ------------------
// ------------------

procedure TGLExtendedSceneProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  with (BorlandIDEServices as IOTAModuleServices) do
    CreateModule(TGLExtendedSceneFormWizard.CreateAndExecute(
    FUnitIdent,
    FClassName,
    FFileName));
end;

initialization
  InitModule;
finalization
  DoneModule;

end.

