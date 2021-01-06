//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.Strings;

(*
  String constants that are used for internationalization

  To build your project with a single language's translations in the executable
  itself, define one of the next languages

Language             $DEFINE    2-character locale id
  -------------------  ---------- ---------------------
  English              LANG_EN    en
  French               LANG_FR    fr
  German               LANG_DE    de
  Italian              LANG_IT    it
  Japanese             LANG_JP    jp
  Russian              LANG_RU    ru
  Spanish              LANG_ES    es
*)

interface

{$DEFINE LANG_EN}

var
  Language: Integer;

resourcestring

  // General
  strDot = '.';
  strError   = 'Error!';
  strErrorEx = 'Error: ';
  strErrorLIB = 'ErrorLIB';
  strInvalidType = 'Invalid data type';
  strNothing = 'Nothing';
  strStateCashMissing = 'States cash missing: ';
  strUnknownArchiveVersion = 'Unknown archive version: ';
  strObjectsNotSupported = 'Linked object not supported';


  // SceneEdit
  strGLSceneEditor = 'GLScene Editor';

  // SceneViewer
  strNoRenderingContext = 'Could not create a rendering context';
  strWrongVersion       = 'Need at least OpenGL version 1.1';
  strTooManyLights      = 'Too many lights in the scene';
  strDisplayList        = 'Failed to create a new display list for object ''%s''';
  strWrongBitmapCanvas  = 'Couldn''t create a rendering context for the given bitmap';
  strWrongPrinter       = 'Couldn''t render to printer';
  strAlreadyRendering   = 'Already rendering';
  strSceneViewerNotDefined = 'SceneViewer not defined!';

  // Cadencer
  strCadencerNotDefined   = 'Cadencer not defined!';
  strCadencerNotDefinedEx = 'Cadencer not defined for  the ''%s'' component';

  // Context
  strCannotAlterAnActiveContext = 'Cannot alter an active context';
  strContextActivationFailed = 'Context activation failed: %X, %s';
  strContextAlreadyCreated = 'Context already created';
  strContextDeactivationFailed = 'Context deactivation failed';
  strContextNotCreated = 'Context not created';
  strDeleteContextFailed = 'Delete context failed';
  strFailedToShare = 'DoCreateContext - Failed to share contexts';
  strIncompatibleContexts = 'Incompatible contexts';
  strInvalidContextRegistration = 'Invalid context registration';
  strInvalidNotificationRemoval = 'Invalid notification removal';
  strNoActiveRC = 'No active rendering context';
  strUnbalancedContexActivations = 'Unbalanced context activations';
  strUnableToCreateLegacyContext = 'Unable to create legacy context';

  // InitOpenGL
  strOpenGLError = 'OpenGL error - %s';

  // File3DS
  str3DSMapNotFound = 'Loading %s map texture failed: %s in %s';
  strError3DS_NO_MEM = 'Not enough memory to complete operation.';
  strError3DS_INVALID_ARG = 'The argument passed to the function is invalid.'#13+
                         'Usually caused by a nil pointer or an out of range numeric argument.';
  strError3DS_INVALID_DATA = 'The structure passed as an argument to the function has'#13+
                          'invalid or out of range data in its fields.';
  strError3DS_INVALID_CHUNK = 'An invalid Chunk structure was encountered while reading'#13+
                           'the database. Usually caused by a corrupt database or file.';
  strError3DS_INVALID_DATABASE = 'The database passed as an argument has not be created yet.';
  strError3DS_WRONG_DATABASE = 'The database passed as an argument is the wrong kind of'#13+
                            'database for this function.';
  strError3DS_UNFOUND_CHUNK = 'The database is missing important file chunks needed to'#13+
                           'fill out the requested structure. Usually caused by a corrupt database or file.';
  strError3DS_WRONG_OBJECT = 'The Name passed to the functions exists but is not the type of object asked for.'#13+
                          'For example asking for a mesh object with the GetCameraByName3DS function.';
  strError3DS_NO_SUCH_FILE = 'The FileName passed as an argument for reading does not exist.';
  strError3DS_INIT_FAILED = 'Failed to initialize structure passed as an argument.';
  strError3DS_OPENING_FILE = 'Could not open requested file.';
  strError3DS_CLOSING_FILE = 'Could not close requested file.';
  strError3DS_READING_FILE = 'Error occured while reading file.';
  strError3DS_CREATING_DATABASE = 'Error occured while creating database.';
  strError3DS_READING_DATABASE = 'Error occured while reading database.';
  strError3DS_WRITING_DATABASE = 'Error occured while writing database.';
  strError3DS_WRITING_FILE = 'Error occured while writing file.';
  strError3DS_STRING_TOO_LONG = 'String encountered in file structure or as an argument was longer than expected.'#13+
                             'Possibly caused by an uninitialed pointer corrupt file or database.';
  strError3DS_GET_FAIL = 'Failed to get new data from database.';
  strError3DS_PUT_FAIL = 'Failed to add new data to the database.';
  strError3DS_INVALID_INDEX = 'Invalid index %d.';

  // FilePGM
  strCUTILFailed = 'Can not initialize cutil32.dll';

  // Graphics
  strCantConvertImg = '%s: can''t convert image to RGBA8 format';

  // Joystick
  strNoJoystickDriver   = 'There''s no joystick driver present';
  strConnectJoystick    = 'Joystick is not connected to your system';
  strJoystickError      = 'Your system reports a joystick error, can''t do anything about it';

  // Material
  strCyclicRefMat = 'Cyclic reference detected in material "%s"';

  // Octree
  strOctreeMustBePreparedBeforeUse = 'Octree must be prepared before use';

  // PersistentClasses
  strInvalidFileSignature = 'Invalid file signature';
  strBrokenObjectListArchive = 'Broken ObjectList archive';
  strListIndexError = 'Invalid list index';


  // SceneContext
  strForwardContextFailed = 'Can not create forward compatible context: #%X, %s';
  strBackwardContextFailed = 'Can not create backward compatible context: #%X, %s';
  strFailHWRC = 'Unable to create rendering context with hardware acceleration - down to software';
  strTmpRC_Created = 'Temporary rendering context created';
  strDriverNotSupportFRC = 'Driver not support creating of forward context';
  strDriverNotSupportOESRC = 'Driver not support creating of OpenGL ES 2.0 context';
  strDriverNotSupportDebugRC = 'Driver not support creating of debug context';
  strOESvsForwardRC = 'OpenGL ES 2.0 context incompatible with Forward context - flag ignored';
  strFRC_created = 'Forward core context seccussfuly created';
  strOESRC_created = 'OpenGL ES 2.0 context seccussfuly created';
  strPBufferRC_created = 'Backward compatible core PBuffer context successfully created';

  // SceneRegister, strings additional to Designintf
  strOpenGLCategoryName = 'OpenGL';
  strLayoutCategoryName = 'Layout';
  strLocalizableCategoryName = 'Localizable';
  strVisualCategoryName = 'Visual';

  // Shaders
  strShaderNeedsAtLeastOneLightSource   = 'This shader needs at least one LightSource!';
  strShaderNeedsAtLeastOneLightSourceEx = 'Shader ''%s'' needs at least one LightSource!';

  // Tree
  strSceneRoot  = 'Scene root';
  strObjectRoot = 'Scene objects';
  strCameraRoot = 'Cameras';
  strCamera     = 'Camera';

  // Textures
  strImageInvalid = 'Could not load texture, image is invalid';
  strNoNewTexture = 'Could not get new texture name';

  // Materials
  strMatLibNotDefined = 'Material Library not defined!';
  strMaterialNotFoundInMatlib = 'Material not found in current Material Library!';
  strMaterialNotFoundInMatlibEx = 'Material "%s" not found in current Material Library!';

  // Objects
  strSphereTopBottom = 'The top angle must be higher than the bottom angle';
  strSphereStartStop = 'The start angle must be smaller than then stop angle';
  strMaterialNotFound = 'Loading failed: could not find material %s';
  strInterleaveNotSupported = 'Interleaved Array format not supported yet. Sorry.';

  //Common messages
  strUnknownArchive = '%s : unknown archive version %d';
  strOutOfMemory = 'Fatal: Out of memory';
  strFileNotFound = 'File %s not found';
  strFailedOpenFile = 'Could not open file: %s';
  strFailedOpenFileFromCurrentDir = 'Could not open file: %s'#13#10'(Current directory is %s)';
  strNoDescriptionAvailable = 'No description available';
  strUnBalancedBeginEndUpdate = 'Unbalanced Begin/EndUpdate';
  strUnknownExtension = 'Unknown file extension (%s), maybe you forgot to add the support '
                       +'unit to your uses? (%s?)' ;
  strMissingResource = 'Missing application resource: %s: %s';
  strIncompatibleTypes = 'Incompatible types!';
  strUnknownType       = 'Unknown type!';
  strUnsupportedType   = 'Unsupported type!';

  // Object categories in GLScene Registry
  strOCBasicGeometry = 'Basic geometry';
  strOCAdvancedGeometry = 'Advanced geometry';
  strOCMeshObjects = 'Mesh objects';
  strOCParticleSystems = 'Particle systems';
  strOCEnvironmentObjects = 'Environment objects';
  strOCSpecialObjects = 'Special objects';
  strOCGraphPlottingObjects = 'Graph-plotting objects';
  strOCDoodad = 'Doodad objects';
  strOCHUDObjects = 'HUD objects';
  strOCGuiObjects = 'GUI objects';
  strOCProxyObjects = 'Proxy objects';
  strOCExperimental = 'Experimental objects';

  // Utils
  strInvalidColor = '''%s'' is not a valid color format!';
  strUnknownParam = 'Unknown %s "%s" for "%s" or program not in use';

  // FXCollectionEditor
  strXCollectionEditor = 'XCollection editor';

  // CUDA strings
  strModuleAbsent = 'Module is absent';
  strInvalidParamType = 'Invalid parameter type';
  strInvalidContextReg = 'Invalid context registration.';
  strInvalidValue = 'Invalid value';
  strWrongParamSetup = 'Function''s parameters must be setup in OnParameterSetup event';
  strLaunchFailed = 'Kernel function "%s" launch failed';
  strFailMap = 'Unable to map %s - already mapped';
  strFailUnmap = 'Unable to unmap %s - not mapped';
  strFailCompilation = 'NVCC failed to compile:' + #10#13 + '%s';
  strFailCreatePipe = 'Unable to create Pipe';
  strFailRunNVCC = 'Unable to run process (NVCC)';
  strFuncRetErr = '%s return error: %s';
  strFuncNotConnected = '%s.Launch: Kernel function not connected';
  strOnlyHostData = 'Only host data or mapped device or array data can be written/read';
  strOutOfRange = 'Indexes out of range';
  strSizeMismatch = 'Element size mismatch';
  strCUDAEditor = 'Scene CUDA Component Editor';
  strRequireFreeThread = 'CUFFT functions require context-free thread';
  strBadPlanSize = 'MemData size less then Plan size';
  strContextNotInit = 'Context not initialized';
  strNoDeviceToCreate = 'No device to create CUDA context';
  strThreadBusy = 'Unable to create CUDA context - thread is busy by another context';
  strMakeFloatingFail = 'Unable to make context floating after creation';
  strUnbalansedUsage = 'Unbalansed CUDA context usage';
  strInvalidGLContext = 'Unable to create CUDA context with OpenGL interop' +
    ' - OpenGL context not ready';
  strFFTFuncRetErr = '%s return error: %s';
  strFailToBindArrayToTex = 'Unable to bind CUDA array to OpenGL unmaped t' + 'exture';
  strOutOfAttribSize = 'The amount of device''s data less then size of att' + 'ribute''s data.';
  strOutOfElementSize = 'The amount of device''s data less then size of in' + 'dexes data.';
  strSourceFileNotFound = 'Source file not found';
  strSuccessCompilation = 'Successful compilation:' + #10#13 + '%s';


//---------------------------------------------------------
implementation
//---------------------------------------------------------


end.

