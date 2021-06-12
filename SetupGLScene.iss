;-----------------------------------------------------------------------------
; Setup GLScene script for Inno Setup Compiler
;-----------------------------------------------------------------------------

#define GLSceneName "GLScene"
#define GLSceneVersion "v.1.9"
#define GLScenePublisher "GLSteam"
#define GLSceneURL "http://www.glscene.org/"

[Setup]
AppId={{8CF5F54E-C1FC-4716-BC82-908867D36AD6}
AppName={#GLSceneName}
AppVersion={#GLSceneVersion}
AppVerName=GLScene for Windows
AppCopyright=Copyright © 1997,2020 GLSteam
AppPublisher={#GLScenePublisher}
AppPublisherURL={#GLSceneURL}
AppSupportURL={#GLSceneURL}
AppUpdatesURL={#GLSceneURL}
;DefaultDirName={pf}\{#GLSceneName} ... and then {app}
DefaultDirName=D:\Components\{#GLSceneName}
DefaultGroupName={#GLSceneName}
DisableProgramGroupPage=yes
OutputBaseFilename=SetupGLScene_{#GLSceneVersion}

; Source directory of files
; SourceDir=D:\GLScene
; Output directory for setup program
OutputDir=D:\GLS\Installation   

InfoBeforeFile=Help\en\Introduction.txt
InfoAfterFile=Samples\Samples.txt

Compression=lzma
SetupIconFile=Samples\media\gls.ico
SolidCompression=yes

;welcome image
WizardImageFile=Samples\media\GLSlogo.bmp  
WizardImageBackColor= clMaroon 
WizardImageStretch=yes
WizardSmallImageFile=Samples\media\GLS.bmp
WizardSmallImageBackColor=clNavy  

;background
WindowVisible=yes 
BackColor=clPurple
BackColor2=clMaroon
;BackColorDirection= lefttoright

;full screen installer
WindowShowCaption=no 
WindowStartMaximized=yes 

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"; LicenseFile: "Help\en\License.txt"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"; LicenseFile: "Help\ru\License.txt"

[Types]
Name: "Full"; Description: "All comps"
Name: "Custom"; Description: "Choose comps"; Flags: iscustom

[Components]
;Name: "Samples"; Description: "Samples"; Types: Full Custom 
;Name: "Utilities"; Description: "Utilities"; Types: Full Custom 

[Code]
function InitializeSetup: Boolean;
begin
  Result := IsAdminLoggedOn;
  if (not Result) then
    MsgBox(SetupMessage(msgAdminPrivilegesRequired), mbCriticalError, MB_OK);
end;

function IsPackageDir: Boolean;
begin
//if DirExist()
//  then
//  begin
//  end;
end;

[Files]
Source: "CleanForRelease.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "SetupGLScene.iss"; DestDir: "{app}"; Flags: ignoreversion
Source: "Readme.txt"; DestDir: "{app}"; Flags: ignoreversion

Source: "bpl\*"; DestDir: "{app}\bpl"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "external\*"; DestDir: "{app}\external"; Flags: ignoreversion
Source: "Help\*"; DestDir: "{app}\Help"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "include\*"; DestDir: "{app}\include"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "lib\*"; DestDir: "{app}\lib"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Packages\*"; DestDir: "{app}\Packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Resources\*"; DestDir: "{app}\Resources"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Samples\*"; DestDir: "{app}\Samples"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Source\*"; DestDir: "{app}\Source"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Utilities\*"; DestDir: "{app}\Utilities"; Flags: ignoreversion recursesubdirs createallsubdirs

[Code]
function IsDadRegistryExist: Boolean;
begin
  if RegKeyExists(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\17.0') or
     RegKeyExists(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\18.0') or
     RegKeyExists(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\19.0')
  then
  begin
    /// "Yes". Update 
  end  
  else
    begin
      if MsgBox('Do you have in registry Software\Embarcadero\BDS\19.0?', mbError, MB_YESNO) = idYes
      then 
        /// Full installation
      else 
        /// Exit
    end;
end;

[Registry]
; Write parameters for the project
Root: HKCU; Subkey: "Software\GLScene"; ValueType: string; ValueName: "Version"; ValueData: {#GLSceneVersion}; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\GLScene"; ValueType: string; ValueName: InslallSettings; ValueData: "{src}\SetupGLScene.exe"; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\GLScene"; ValueType: string; ValueName: LibraryDir; ValueData: "{app}"; Flags: createvalueifdoesntexist uninsdeletekey 

; Write parameters for RAD Studio   
; Auto Save of Desktop and Editor Files
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Auto Save"; ValueType: string; ValueName: Desktop; ValueData: "True"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Auto Save"; ValueType: string; ValueName: Editor Files; ValueData: "True"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Auto Save"; ValueType: string; ValueName: Desktop; ValueData: "True"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Auto Save"; ValueType: string; ValueName: Editor Files; ValueData: "True"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Auto Save"; ValueType: string; ValueName: Desktop; ValueData: "True"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Auto Save"; ValueType: string; ValueName: Editor Files; ValueData: "True"; 
                     
; Environmental Variables, the ValueData needs to be changed from SourceDir to {app}   
; New user variable 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Environment Variables"; ValueType: string; ValueName: GLSCENE; ValueData: "{app}"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Environment Variables"; ValueType: string; ValueName: GLSCENE; ValueData: "{app}"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Environment Variables"; ValueType: string; ValueName: GLSCENE; ValueData: "{app}"; Flags: deletevalue uninsdeletevalue

; Delphi Options
; Win32 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: Browsing Path; ValueData: "{olddata};$(GLSCENE)\Source;$(GLSCENE)\Source\Basis;$(GLSCENE)\Source\DesignTime;$(GLSCENE)\Source\FileFormats;$(GLSCENE)\Source\GameAPIs;$(GLSCENE)\Source\ParallelAPIs;$(GLSCENE)\Source\PhysicsAPIs;$(GLSCENE)\Source\ScriptingAPIs;$(GLSCENE)\Source\Shaders;$(GLSCENE)\Source\SoundVideoAPIs";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: Package Search Path; ValueData: "{olddata};$(GLSCENE)\bpl\Win32";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(GLSCENE)\lib\Win32"; 

Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Library\Win32"; ValueType: string; ValueName: Browsing Path; ValueData: "{olddata};$(GLSCENE)\Source;$(GLSCENE)\Source\Basis;$(GLSCENE)\Source\DesignTime;$(GLSCENE)\Source\FileFormats;$(GLSCENE)\Source\GameAPIs;$(GLSCENE)\Source\ParallelAPIs;$(GLSCENE)\Source\PhysicsAPIs;$(GLSCENE)\Source\ScriptingAPIs;$(GLSCENE)\Source\Shaders;$(GLSCENE)\Source\SoundVideoAPIs";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Library\Win32"; ValueType: string; ValueName: Package Search Path; ValueData: "{olddata};$(GLSCENE)\bpl\Win32";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Library\Win32"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(GLSCENE)\lib\Win32"; 

Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Library\Win32"; ValueType: string; ValueName: Browsing Path; ValueData: "{olddata};$(GLSCENE)\Source;$(GLSCENE)\Source\Basis;$(GLSCENE)\Source\DesignTime;$(GLSCENE)\Source\FileFormats;$(GLSCENE)\Source\GameAPIs;$(GLSCENE)\Source\ParallelAPIs;$(GLSCENE)\Source\PhysicsAPIs;$(GLSCENE)\Source\ScriptingAPIs;$(GLSCENE)\Source\Shaders;$(GLSCENE)\Source\SoundVideoAPIs";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Library\Win32"; ValueType: string; ValueName: Package Search Path; ValueData: "{olddata};$(GLSCENE)\bpl\Win32";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Library\Win32"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(GLSCENE)\lib\Win32"; 
; Win64
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win64"; ValueType: string; ValueName: Browsing Path; ValueData: "{olddata};$(GLSCENE)\Source;$(GLSCENE)\Source\Basis;$(GLSCENE)\Source\DesignTime;$(GLSCENE)\Source\FileFormats;$(GLSCENE)\Source\GameAPIs;$(GLSCENE)\Source\ParallelAPIs;$(GLSCENE)\Source\PhysicsAPIs;$(GLSCENE)\Source\ScriptingAPIs;$(GLSCENE)\Source\Shaders;$(GLSCENE)\Source\SoundVideoAPIs";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win64"; ValueType: string; ValueName: Package Search Path; ValueData: "{olddata};$(GLSCENE)\bpl\Win64";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win64"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(GLSCENE)\lib\Win64";  

Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Library\Win64"; ValueType: string; ValueName: Browsing Path; ValueData: "{olddata};$(GLSCENE)\Source;$(GLSCENE)\Source\Basis;$(GLSCENE)\Source\DesignTime;$(GLSCENE)\Source\FileFormats;$(GLSCENE)\Source\GameAPIs;$(GLSCENE)\Source\ParallelAPIs;$(GLSCENE)\Source\PhysicsAPIs;$(GLSCENE)\Source\ScriptingAPIs;$(GLSCENE)\Source\Shaders;$(GLSCENE)\Source\SoundVideoAPIs";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Library\Win64"; ValueType: string; ValueName: Package Search Path; ValueData: "{olddata};$(GLSCENE)\bpl\Win64";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Library\Win64"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(GLSCENE)\lib\Win64";  

Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Library\Win64"; ValueType: string; ValueName: Browsing Path; ValueData: "{olddata};$(GLSCENE)\Source;$(GLSCENE)\Source\Basis;$(GLSCENE)\Source\DesignTime;$(GLSCENE)\Source\FileFormats;$(GLSCENE)\Source\GameAPIs;$(GLSCENE)\Source\ParallelAPIs;$(GLSCENE)\Source\PhysicsAPIs;$(GLSCENE)\Source\ScriptingAPIs;$(GLSCENE)\Source\Shaders;$(GLSCENE)\Source\SoundVideoAPIs";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Library\Win64"; ValueType: string; ValueName: Package Search Path; ValueData: "{olddata};$(GLSCENE)\bpl\Win64";
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Library\Win64"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(GLSCENE)\lib\Win64";  

; C++Builder Options
; Win32
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: IncludePath; ValueData: "{olddata};$(GLSCENE)\include\Win32"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(GLSCENE)\lib\Win32"; 

Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\C++\Paths\Win32"; ValueType: string; ValueName: IncludePath; ValueData: "{olddata};$(GLSCENE)\include\Win32"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\C++\Paths\Win32"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(GLSCENE)\lib\Win32"; 

Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\C++\Paths\Win32"; ValueType: string; ValueName: IncludePath; ValueData: "{olddata};$(GLSCENE)\include\Win32"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\C++\Paths\Win32"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(GLSCENE)\lib\Win32"; 
; Win64
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win64"; ValueType: string; ValueName: UserIncludePath; ValueData: "$(GLSCENE)\include\Win64"; Flags: deletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win64"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(GLSCENE)\lib\Win64";

Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\C++\Paths\Win64"; ValueType: string; ValueName: UserIncludePath; ValueData: "$(GLSCENE)\include\Win64"; Flags: deletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\C++\Paths\Win64"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(GLSCENE)\lib\Win64";

Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\C++\Paths\Win64"; ValueType: string; ValueName: UserIncludePath; ValueData: "$(GLSCENE)\include\Win64"; Flags: deletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\C++\Paths\Win64"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(GLSCENE)\lib\Win64";

; Known Packages
; 17.0 Seattle
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Cg_DT.bpl; ValueData: "GLScene Cg Shaders"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Parallel_DT.bpl; ValueData: "GLScene GPU Computing"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_DT.bpl; ValueData: "GLScene OpenGL 3D library"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Physics_DT.bpl; ValueData: "GLScene Physics Managers"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Sounds_DT.bpl; ValueData: "GLScene Sound Managers"; Flags: deletevalue uninsdeletevalue

; 18.0 Berlin
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Cg_DT.bpl; ValueData: "GLScene Cg Shaders"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Parallel_DT.bpl; ValueData: "GLScene GPU Computing"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_DT.bpl; ValueData: "GLScene OpenGL 3D library"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Physics_DT.bpl; ValueData: "GLScene Physics Managers"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Sounds_DT.bpl; ValueData: "GLScene Sound Managers"; Flags: deletevalue uninsdeletevalue

; 19.0 Tokyo
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Cg_DT.bpl; ValueData: "GLScene Cg Shaders"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Parallel_DT.bpl; ValueData: "GLScene GPU Computing"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_DT.bpl; ValueData: "GLScene OpenGL 3D library"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Physics_DT.bpl; ValueData: "GLScene Physics Managers"; Flags: deletevalue uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\19.0\Known Packages"; ValueType: string; ValueName: $(GLSCENE)\bpl\Win32\GLScene_Sounds_DT.bpl; ValueData: "GLScene Sound Managers"; Flags: deletevalue uninsdeletevalue

[Code]

function IsRegularUser(): Boolean;
begin
  Result := not (IsAdminLoggedOn or IsPowerUserLoggedOn)
end;
 
function GetDefRoot(Param: String): String;
begin
  if IsRegularUser then
    Result := ExpandConstant('{localappdata}')
  else
    Result := ExpandConstant('{pf}')
end;

[Run]
; Installation of DLLs in System32 and SysWOW64 directories 
Filename: "{app}\external\SetupDLLs.bat"
