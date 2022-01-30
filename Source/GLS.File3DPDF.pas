//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.File3DPDF;

(* 3D PDF converter of GLScene's models *)

interface

uses
  WinApi.Windows,
  WinApi.ShellAPI,
  System.Classes,
  System.SysUtils,
  System.StrUtils,

  GLS.VectorTypes,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorFileObjects,
  GLS.ApplicationFileIO,
  GLS.Utils;

type
  (* The IDTF vector file (Intermediate Data Text File).
     Used for converting to IDTF -> U3D -> 3D PDF *)
  TGLIDTFVectorFile = class(TGLVectorFile)
  private
    procedure BuildNormals(m: TGLMeshObject);
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure SaveToStream(aStream: TStream); override;
  end;

  // The U3D vector file (using IDTF and U3DConverter).
  TGLU3DVectorFile = class(TGLIDTFVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure SaveToStream(aStream: TStream); override;
  end;

var
  // global variable for accing the IDTF->U3D converter
  IDTFConverterFileName: string;

//=========================================================
implementation
//=========================================================

const
  ConstIDTFTemplate =
    '    FILE_FORMAT "IDTF"                                                     ' + #13#10 +
    '    FORMAT_VERSION 100                                                     ' + #13#10 +
    '                                                                           ' + #13#10 +
    '    NODE "MODEL" {                                                         ' + #13#10 +
    '         NODE_NAME "VcgMesh01"                                             ' + #13#10 +
    '         PARENT_LIST {                                                     ' + #13#10 +
    '              PARENT_COUNT 1                                               ' + #13#10 +
    '              PARENT 0 {                                                   ' + #13#10 +
    '                   PARENT_NAME "<NULL>"                                    ' + #13#10 +
    '                   PARENT_TM {                                             ' + #13#10 +
    '                        1.000000 0.000000 0.000000 0.000000                ' + #13#10 +
    '                        0.000000 1.000000 0.000000 0.000000                ' + #13#10 +
    '                        0.000000 0.000000 1.000000 0.000000                ' + #13#10 +
    '                        0.000000 0.000000 0.000000 1.000000                ' + #13#10 +
    '                   }                                                       ' + #13#10 +
    '              }                                                            ' + #13#10 +
    '         }                                                                 ' + #13#10 +
    '         RESOURCE_NAME "MyVcgMesh01"                                       ' + #13#10 +
    '    }                                                                      ' + #13#10 +
    '                                                                           ' + #13#10 +
    '    RESOURCE_LIST "MODEL" {                                                ' + #13#10 +
    '         RESOURCE_COUNT 1                                                  ' + #13#10 +
    '         RESOURCE 0 {                                                      ' + #13#10 +
    '              RESOURCE_NAME "MyVcgMesh01"                                  ' + #13#10 +
    '              MODEL_TYPE "MESH"                                            ' + #13#10 +
    '              MESH {                                                       ' + #13#10 +
    '                   FACE_COUNT %d                                           ' + #13#10 +
    '                   MODEL_POSITION_COUNT %d                                 ' + #13#10 +
    '                   MODEL_NORMAL_COUNT %d                                   ' + #13#10 +
    '                   MODEL_DIFFUSE_COLOR_COUNT 0                             ' + #13#10 +
    '                   MODEL_SPECULAR_COLOR_COUNT 0                            ' + #13#10 +
    '                   MODEL_TEXTURE_COORD_COUNT 0                             ' + #13#10 +
    '                   MODEL_BONE_COUNT 0                                      ' + #13#10 +
    '                   MODEL_SHADING_COUNT 1                                   ' + #13#10 +
    '                   MODEL_SHADING_DESCRIPTION_LIST {                        ' + #13#10 +
    '                       SHADING_DESCRIPTION 0 {                             ' + #13#10 +
    '                             TEXTURE_LAYER_COUNT 0                         ' + #13#10 +
    '                             SHADER_ID 0                                   ' + #13#10 +
    '                        }                                                  ' + #13#10 +
    '                   }                                                       ' + #13#10 +
    '                   MESH_FACE_POSITION_LIST {                               ' + #13#10 +
    '                        %s                                                 ' + #13#10 +
    '                   }                                                       ' + #13#10 +
    '                   MESH_FACE_NORMAL_LIST {                                 ' + #13#10 +
    '                        %s                                                 ' + #13#10 +
    '                   }                                                       ' + #13#10 +
    '                   MESH_FACE_SHADING_LIST {                                ' + #13#10 +
    '                        %s                                                 ' + #13#10 +
    '                   }                                                       ' + #13#10 +
    '                   MODEL_POSITION_LIST {                                   ' + #13#10 +
    '                        %s                                                 ' + #13#10 +
    '                   }                                                       ' + #13#10 +
    '                   MODEL_NORMAL_LIST {                                     ' + #13#10 +
    '                        %s                                                 ' + #13#10 +
    '                   }                                                       ' + #13#10 +
    '              }                                                            ' + #13#10 +
    '         }                                                                 ' + #13#10 +
    '    }                                                                      ';


var
  USFormat: TFormatSettings;

// helper functions

function SingleToStr(const AValue: Single): string; inline;
begin
  if AValue = 0.0 then
    begin
      Result:= '0';
      Exit;
    end
  else
  // Limit to maxint
  if AValue > (MaxInt - 1) then
    Result:= '2147483647'
  else
  if AValue < -(MaxInt - 1) then
    Result:= '2147483647';

  Result:= FloatToStrF(AValue, ffFixed, 8, 6, USFormat);
end;

function GetTempPath: string;
var
  Len: Integer;
begin
  SetLastError(ERROR_SUCCESS);

  // get memory for the buffer retaining the temp path (plus null-termination)
  SetLength(Result, MAX_PATH);
  Len := Winapi.Windows.GetTempPath(MAX_PATH, PChar(Result));
  if Len <> 0 then
  begin
    Len := GetLongPathName(PChar(Result), nil, 0);
    GetLongPathName(PChar(Result), PChar(Result), Len);
    SetLength(Result, Len - 1);
  end
  else
    Result := '';
end;

procedure ExecProgramAndWait(const AProcessName, AParams: string);
var
   startUpInfo : TStartupInfo;
   ProcessInfo  : TProcessInformation;
   exeCmd : string;
   ExitCode: cardinal;
begin
   // Concat in the parameters
   exeCmd := AProcessName + ' ' + AParams;
   // Initialise the StartUpInfo record, which handles the creation of
   // a new main window for a process
   FillChar(startUpInfo, SizeOf(startUpInfo), 0);
   StartUpInfo.cb := SizeOf( StartUpInfo );
   StartUpInfo.dwFlags     := STARTF_USESHOWWINDOW;
   StartUpInfo.wShowWindow := SW_HIDE;

   // Spawn the process out.
   if CreateProcess(nil, PChar(exeCmd), nil, nil, false,
            CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil,
            PChar(ExtractFilePath(AProcessName)), startUpInfo, ProcessInfo) then
     begin
       // close the thread handle as soon as it is no longer needed
       CloseHandle(ProcessInfo.hThread);
       // Wait for the process to finish.
       if not WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0 then
         raise Exception.CreateFmt('U3DConverter: %s', [SysErrorMessage(GetLastError)]);
       // finish process
       if GetExitCodeProcess(ProcessInfo.hProcess, ExitCode) then
         raise Exception.CreateFmt('U3DConverter failed with exitcode $%x', [ExitCode]);
       CloseHandle(ProcessInfo.hProcess);
     end
   else
     begin
       // create process failure
       raise Exception.CreateFmt('U3DConverter: %s', [SysErrorMessage(GetLastError)]);
     end;
end;


{ TGLIDTFVectorFile }

class function TGLIDTFVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcWrite];
end;

    // build normals
procedure TGLIDTFVectorFile.BuildNormals(m: TGLMeshObject);
var
  i, j: Integer;
  v1, v2, v3, v4, n: TAffineVector;
begin
  for i := 0 to m.vertices.count - 1 do
    m.Normals.Add(0, 0, 0);
  for i := 0 to m.FaceGroups.count - 1 do
    if m.FaceGroups[i] is TFGVertexIndexList then
      with m.FaceGroups[i] as TFGVertexIndexList do
        case mode of
          fgmmTriangles:
            begin
              for j := 0 to (VertexIndices.count div 3) - 1 do
              begin
                v1 := m.vertices[VertexIndices[j * 3]];
                v2 := m.vertices[VertexIndices[j * 3 + 1]];
                v3 := m.vertices[VertexIndices[j * 3 + 2]];
                n := CalcPlaneNormal(v1, v2, v3);
                m.Normals.items[VertexIndices[j * 3]] :=
                  VectorAdd(m.Normals.items[VertexIndices[j * 3]], n);
                m.Normals.items[VertexIndices[j * 3 + 1]] :=
                  VectorAdd(m.Normals.items[VertexIndices[j * 3 + 1]], n);
                m.Normals.items[VertexIndices[j * 3 + 2]] :=
                  VectorAdd(m.Normals.items[VertexIndices[j * 3 + 2]], n);
              end;
            end;
          fgmmQuads:
            begin
              for j := 0 to (VertexIndices.count div 4) - 1 do
              begin
                v1 := m.vertices[VertexIndices[j * 4]];
                v2 := m.vertices[VertexIndices[j * 4 + 1]];
                v3 := m.vertices[VertexIndices[j * 4 + 2]];
                v4 := m.vertices[VertexIndices[j * 4 + 3]];
                n := CalcPlaneNormal(v1, v2, v3);
                m.Normals.items[VertexIndices[j * 4]] :=
                  VectorAdd(m.Normals.items[VertexIndices[j * 4]], n);
                m.Normals.items[VertexIndices[j * 4 + 1]] :=
                  VectorAdd(m.Normals.items[VertexIndices[j * 4 + 1]], n);
                m.Normals.items[VertexIndices[j * 4 + 2]] :=
                  VectorAdd(m.Normals.items[VertexIndices[j * 4 + 2]], n);
                m.Normals.items[VertexIndices[j * 4 + 3]] :=
                  VectorAdd(m.Normals.items[VertexIndices[j * 4 + 3]], n);
              end;
            end;
        end;
  for i := 0 to m.Normals.count - 1 do
    m.Normals.items[i] := VectorNormalize(m.Normals.items[i]);
end;


procedure TGLIDTFVectorFile.SaveToStream(aStream: TStream);
var
  S: String;
  Mesh: TGLMeshObject;
  FaceCount, ModelPositionCount, NormalCount, I, J: Integer;
  FacePositionList, NormalList, ModelPositionList, ModelNormalList, FaceShadingList: String;
  Lines: TStringList;
  Indicies: TGLIntegerList;

  function FormatVector(const AVector: TAffineVector): string;
  begin
    Result:= SingleToStr(AVector.X) + ' ' + SingleToStr(AVector.Y) + ' ' + SingleToStr(AVector.Z);
  end;
begin
  // ++ todo: save more than one mesh
  Mesh:= Owner.MeshObjects[0];
//  BuildNormals(Mesh);


  // count
  NormalList:= '';
  ModelNormalList:= '';

  // faces
  // MESH_FACE_POSITION_LIST
  FaceCount:= 0;
  FacePositionList:= '';
  for i := 0 to Mesh.FaceGroups.count - 1 do
    if Mesh.FaceGroups[i] is TFGVertexIndexList then
      with Mesh.FaceGroups[i] as TFGVertexIndexList do
        begin
          // face indicies
          Indicies:= TFGVertexIndexList(Mesh.FaceGroups[i]).VertexIndices;
          Inc(FaceCount, Indicies.Count div 3);
          J:= 0;
          while J < Indicies.Count do
            begin
              FacePositionList:= FacePositionList + '                        ' + Format('%d %d %d', [Indicies[J +2], Indicies[J +1], Indicies[J]]) + #13#10;
              inc(J, 3);
            end;
        end;
  FacePositionList:= Trim(FacePositionList);

  FaceShadingList:= '';
  for i := 1 to FaceCount do
   FaceShadingList:= FaceShadingList + '                        0' + #13#10;
   FaceShadingList:= Trim(FaceShadingList);
  // verticies
  ModelPositionList:= '';
  ModelPositionCount:= Mesh.Vertices.Count;
  for I:= 0 to Pred(Mesh.Vertices.Count) do
    ModelPositionList:= ModelPositionList + '                        ' + FormatVector(Mesh.Vertices[I]) + #13#10;
  ModelPositionList:= Trim(ModelPositionList);
  // points

  // normals
//  FMeshObject.BuildNormals(FMeshObject.Vertices., momFaceGroups);  ++
  NormalCount:= Mesh.Normals.Count;
  NormalList:= '';
  for I:= 0 to Pred(Mesh.Normals.Count) do
    NormalList:= NormalList + '                        ' + FormatVector(Mesh.Normals[I]) + #13#10;
  NormalList:= Trim(NormalList);


  // build the IDTF file
  S:= Format(ConstIDTFTemplate, [FaceCount, ModelPositionCount, NormalCount,
    FacePositionList, NormalList, FaceShadingList, ModelPositionList, ModelNormalList]);
  ;
  Lines:= TStringList.Create;
  try
    Lines.Text:= S;
    Lines.SaveToStream(aStream, TEncoding.ASCII);
  finally
    Lines.Free;
  end;
end;


{ TGLU3DVectorFile }

class function TGLU3DVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcWrite];
end;

procedure TGLU3DVectorFile.SaveToStream(aStream: TStream);
var
  TempStream: TStream;
  TempInFile, TempOutFile, Params: String;
begin
  TempInFile:= IncludeTrailingPathDelimiter(GetTempPath) + 'GLObject.idtf';
  // save as temp .idtf file
  TempStream:= TFileStream.Create(TempInFile, fmCreate);
  try
    inherited SaveToStream(TempStream);
  finally
    TempStream.Free;
  end;
  // convert IDTF to U3D using U3DConverter
  TempOutFile:= ChangeFileExt(TempInFile, '.u3d');
  Params:= Format('-i "%s" -o "%s"', [TempInFile, TempOutFile]);
  ExecProgramAndWait(IDTFConverterFileName, Params);
  // copy U3D file to stream
  TempStream:= TFileStream.Create(TempOutFile, fmOpenRead or fmShareDenyWrite);
  try
    AStream.CopyFrom(TempStream, 0);
  finally
    TempStream.Free;
  end;
  DeleteFile(TempInFile);
  DeleteFile(TempOutFile);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  USFormat:= TFormatSettings.Create('en_us');
  IDTFConverterFileName:= IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'IDTFConverter.exe';

  // register formats
  RegisterVectorFileFormat('idtf', 'Intermediate Data Text File', TGLIDTFVectorFile);
  RegisterVectorFileFormat('u3d', 'Universal 3D', TGLU3DVectorFile);


end.
