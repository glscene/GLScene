//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FileDXF;

(*
  Support-Code to load DXF (Drawing eXchange Files) TGLFreeForm or
  TGLActor Components in GLScene.
  Note that you must manually add this unit to one of your project's uses
  to enable support for DXF at run-time.
  Turn on TwoSideLighting in your Buffer! DXF-Faces have no defined winding order
*)

interface

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorTypes,
  GLS.PersistentClasses,
  GLS.ApplicationFileIO,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.Scene,
  GLS.Texture,
  GLS.VectorFileObjects,
  GLS.Material;

type
  TGLDXFVectorFile = class(TGLVectorFile)
  private
    FSourceStream: TStream; { Load from this stream }
    FBuffer: String; { Buffer and current line }
    FLineNo: Integer; { current Line number - for error messages }
    FEof: Boolean; { Stream done? }
    FBufPos: Integer; { Position in the buffer }
    HasPushedCode: Boolean;
    PushedCode: Integer;
    FLayers: TStringList;
    FBlocks: TStringList;
    FLastpercentdone: BYTE;
  protected
    procedure PushCode(code: Integer);
    function GetCode: Integer;
    procedure SkipTable;
    procedure SkipSection;
    // procedure DoProgress (Stage: TGLProgressStage; PercentDone: single; RedrawNow: Boolean; const Msg: string);
    function NeedMesh(basemesh: TGLBaseMesh; layer: STRING): TMeshObject;
    function NeedFaceGroup(m: TMeshObject; fgmode: TGLFaceGroupMeshMode;
      fgmat: STRING): TFGVertexIndexList;
    procedure NeedMeshAndFaceGroup(basemesh: TGLBaseMesh; layer: STRING;
      fgmode: TGLFaceGroupMeshMode; fgmat: STRING; var m: TMeshObject;
      var fg: TFGVertexIndexList);

    function ReadLine: STRING;
    // Read a single line of text from the source stream, set FEof to true when done.
    function ReadInt: Integer;
    function ReadDouble: double;
    procedure ReadTables;
    procedure ReadLayer;
    procedure ReadLayerTable;
    procedure ReadBlocks;
    procedure ReadInsert(basemesh: TGLBaseMesh);
    procedure ReadEntity3Dface(basemesh: TGLBaseMesh);
    procedure ReadEntityPolyLine(basemesh: TGLBaseMesh);
    procedure ReadEntities(basemesh: TGLBaseMesh);

  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

implementation

procedure BuildNormals(m: TMeshObject); FORWARD;

const
  DXFcolorsRGB: ARRAY [1 .. 255] OF LONGINT = ($FF0000, $FFFF00, $00FF00,
    $00FFFF, $0000FF, $FF00FF, $000000, $000000, $000000, $FF0000, $FF8080,
    $A60000, $A65353, $800000, $804040, $4D0000, $4D2626, $260000, $261313,
    $FF4000, $FF9F80, $A62900, $A66853, $802000, $805040, $4D1300, $4D3026,
    $260A00, $261813, $FF8000, $FFBF80, $A65300, $A67C53, $804000, $806040,
    $4D2600, $4D3926, $261300, $261D13, $FFBF00, $FFDF80, $A67C00, $A69153,
    $806000, $807040, $4D3900, $4D4326, $261D00, $262113, $FFFF00, $FFFF80,
    $A6A600, $A6A653, $808000, $808040, $4D4D00, $4D4D26, $262600, $262613,
    $BFFF00, $DFFF80, $7CA600, $91A653, $608000, $708040, $394D00, $434D26,
    $1D2600, $212613, $80FF00, $BFFF80, $53A600, $7CA653, $408000, $608040,
    $264D00, $394D26, $132600, $1D2613, $40FF00, $9FFF80, $29A600, $68A653,
    $208000, $508040, $134D00, $304D26, $0A2600, $182613, $00FF00, $80FF80,
    $00A600, $53A653, $008000, $408040, $004D00, $264D26, $002600, $132613,
    $00FF40, $80FF9F, $00A629, $53A668, $008020, $408050, $004D13, $264D30,
    $00260A, $132618, $00FF80, $80FFBF, $00A653, $53A67C, $008040, $408060,
    $004D26, $264D39, $002613, $13261D, $00FFBF, $80FFDF, $00A67C, $53A691,
    $008060, $408070, $004D39, $264D43, $00261D, $132621, $00FFFF, $80FFFF,
    $00A6A6, $53A6A6, $008080, $408080, $004D4D, $264D4D, $002626, $132626,
    $00BFFF, $80DFFF, $007CA6, $5391A6, $006080, $407080, $00394D, $26434D,
    $001D26, $132126, $0080FF, $80BFFF, $0053A6, $537CA6, $004080, $406080,
    $00264D, $26394D, $001326, $131D26, $0040FF, $809FFF, $0029A6, $5368A6,
    $002080, $405080, $00134D, $26304D, $000A26, $131826, $0000FF, $8080FF,
    $0000A6, $5353A6, $000080, $404080, $00004D, $26264D, $000026, $131326,
    $4000FF, $9F80FF, $2900A6, $6853A6, $200080, $504080, $13004D, $30264D,
    $0A0026, $181326, $8000FF, $BF80FF, $5300A6, $7C53A6, $400080, $604080,
    $26004D, $39264D, $130026, $1D1326, $BF00FF, $DF80FF, $7C00A6, $9153A6,
    $600080, $704080, $39004D, $43264D, $1D0026, $211326, $FF00FF, $FF80FF,
    $A600A6, $A653A6, $800080, $804080, $4D004D, $4D264D, $260026, $261326,
    $FF00BF, $FF80DF, $A6007C, $A65391, $800060, $804070, $4D0039, $4D2643,
    $26001D, $261321, $FF0080, $FF80BF, $A60053, $A6537C, $800040, $804060,
    $4D0026, $4D2639, $260013, $26131D, $FF0040, $FF809F, $A60029, $A65368,
    $800020, $804050, $4D0013, $4D2630, $26000A, $261318, $545454, $767676,
    $989898, $BBBBBB, $DDDDDD, $FFFFFF);

const
  BufSize = 65536; { Load input data in chunks of BufSize Bytes. }
  LineLen = 100; { Allocate memory for the current line in chunks }

  function RGB2BGR(bgr: LONGINT): LONGINT;
  begin
    result := ((bgr SHR 16) and $FF) or (bgr AND $FF00) or
      ((bgr SHL 16) and $FF0000)
  end;

  function StreamEOF(S: TStream): Boolean;
  begin { Is the stream at its end? }
    result := (S.Position >= S.Size);
  end;

  class function TGLDXFVectorFile.Capabilities: TGLDataFileCapabilities;
  begin
    result := [dfcRead];
  end;

  function TGLDXFVectorFile.ReadLine: STRING;
  var
    j: Integer;
    FLine: STRING;
    NewlineChar: CHAR;
    procedure FillBuffer;
    var
      l: Integer;
    begin
      l := FSourceStream.Size - FSourceStream.Position;
      if l > BufSize then
        l := BufSize;
      SetLength(FBuffer, l);
      FSourceStream.Read(FBuffer[1], l);
      FBufPos := 1;
    end;

  begin
    Inc(FLineNo);
    if FBufPos < 1 then
      FillBuffer;
    j := 1;
    while True do
    begin
      if FBufPos > Length(FBuffer) then
      begin
        if StreamEOF(FSourceStream) then
        begin
          FEof := True;
          break;
        end
        else
          FillBuffer
      end
      else
      begin
        case FBuffer[FBufPos] of
          #10, #13:
            begin
              NewlineChar := FBuffer[FBufPos];
              Inc(FBufPos);
              if FBufPos > Length(FBuffer) then
                if StreamEOF(FSourceStream) then
                  break
                else
                  FillBuffer;
              if ((FBuffer[FBufPos] = #10) or (FBuffer[FBufPos] = #13)) and
                (FBuffer[FBufPos] <> NewlineChar) then
                Inc(FBufPos);
              break;
            end;
        else
          if j > Length(FLine) then
            SetLength(FLine, Length(FLine) + LineLen);
          if FBuffer[FBufPos] = #9 then
            FLine[j] := #32
          else
            FLine[j] := FBuffer[FBufPos];
          Inc(FBufPos);
          Inc(j);
        end;
      end;
    end;
    SetLength(FLine, j - 1);
    ReadLine := Trim(FLine);
  end;

{
  procedure TGLDXFVectorFile.DoProgress (Stage: TGLProgressStage; PercentDone: single; RedrawNow: Boolean; const Msg: string);
  var perc:BYTE;
  begin
  // If the following line stops your compiler, just comment this function
  if @owner.OnProgress<>NIL then
  begin
  perc:=round(percentdone);
  if (perc<>Flastpercentdone) or (msg<>'') or redrawnow then
  owner.OnProgress (owner,stage,perc,redrawnow,msg);
  Flastpercentdone:=perc;
  end;
  end;
}
  procedure TGLDXFVectorFile.PushCode(code: Integer);
  begin
    PushedCode := code;
    HasPushedCode := True;
  end;

  function TGLDXFVectorFile.GetCode: Integer;
  var
    S: STRING;
  begin
    if HasPushedCode then
    begin
      GetCode := PushedCode;
      HasPushedCode := FALSE;
    end
    else
    begin
      S := ReadLine;
      result := StrToIntDef(S, -1);
      if result = -1 then
        raise Exception.create('Invalid DXF Code ' + S + ' on Line #' +
          IntToStr(FLineNo));
    end;
  end;

  function TGLDXFVectorFile.ReadDouble: double;
  var
    S: String;
    c: CHAR;
  begin
    c := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
    S := Trim(ReadLine);
    result := StrToFloat(S);
    FormatSettings.DecimalSeparator := c;
  end;

  function TGLDXFVectorFile.ReadInt: Integer;
  var
    S: String;
  begin
    S := Trim(ReadLine);
    result := StrToInt(S);
  end;

  procedure TGLDXFVectorFile.SkipSection;
  var
    S: String;
    code: Integer;
  begin
    repeat
      code := GetCode;
      S := ReadLine;
    until (code = 0) and (S = 'ENDSEC');
  end;

  procedure TGLDXFVectorFile.SkipTable;
  var
    S: String;
    code: Integer;
  begin
    repeat
      code := GetCode;
      S := ReadLine;
    until (code = 0) and (S = 'ENDTAB');
  end;

  procedure TGLDXFVectorFile.ReadLayer;
  var
    layername, color: String;
    code: Integer;
  begin
    color := '1';
    repeat
      code := GetCode;
      case code of
        0:
          ;
        2:
          layername := ReadLine;
        70:
          ReadLine; // freeze and lock flags
        62:
          color := ReadLine;
      else
        ReadLine;
      end;
    until code = 0;
    PushCode(0);
    FLayers.AddObject(layername, POINTER(StrToIntDef(color, 1)));
  end;

  procedure TGLDXFVectorFile.ReadLayerTable;
  var
    S: STRING;
    code: Integer;
  begin
    repeat
      code := GetCode;
      S := ReadLine;
      if (code = 0) and (S = 'LAYER') then
        ReadLayer;
    until (code = 0) and (S = 'ENDTAB');
  end;

  procedure TGLDXFVectorFile.ReadTables;
  var
    S: String;
    code: Integer;
  begin
    repeat
      code := GetCode;
      S := ReadLine;
      if (code = 0) and (S = 'TABLE') then
      begin
        code := GetCode;
        S := ReadLine;
        if (code = 2) then
          if S = 'LAYER' then
            ReadLayerTable
          else
            SkipTable; // LTYPE, STYLE, UCS, and more currently skipped
      end
      until (code = 0) and (S = 'ENDSEC');
    end;

    procedure TGLDXFVectorFile.ReadBlocks;
    var
      S: String;
      code: Integer;
      blockname: String;
      blockmesh: TGLFreeForm;

    begin
      // This code reads blocks into orphaned TGLFreeForms.
      // ReadInsert then either copies or parents this object to its parent
      // unused blocks are freed upon completion
      repeat
        code := GetCode;
        S := ReadLine;
        if (code = 0) and (S = 'BLOCK') then
        begin
          blockmesh := TGLFreeForm.create(owner);
          blockmesh.IgnoreMissingTextures := True;
          blockmesh.MaterialLibrary := owner.MaterialLibrary;
          blockmesh.OnProgress := NIL;
          blockname := 'DXFBLOCK' + IntToStr(FBlocks.count);
          repeat
            code := GetCode;
            case code of
              0:
                ;
              2:
                blockname := ReadLine;
            else
              S := ReadLine;
            end;
          until code = 0;
          PushCode(0);
          FBlocks.AddObject(blockname, blockmesh);
          ReadEntities(blockmesh);
          // basemesh.Direction.SetVector(0,1,0);
          // code:=GetCode;
          // s:=ReadLine;
          // asm nop end;
        end;
      until (code = 0) and (S = 'ENDSEC');
    end;

    procedure TGLDXFVectorFile.ReadInsert(basemesh: TGLBaseMesh);
    var
      code, idx, indexoffset: Integer;
      i, j, k: Integer;
      blockname, S: STRING;
      pt, insertpoint, scale: TAffineVector;
      blockmesh: TGLBaseMesh;
      // blockproxy  :TGLProxyObject;
      mo_block: TMeshObject;
      mo_base: TMeshObject;
      fg_block, fg_base: TFGVertexIndexList;
    begin
      blockname := '';
      insertpoint := NullVector;
      scale := XYZvector;
      repeat // see ReadBlocks for details
        code := GetCode;
        case code of
          0:
            ;
          2:
            blockname := ReadLine;
          10:
            insertpoint.X := ReadDouble;
          20:
            insertpoint.Y := ReadDouble;
          30:
            insertpoint.Z := ReadDouble;
          41:
            scale.X := ReadDouble;
          42:
            scale.Y := ReadDouble;
          43:
            scale.Z := ReadDouble;
        else
          S := ReadLine;
        end;
      until code = 0;
      idx := FBlocks.IndexOf(blockname);
      if idx >= 0 then
      begin
        blockmesh := FBlocks.Objects[idx] as TGLBaseMesh;

        // FLAT STRUCTURES
        // Insert a block into its parent by copying the contents.
        // the blockmesh will be freed upon completion, leaving only the copies.
        for i := 0 to blockmesh.MeshObjects.count - 1 do
        begin
          mo_block := blockmesh.MeshObjects[i];
          mo_base := NeedMesh(basemesh, mo_block.name);
          indexoffset := mo_base.vertices.count;
          for j := 0 to mo_block.vertices.count - 1 do
          begin
            pt := mo_block.vertices[j];
            ScaleVector(pt, scale);
            AddVector(pt, insertpoint);
            mo_base.vertices.Add(pt);
          end;
          for j := 0 to mo_block.FaceGroups.count - 1 do
          begin
            fg_block := mo_block.FaceGroups[j] as TFGVertexIndexList;
            fg_base := NeedFaceGroup(mo_base, fg_block.mode,
              fg_block.MaterialName);
            for k := 0 to fg_block.VertexIndices.count - 1 do
            begin
              fg_base.VertexIndices.Add(fg_block.VertexIndices[k] +
                indexoffset);
            end;
          end;
        end;

        // TREE STRUCTURES
        // Instead of copying the contents of the block, they are parented to the
        // base mesh. If the block already has a parent, a proxy object is created.
        // WARNING: THE CODE BELOW DOES NOT WORK.
        (*
          if blockmesh.Parent =NIL then
          begin
          blockmesh.Position.AsAffineVector:=insertpoint;
          blockmesh.ShowAxes:=TRUE;
          basemesh.AddChild(blockmesh);
          for i:=0 to blockmesh.MeshObjects.Count-1 do
          BuildNormals(blockmesh.MeshObjects[i]);
          end
          else
          begin
          blockproxy:=TGLproxyObject.CreateAsChild(basemesh);
          blockproxy.MasterObject:=blockmesh;
          blockproxy.Position.AsAffineVector:=insertpoint;
          blockproxy.ShowAxes:=TRUE;
          end;
        *)
      end;
      PushCode(0);
    end;

    function TGLDXFVectorFile.NeedMesh(basemesh: TGLBaseMesh; layer: STRING)
      : TMeshObject;
    var
      i: Integer;
    begin
      i := 0;
      while (i < basemesh.MeshObjects.count) and
        not(basemesh.MeshObjects[i].name = layer) do
        Inc(i);
      if i < basemesh.MeshObjects.count then
        result := basemesh.MeshObjects[i]
      else
      begin
        result := TMeshObject.CreateOwned(basemesh.MeshObjects);
        result.mode := momFaceGroups;
        result.name := layer;
      end;
    end;

    function TGLDXFVectorFile.NeedFaceGroup(m: TMeshObject;
      fgmode: TGLFaceGroupMeshMode; fgmat: STRING): TFGVertexIndexList;
    var
      i: Integer;
      acadcolor: LONGINT;
      libmat: TGLLibMaterial;
      fg: TFGVertexIndexList;
    begin
      i := 0;
      while (i < m.FaceGroups.count) and
        not((m.FaceGroups[i] is TFGVertexIndexList) and
        ((m.FaceGroups[i] as TFGVertexIndexList).mode = fgmode) and
        (m.FaceGroups[i].MaterialName = fgmat)) do
        Inc(i);
      if i < m.FaceGroups.count then
        fg := m.FaceGroups[i] as TFGVertexIndexList
      else
      begin
        fg := TFGVertexIndexList.CreateOwned(m.FaceGroups);
        fg.mode := fgmode;
        fg.MaterialName := fgmat;
        if owner.MaterialLibrary <> NIL then
        begin
          libmat := owner.MaterialLibrary.Materials.GetLibMaterialByName(fgmat);
          if libmat = NIL then // creates a colored material
          begin
            acadcolor := StrToIntDef(fgmat, 0);
            if acadcolor in [1 .. 255] then
            begin
              libmat := owner.MaterialLibrary.Materials.Add;
              libmat.name := fgmat;
              libmat.Material.FrontProperties.Diffuse.AsWinColor :=
                RGB2BGR(DXFcolorsRGB[acadcolor]);
              libmat.Material.BackProperties.Diffuse.AsWinColor :=
                RGB2BGR(DXFcolorsRGB[acadcolor]);
              libmat.Material.FaceCulling := fcNoCull;
            end;
          end;
        end;
      end;
      result := fg;
    end;

    procedure TGLDXFVectorFile.NeedMeshAndFaceGroup(basemesh: TGLBaseMesh;
      layer: STRING; fgmode: TGLFaceGroupMeshMode; fgmat: STRING;
      var m: TMeshObject; var fg: TFGVertexIndexList);
    begin
      m := NeedMesh(basemesh, layer);
      fg := NeedFaceGroup(m, fgmode, fgmat);
    end;

    procedure TGLDXFVectorFile.ReadEntity3Dface(basemesh: TGLBaseMesh);
    var
      code, i: Integer;
      pts: ARRAY [0 .. 3] of TAffineVector;
      isquad: Boolean;
      fg: TFGVertexIndexList;
      color, layer: STRING;
      m: TMeshObject;
    begin
      color := '';
      layer := '';
      isquad := FALSE;
      for i := 0 to 3 do
        pts[i] := NullVector;
      repeat
        code := GetCode;
        case code of
          0:
            ;
          8:
            layer := ReadLine; // Layer
          10:
            pts[0].X := ReadDouble;
          11:
            pts[1].X := ReadDouble;
          12:
            pts[2].X := ReadDouble;
          13:
            begin
              pts[3].X := ReadDouble;
              isquad := True
            end;
          20:
            pts[0].Y := ReadDouble;
          21:
            pts[1].Y := ReadDouble;
          22:
            pts[2].Y := ReadDouble;
          23:
            begin
              pts[3].Y := ReadDouble;
              isquad := True
            end;
          30:
            pts[0].Z := ReadDouble;
          31:
            pts[1].Z := ReadDouble;
          32:
            pts[2].Z := ReadDouble;
          33:
            begin
              pts[3].Z := ReadDouble;
              isquad := True
            end;
          62:
            color := ReadLine; // Color
        else
          ReadLine;
        end;
      until code = 0;
      PushCode(0);
      isquad := isquad and ((pts[2].X <> pts[3].X) or (pts[2].Y <> pts[3].Y) or
        (pts[2].Z <> pts[3].Z));
      if isquad then
        NeedMeshAndFaceGroup(basemesh, layer, fgmmQuads, color, m, fg)
      else
        NeedMeshAndFaceGroup(basemesh, layer, fgmmTriangles, color, m, fg);
      fg.Add(m.vertices.FindOrAdd(pts[0]));
      fg.Add(m.vertices.FindOrAdd(pts[1]));
      fg.Add(m.vertices.FindOrAdd(pts[2]));
      if isquad then
        fg.Add(m.vertices.FindOrAdd(pts[3]));
    end;

    procedure TGLDXFVectorFile.ReadEntityPolyLine(basemesh: TGLBaseMesh);

      procedure ReadPolylineVertex(m: TMeshObject; vertexindexbase: Integer);
      var
        color: STRING;
        pt: TAffineVector;
        fg: TFGVertexIndexList;
        code, idx, i70, i71, i72, i73, i74: Integer;
      begin
        color := '';
        pt := NullVector;
        i70 := 0;
        i71 := 0;
        i72 := 0;
        i73 := 0;
        i74 := 0;
        repeat
          code := GetCode;
          case code of
            0:
              ;
            5:
              ReadLine; // ID   :=ReadHex16;
            8:
              ReadLine; // ignore per vertex layer. Polyline vertices cannot cross layers!
            10:
              pt.X := ReadDouble;
            20:
              pt.Y := ReadDouble;
            30:
              pt.Z := ReadDouble;
            62:
              color := ReadLine;
            70:
              i70 := ReadInt;
            71:
              i71 := abs(ReadInt);
              // negative values should hide points... we cannot
            72:
              i72 := abs(ReadInt);
            73:
              i73 := abs(ReadInt);
            74:
              i74 := abs(ReadInt);
            100:
              ReadLine; // Subclass Marker
            330:
              ReadLine; // Soft Pointer?
          else
            ReadLine;
          end;
        until code = 0;
        PushCode(0);
        if (color = '') or (color = '256') or (color = 'BYLAYER') then
        begin
          idx := FLayers.IndexOf(m.name);
          if idx >= 0 then
            color := IntToStr(LONGINT(FLayers.Objects[idx]));
        end;
        if i70 and 192 = 192 then
        begin
          m.vertices.Add(pt);
        end
        else if i70 and 192 = 128 then
        begin
          i71 := i71 - 1 + vertexindexbase;
          i72 := i72 - 1 + vertexindexbase;
          i73 := i73 - 1 + vertexindexbase;
          if i74 = 0 then
          begin
            fg := NeedFaceGroup(m, fgmmTriangles, color);
            fg.Add(i71);
            fg.Add(i72);
            fg.Add(i73);
          end
          else
          begin
            i74 := i74 - 1 + vertexindexbase;
            fg := NeedFaceGroup(m, fgmmQuads, color);
            fg.Add(i71);
            fg.Add(i72);
            fg.Add(i73);
            fg.Add(i74);
          end
        end
        else
          // hmm?
      end;

    var
      m: TMeshObject;
      code, vertexindexbase: Integer;
      S, layer: STRING;
    begin
      m := NIL;
      vertexindexbase := 0;
      repeat
        code := GetCode;
        S := ReadLine;
        if (code = 8) then
        begin
          layer := S;
          m := NeedMesh(basemesh, layer);
          vertexindexbase := m.vertices.count;
        end;
        if (code = 0) and (S = 'VERTEX') and (m <> NIL) then
          ReadPolylineVertex(m, vertexindexbase);
      until (code = 0) and (S = 'SEQEND');
      repeat
        code := GetCode;
        if code <> 0 then
          ReadLine;
      until (code = 0);
      PushCode(0);
    end;

    procedure TGLDXFVectorFile.ReadEntities(basemesh: TGLBaseMesh);
    var
      code: Integer;
      S: STRING;
    begin
      repeat
        code := GetCode;
        // DoProgress (psRunning,FSourceStream.Position/FSourceStream.Size*100,false,'');
        case code of
          0:
            begin
              S := ReadLine;
              if S = 'POLYLINE' then
                ReadEntityPolyLine(basemesh)
              else if S = '3DFACE' then
                ReadEntity3Dface(basemesh)
              else if S = 'INSERT' then
                ReadInsert(basemesh)
              else if S = 'ENDSEC' then
              begin
              end
              else if S = 'ENDBLK' then
              begin
              end
              else
                (*
                asm
                  nop
                end    // put breakpoint here to catch other entities
                *)
            end;
        else
          S := ReadLine;
        end;
      until (code = 0) and ((S = 'ENDSEC') or (S = 'ENDBLK'));
    end;

    // build normals
    procedure BuildNormals(m: TMeshObject);
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

    procedure TGLDXFVectorFile.LoadFromStream(aStream: TStream);
    var
      S: STRING;
      code, i: Integer;
    begin
      FLastpercentdone := 1;
      /// DoProgress (psStarting,0,false,'Starting');
      FEof := FALSE;
      FSourceStream := aStream;
      FLineNo := 0;
      HasPushedCode := FALSE;
      FLayers := TStringList.create;
      FBlocks := TStringList.create;
      while not FEof do
      begin
        /// DoProgress (psStarting,FSourceStream.Position/FSourceStream.Size*90,false,'');
        code := GetCode;
        if (code = 0) then
        begin
          S := ReadLine;
          if S = 'EOF' then
            break
          else if S = 'SECTION' then
          begin
            code := GetCode;
            if code <> 2 then
              raise Exception.create('Name must follow Section' + ' on Line #' +
                IntToStr(FLineNo))
            else
            begin
              S := ReadLine;
              if S = 'HEADER' then
                SkipSection
              else if S = 'BLOCKS' then
                ReadBlocks
              else if S = 'ENTITIES' then
                ReadEntities(owner)
              else if S = 'CLASSES' then
                SkipSection
              else if S = 'TABLES' then
                ReadTables
              else if S = 'OBJECTS' then
                SkipSection
              else
                SkipSection;
            end
          end
          else if S = 'ENDSEC' then
            raise Exception.create('SECTION/ENDSEC Mismatch' + ' on Line #' +
              IntToStr(FLineNo))
        end
        else
          S := ReadLine; // raise Exception.create ('Invalid Group Code');
      end;
      // calc normals
      FLayers.free;
      for i := FBlocks.count - 1 downto 0 do
        (FBlocks.Objects[i] as TGLFreeForm).free;
      FBlocks.free;
      for i := 0 to owner.MeshObjects.count - 1 do
        BuildNormals(owner.MeshObjects[i]);
      /// DoProgress (psEnding,100,false,'');
    end;

initialization

RegisterVectorFileFormat('dxf', 'AutoCAD Exchange Format', TGLDXFVectorFile);

end.
