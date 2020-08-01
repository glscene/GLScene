//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit FileQ3BSP;

(* Simple Quake III BSP file loader. *)

interface

uses 
  System.Classes, 
  System.SysUtils,
  Scene.VectorTypes;

const FACE_POLYGON = 1;
const MAX_TEXTURES = 1000;

type

   TBSPHeader = record
      StrID : array [0..3] of AnsiChar;    // This should always be 'IBSP'
      Version : Integer;               // This should be 0x2e for Quake 3 files
   end;

   TBSPLump = record
      Offset : Integer;                // The offset into the file for the start of this lump
      Length : Integer;                // The length in bytes for this lump
   end;

   TBSPBBox = array [0..5] of Integer;

   TBSPNode = record
      Plane : Integer;                       // Space partitioning plane
      Children : array [0..1] of Integer;    // Back and front child nodes
      BBox : TBSPBBox;                       // Bounding box of node
   end;

   TBSPLeaf = record
      Cluster : Integer;                  // Visibility cluster number
      Area : Integer;                     // Volume of the leaf
      BBox : TBSPBBox;                    // Bounding box of leaf
      FirstFace, NumFaces : Integer;      // Lookup for the face list (indexes are for faces)
      FirstBrush, NumBrushes : Integer;   // Lookup for the brush list (indexes are for brushes)
   end;

   TBSPModel = record
      BBox : TBSPBBox;                    // Bounding box of model
      FirstFace, NumFaces : Integer;      // Lookup for the face list (indexes are for faces)
      FirstBrush, NumBrushes : Integer;   // Lookup for the brush list (indexes are for brushes)
   end;

   TBSPVertex = record
      Position      : TVector3f;             // (x, y, z) position.
      TextureCoord  : TVector2f;             // (u, v) texture coordinate
      LightmapCoord : TVector2f;             // (u, v) lightmap coordinate
      Normal        : TVector3f;             // (x, y, z) normal vector
      Color         : array [0..3] of Byte   // RGBA color for the vertex
   end;
   PBSPVertex = ^TBSPVertex;

   TBSPFace = record
      textureID : Integer;                   // The index into the texture array
      effect    : Integer;                   // The index for the effects (or -1 = n/a)
      FaceType  : Integer;                   // 1=polygon, 2=patch, 3=mesh, 4=billboard
      startVertIndex : Integer;              // The starting index into this face's first vertex
      numOfVerts     : Integer;              // The number of vertices for this face
      meshVertIndex  : Integer;              // The index into the first meshvertex
      numMeshVerts   : Integer;              // The number of mesh vertices
      lightmapID     : Integer;              // The texture index for the lightmap
      lMapCorner : array [0..1] of Integer;  // The face's lightmap corner in the image
      lMapSize   : array [0..1] of Integer;  // The size of the lightmap section
      lMapPos  : TVector3f;                  // The 3D origin of lightmap.
      lMapVecs : array [0..1] of TVector3f;  // The 3D space for s and t unit vectors.
      vNormal  : TVector3f;                  // The face normal.
      Size : array [0..1] of Integer;        // The bezier patch dimensions.
   end;
   PBSPFace = ^TBSPFace;

   TBSPTexture = record
      TextureName : array [0..63] of WideChar;   // The name of the texture w/o the extension
      flags    : Integer;                    // The surface flags (unknown)
      contents : Integer;                    // The content flags (unknown)
   end;
   PBSPTexture = ^TBSPTexture;

   TBSPLightmap = record
      imageBits : array [0..49151] of Byte;     // The RGB data in a 128x128 image
   end;
   PBSPLightmap = ^TBSPLightmap;
   
   TBSPVisData = record
      numOfClusters : Integer;
      bytesPerCluster : Integer;
      bitSets : array of Byte;
   end;

   TQ3BSP = class (TObject)
      public
         Header         : TBSPHeader;
         Lumps          : array of TBSPLump;
         NumOfVerts     : Integer;
         NumOfNodes     : Integer;
         NumOfPlanes    : Integer;
         NumOfLeaves    : Integer;
         NumOfFaces     : Integer;
         NumOfTextures  : Integer;
         NumOfLightmaps : Integer;
         Vertices       : array of TBSPVertex;
         Nodes          : array of TBSPNode;
         Planes         : array of TVector4f;
         Leaves         : array of TBSPLeaf;
         Faces          : array of TBSPFace;
         Textures       : array of TBSPTexture; // texture names (without extension)
         Lightmaps      : array of TBSPLightmap;
         VisData        : TBSPVisData;
         constructor Create(bspStream : TStream);
   end;

const
   kEntities    = 0;            // Stores player/object positions, etc...
   kTextures    = 1;            // Stores texture information
   kPlanes      = 2;            // Stores the splitting planes
   kNodes       = 3;            // Stores the BSP nodes
   kLeafs       = 4;            // Stores the leafs of the nodes
   kLeafFaces   = 5;            // Stores the leaf's indices into the faces
   kLeafBrushes = 6;            // Stores the leaf's indices into the brushes
   kModels      = 7;            // Stores the info of world models
   kBrushes     = 8;            // Stores the brushes info (for collision)
   kBrushSides  = 9;            // Stores the brush surfaces info
   kVertices    = 10;           // Stores the level vertices
   kMeshVerts   = 11;           // Stores the model vertices offsets
   kShaders     = 12;           // Stores the shader files (blending, anims..)
   kFaces       = 13;           // Stores the faces for the level
   kLightmaps   = 14;           // Stores the lightmaps for the level
   kLightVolumes= 15;           // Stores extra world lighting information
   kVisData     = 16;           // Stores PVS and cluster info (visibility)
   kMaxLumps    = 17;           // A constant to store the number of lumps

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TQ3BSP ------------------
// ------------------

constructor TQ3BSP.Create(bspStream : TStream);
begin
   SetLength(Lumps, kMaxLumps);

   // Read in the header and lump data
   bspStream.Read(Header, SizeOf(Header));
   bspStream.Read(Lumps[0], kMaxLumps*SizeOf(TBSPLump));

   NumOfNodes:=Round(lumps[kNodes].length/SizeOf(TBSPNode));
   SetLength(Nodes, NumOfNodes);
   bspStream.Position:=lumps[kNodes].offset;
   bspStream.Read(Nodes[0], NumOfNodes*SizeOf(TBSPNode));

   NumOfPlanes:=Round(lumps[kPlanes].length/SizeOf(TVector4f));
   SetLength(Planes, NumOfPlanes);
   bspStream.Position:=lumps[kPlanes].offset;
   bspStream.Read(Planes[0], NumOfPlanes*SizeOf(TVector4f));

   NumOfLeaves:=Round(lumps[kLeafs].length/SizeOf(TBSPLeaf));
   SetLength(Leaves, NumOfLeaves);
   bspStream.Position:=lumps[kLeafs].offset;
   bspStream.Read(Leaves[0], NumOfLeaves*SizeOf(TBSPLeaf));

   NumOfVerts:=Round(lumps[kVertices].length/SizeOf(TBSPVertex));
   SetLength(Vertices, numOfVerts);
   bspStream.Position:=lumps[kVertices].offset;
   bspStream.Read(Vertices[0], numOfVerts*SizeOf(TBSPVertex));

   numOfFaces:=Round(lumps[kFaces].length/SizeOf(TBSPFace));
   SetLength(Faces, numOfFaces);
   bspStream.Position:=lumps[kFaces].offset;
   bspStream.Read(Faces[0], numOfFaces*SizeOf(TBSPFace));

   NumOfTextures:=Round(lumps[kTextures].length/SizeOf(TBSPTexture));
   SetLength(Textures, NumOfTextures);
   bspStream.Position:=lumps[kTextures].offset;
   bspStream.Read(Textures[0], NumOfTextures*SizeOf(TBSPTexture));

   NumOfLightmaps:=Round(lumps[kLightmaps].length/SizeOf(TBSPLightmap));
   SetLength(Lightmaps, NumOfLightmaps);
   bspStream.Position:=lumps[kLightmaps].offset;
   bspStream.Read(Lightmaps[0], NumOfLightmaps*SizeOf(TBSPLightmap));

   bspStream.Position:=lumps[kVisData].offset;
   bspStream.Read(VisData.numOfClusters, SizeOf(Integer));
   bspStream.Read(VisData.bytesPerCluster, SizeOf(Integer));
   if VisData.numOfClusters*VisData.bytesPerCluster>0 then begin
      SetLength(VisData.bitSets, VisData.numOfClusters*VisData.bytesPerCluster);
      bspStream.Read(VisData.bitSets[0], VisData.numOfClusters*VisData.bytesPerCluster);
   end;
end;

end.
