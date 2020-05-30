//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLPortal"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  SetGLSceneMediaDir();
  int i;
  for (i=0; i < 15; i++)
	  SGMap->Cells[i][i] = 'X';
   SGMap->Cells[8][8] = " ";
   SGMap->Col = 8;
   SGMap->Row = 12;
   GLMaterialLibrary1->AddTextureMaterial("gnd", "walkway.jpg");
   GLMaterialLibrary1->AddTextureMaterial("wall", "rawwall.jpg")->TextureScale->Y = 3;
   BBProcessClick(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BBProcessClick(TObject *Sender)
{
   int x, y, n;
   float h;
   TSectorMeshObject *Sector;
   TFGPolygon *Poly;

   h=3;
   portalCount = 0;
   triangleCount = 0;
   Portal1->MeshObjects->Clear();
   for (x = -7; x < 8; x++)
	 for (y = -7; y < 8; y++)
	 {
	  Sector = new TSectorMeshObject;
	  Sector = (TSectorMeshObject*)(Portal1->MeshObjects);
	  n = Sector->Vertices->Count;
	  Sector->Vertices->Add(x, 0, y);     Sector->Vertices->Add(x+1, 0, y);
	  Sector->Vertices->Add(x+1, 0, y+1); Sector->Vertices->Add(x, 0, y+1);
	  Sector->Vertices->Add((float)x, h, (float)y);
	  Sector->Vertices->Add((float)x+1, h, (float)y);
	  Sector->Vertices->Add((float)x+1, h, (float)y+1);
	  Sector->Vertices->Add((float)x, h, (float)y+1);
	  Sector->TexCoords->Add(0, 0, 0);    Sector->TexCoords->Add(1, 0, 0);
	  Sector->TexCoords->Add(1, 1, 0);    Sector->TexCoords->Add(0, 1, 0);
	  // ground
	  Sector->Normals->Add(0, 1, 0);
	  if (SGMap->Cells[x+7][y+7]=="")
	  {
		 Poly = new TFGPolygon;
		 Poly = (TFGPolygon*)(Sector->FaceGroups);
		 Poly->MaterialName = "gnd";
		 Poly->Add(n+0, 0, 0); Poly->Add(n+3, 0, 3);
		 Poly->Add(n+2, 0, 2); Poly->Add(n+1, 0, 1);
	  }
	  // front wall
	  Sector->Normals->Add(0, 0, 1);
	  if ((y=-7) || (SGMap->Cells[x+7][y-1+7]!=""))
	  {
		 Poly = new TFGPolygon;
		 Poly = (TFGPolygon*)(Sector->FaceGroups);
		 Poly->MaterialName = "wall";
		 triangleCount++; triangleCount++;	// Inc(triangleCount, 2);
	  }
	  else
	  {
		 Poly = new TFGPortalPolygon;
		 Poly = (TFGPortalPolygon*)(Sector->FaceGroups);
		 //Delphi - TFGPortalPolygon(poly).DestinationSectorIndex:=(x+7)*16+(y-1+7);
		 //-TFGPortalPolygon(Poly)->DestinationSectorIndex = (x+7)*16+(y-1+7);  ???
		 portalCount++;
	  }
	  Poly->Add(n+0, 1, 3);	  Poly->Add(n+1, 1, 2);
	  Poly->Add(n+5, 1, 1);	  Poly->Add(n+4, 1, 0);
	  // left wall
	  Sector->Normals->Add(1, 0, 0);
	  if ((x==-7) || (SGMap->Cells[x-1+7][y+7]!=""))
	  {
		 Poly = new TFGPolygon;
		 Poly = (TFGPolygon*) (Sector->FaceGroups);
		 Poly->MaterialName = "wall";
		 triangleCount++; triangleCount++; //Inc(triangleCount, 2);
	  }
	  else
	  {
		 //Delphi - Poly = TFGPortalPolygon.CreateOwned(Sector.FaceGroups);
		 Poly = new TFGPortalPolygon;
		 Poly = (TFGPortalPolygon*)(Sector->FaceGroups);
		 //-TFGPortalPolygon(poly)->DestinationSectorIndex = (x-1+7)*16+(y+7);
		 portalCount++;
	  }
	  Poly->Add(n+4, 2, 1); Poly->Add(n+7, 2, 0);
	  Poly->Add(n+3, 2, 3); Poly->Add(n+0, 2, 2);
	  // right wall
	  Sector->Normals->Add(-1, 0, 0);
	  if ((x==8) || (SGMap->Cells[x+1+7][y+7]!=""))
	  {
		 Poly = new TFGPolygon;
		 Poly = (TFGPolygon*)(Sector->FaceGroups);
		 Poly->MaterialName = "wall";
		 triangleCount++; triangleCount++; //Inc(triangleCount, 2);
	  }
	  else
	  {
		 //Delphi - Poly = TFGPortalPolygon.CreateOwned(Sector.FaceGroups);
		 Poly = new TFGPortalPolygon;
		 Poly = (TFGPortalPolygon*)(Sector->FaceGroups);
		 //-TFGPortalPolygon(Poly)->DestinationSectorIndex = (x+1+7)*16+(y+7);
		 portalCount++;
	  }
	  Poly->Add(n+1, 3, 3); Poly->Add(n+2, 3, 2);
	  Poly->Add(n+6, 3, 1); Poly->Add(n+5, 3, 0);
	  // back wall
	  Sector->Normals->Add(0, 0, 1);
	  if ((y=8) || (SGMap->Cells[x+7][y+1+7] != ""))
	  {
		 Poly = new TFGPolygon;
		 Poly = (TFGPolygon*)(Sector->FaceGroups);
		 Poly->MaterialName = "wall";
		 triangleCount++; triangleCount++; // Inc(triangleCount, 2);
	  }
	  else
	  {
		 //Delphi - Poly = TFGPortalPolygon.CreateOwned(Sector.FaceGroups);
		 Poly = new TFGPortalPolygon;
		 Poly = (TFGPortalPolygon*)(Sector->FaceGroups);
		 //-TFGPortalPolygon(Poly)->DestinationSectorIndex = (x+7)*16+(y+1+7);
		 portalCount++;
	  }
	  Poly->Add(n+3, 4, 2); Poly->Add(n+7, 4, 1);
	  Poly->Add(n+6, 4, 0); Poly->Add(n+2, 4, 3);
   }
   Portal1->StructureChanged();
}
//---------------------------------------------------------------------------
