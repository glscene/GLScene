//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLVerletHairClasses;

(*
   Creates a single strand of hair using verlet classes. Can be used to simulate
   ropes, fur or hair. 
*)

interface

{$I GLScene.inc}

uses
  System.Classes, 
  System.SysUtils,
  GLVerletTypes, 
  Scene.VectorTypes, 
  Scene.VectorLists, 
  Scene.VectorGeometry;

type
  TVHStiffness = (vhsFull, vhsSkip1Node, vhsSkip2Node, vhsSkip3Node,
    vhsSkip4Node, vhsSkip5Node, vhsSkip6Node, vhsSkip7Node, vhsSkip8Node,
    vhsSkip9Node);
  TVHStiffnessSet = set of TVHStiffness;

  TGLVerletHair = class
  private
    FNodeList: TVerletNodeList;
    FLinkCount: integer;
    FRootDepth: single;
    FVerletWorld: TGLVerletWorld;
    FHairLength: single;
    FData: pointer;
    FStiffness: TVHStiffnessSet;
    FStiffnessList : TList;
    function GetAnchor: TVerletNode;
    function GetRoot: TVerletNode;
    function GetLinkLength: single;
    procedure AddStickStiffness(const ANodeSkip : integer);
    procedure SetStiffness(const Value: TVHStiffnessSet);
  public
    procedure BuildHair(const AAnchorPosition, AHairDirection: TAffineVector);
    procedure BuildStiffness;
    procedure ClearStiffness;
    procedure Clear;
    constructor Create(const AVerletWorld : TGLVerletWorld;
    const ARootDepth, AHairLength : single; ALinkCount : integer;
    const AAnchorPosition, AHairDirection : TAffineVector;
    const AStiffness : TVHStiffnessSet);
    destructor Destroy; override;
    property NodeList : TVerletNodeList read FNodeList;
    property VerletWorld : TGLVerletWorld read FVerletWorld;
    property RootDepth : single read FRootDepth;
    property LinkLength : single read GetLinkLength;
    property LinkCount : integer read FLinkCount;
    property HairLength : single read FHairLength;
    property Stiffness : TVHStiffnessSet read FStiffness write SetStiffness;
    property Data : pointer read FData write FData;
    {Anchor should be nailed down to give the hair stability }
    property Anchor : TVerletNode read GetAnchor;
    {Root should be nailed down to give the hair stability }
    property Root : TVerletNode read GetRoot;
  end;

//------------------------------------------------------------------
implementation
//------------------------------------------------------------------

{ TGLVerletHair }

procedure TGLVerletHair.AddStickStiffness(const ANodeSkip: integer);
var
  i : integer;
begin
  for i := 0 to NodeList.Count-(1+ANodeSkip*2) do
    FStiffnessList.Add(VerletWorld.CreateStick(NodeList[i], NodeList[i+2*ANodeSkip]));
end;

procedure TGLVerletHair.BuildHair(const AAnchorPosition, AHairDirection: TAffineVector);
var
  i : integer;
  Position : TAffineVector;
  Node, PrevNode : TVerletNode;
  Direction : TAffineVector;
begin
  Clear;

  Direction := VectorNormalize(AHairDirection);

  // Fix the root of the hair
  Position := VectorAdd(AAnchorPosition, VectorScale(Direction, -FRootDepth));
  Node := VerletWorld.CreateOwnedNode(Position);
  NodeList.Add(Node);
  Node.NailedDown := true;
  PrevNode := Node;

  // Now add the links in the hair
  for i := 0 to FLinkCount-1 do
  begin
    Position := VectorAdd(AAnchorPosition, VectorScale(Direction, HairLength * (i/LinkCount)));

    Node := VerletWorld.CreateOwnedNode(Position);
    NodeList.Add(Node);

    // first one is the anchor
    if i=0 then
      Node.NailedDown := true
    else
      // Creates the hair link
      VerletWorld.CreateStick(PrevNode, Node);

    PrevNode := Node;
  end;

  // Now we must stiffen the hair with either sticks or springs
  BuildStiffness;
end;

procedure TGLVerletHair.BuildStiffness;
var
  i : integer;
begin
  ClearStiffness;

  if vhsFull in FStiffness then
  begin
    for i := 1 to 100 do
      AddStickStiffness(i);
      
    exit;
  end;

  if vhsSkip1Node in FStiffness then AddStickStiffness(1);
  if vhsSkip2Node in FStiffness then AddStickStiffness(2);
  if vhsSkip3Node in FStiffness then AddStickStiffness(3);
  if vhsSkip4Node in FStiffness then AddStickStiffness(4);
  if vhsSkip5Node in FStiffness then AddStickStiffness(5);
  if vhsSkip6Node in FStiffness then AddStickStiffness(6);
  if vhsSkip7Node in FStiffness then AddStickStiffness(7);
  if vhsSkip8Node in FStiffness then AddStickStiffness(8);
  if vhsSkip9Node in FStiffness then AddStickStiffness(9);
end;

procedure TGLVerletHair.Clear;
var
  i : integer;
begin
  ClearStiffness;
  for i := FNodeList.Count-1 downto 0 do
    FNodeList[i].Free;

  FNodeList.Clear;
  FStiffnessList.Clear;
end;

procedure TGLVerletHair.ClearStiffness;
var
  i : integer;
begin
  for i := 0 to FStiffnessList.Count-1 do
    TVerletConstraint(FStiffnessList[i]).Free;

  FStiffnessList.Clear;
end;

constructor TGLVerletHair.Create(const AVerletWorld : TGLVerletWorld;
      const ARootDepth, AHairLength : single; ALinkCount : integer;
      const AAnchorPosition, AHairDirection : TAffineVector;
      const AStiffness : TVHStiffnessSet);
begin
  FVerletWorld := AVerletWorld;
  FRootDepth := ARootDepth;
  FLinkCount := ALinkCount;
  FHairLength := AHairLength;

  FNodeList := TVerletNodeList.Create;
  FStiffness := AStiffness;
  FStiffnessList := TList.Create;

  BuildHair(AAnchorPosition, AHairDirection);
end;

destructor TGLVerletHair.Destroy;
begin
  Clear;
  FreeAndNil(FNodeList);
  FreeAndNil(FStiffnessList);
  inherited;
end;

function TGLVerletHair.GetAnchor: TVerletNode;
begin
  result := NodeList[1];
end;

function TGLVerletHair.GetLinkLength: single;
begin
  if LinkCount>0 then
    result := HairLength / LinkCount
  else
    result := 0;
end;

function TGLVerletHair.GetRoot: TVerletNode;
begin
  result := NodeList[0];
end;

procedure TGLVerletHair.SetStiffness(const Value: TVHStiffnessSet);
begin
  FStiffness := Value;
  BuildStiffness;
end;

end.
