{: Mushroom frenzy : demonstrates loading 3DS files and using proxy objects.

   In this sample, we have a single 3DS mesh (a mushroom), and we want to display
   a whole bunch of mushrooms. To reach this goal, we use a TGLFreeForm and load
   the 3DS mesh with its "LoadFromFile" method.

   The other mushrooms are obtained with proxy objects (see "AddMushrooms"),
   our freeform is used as MasterObject, the scale and position are then randomized
   and scattered around our ground (a textured disk).

   This results could also have been obtained by creating FreeForms instead of
   ProxyObjects, but using ProxyObjects avoids duplicating mesh data and helps
   in sustaining better framerates (the same data and build list is shared among
   all mushrooms).
}
program MushroomD;

uses
  Forms,
  fMushroomD in 'fMushroomD.pas' {FormMushroom};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMushroom, FormMushroom);
  Application.Run;
end.
