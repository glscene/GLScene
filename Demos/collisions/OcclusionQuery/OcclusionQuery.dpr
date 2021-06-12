{: Demo for Occlusion Querying (also includes timer query).

   Occlusion querying is useful for counting how many pixels (or samples) of an
   object are visible on screen.

   This demo renders a few objects normally, then queries how many pixels are
   visible of the objects rendered between the start of the query and the
   end of the query (the objects contained inside dcTestObjects dummycube).

   Any objects rendered after the query has finished won't be included in the
   results.

   A timer query is also included to see how long it takes to render the same
   objects.
}
program OcclusionQuery;

uses
  Forms,
  OcclusionQueryFm in 'OcclusionQueryFm.pas' {FormOcclusionQuery};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOcclusionQuery, FormOcclusionQuery);
  Application.Run;
end.
