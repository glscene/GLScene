(* Delphi-Unit -
   Initialization for GnuGetText
   =============================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - July 2023
   last mdified: July 2023
   *)

unit GnuGetTextInit;

interface

// InitTranslation has to be placed in the project file before "Application.Initialize"
// "Domains" is a list of all po/mo files needed by the application
// Calling sample: InitTranslation(['delphi10','indy10']);
procedure InitTranslation (const Domains : array of string);

implementation

uses GnuGetText;

{ ------------------------------------------------------------------- }
// InitTranslation has to be placed in the project file before "Application.Initialize"
procedure InitTranslation (const Domains : array of string);
var
  i  : integer;
begin
  for i:=0 to High(Domains) do AddDomainForResourceString(Domains[i]);
  end;

end.
