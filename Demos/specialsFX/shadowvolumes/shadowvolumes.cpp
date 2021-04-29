/*: Shadow volumes demo.

   This demo is under construction...

   246 (what?)

   <b>History : </b><font size=-1><ul>
	  <li>29/11/03 - MF - Items now self shadow, and a new cylinder was added.
		Both changes are intended to demonstrate the problems of darkening.
	  <li>?/?/03 - EG - Creation (based on code from Mattias Fagerlund)
   </ul></font>
*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("shadowvolumes.res");
USEFORM("Unit1.cpp", Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
