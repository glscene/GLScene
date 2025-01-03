/*
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Image View Layers Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf <andre@metaexception.de>
 * Christian-W. Budde <Christian@aixcoustic.com>
 *
 * ***** END LICENSE BLOCK ***** *)
*/
//---------------------------------------------------------------------------

#ifndef MainUnitCH
#define MainUnitCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GR32_Image.hpp"
#include "GR32_RangeBars.hpp"
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
class TForm4 : public TForm
{
__published:	// IDE-managed Components
	TImgView32 *ImgView;
	TPanel *PnlControl;
	TPanel *PnlImage;
	TLabel *LblScale;
	TComboBox *ScaleCombo;
	TPanel *PnlImageHeader;
	TCheckBox *CbxImageInterpolate;
	TCheckBox *CbxOptRedraw;
	TPanel *PnlBitmapLayer;
	TLabel *LblOpacity;
	TPanel *PnlBitmapLayerHeader;
	TGaugeBar *GbrLayerOpacity;
	TCheckBox *CbxLayerInterpolate;
	TButton *BtnLayerRescale;
	TButton *BtnLayerResetScale;
	TCheckBox *CbxCropped;
	TPanel *PnlMagnification;
	TLabel *LblMagifierOpacity;
	TLabel *LblMagnification;
	TLabel *LblRotation;
	TPanel *PnlMagnificationHeader;
	TGaugeBar *GbrMagnOpacity;
	TGaugeBar *GbrMagnMagnification;
	TGaugeBar *GbrMagnRotation;
	TCheckBox *CbxMagnInterpolate;
	TPanel *PnlButtonMockup;
	TLabel *LblBorderRadius;
	TLabel *LblBorderWidth;
	TPanel *PnlButtonMockupHeader;
	TGaugeBar *GbrBorderRadius;
	TGaugeBar *GbrBorderWidth;
	TMainMenu *MainMenu;
	TMenuItem *MnuFile;
	TMenuItem *MnuFileNew;
	TMenuItem *MnuFileOpen;
	TMenuItem *N6;
	TMenuItem *MnuPrint;
	TMenuItem *MnuLayers;
	TMenuItem *MnuNewBitmapLayer;
	TMenuItem *MnuNewBitmapRGBA;
	TMenuItem *MnuNewCustomLayer;
	TMenuItem *MnuSimpleDrawing;
	TMenuItem *MnuButtonMockup;
	TMenuItem *MnuMagnifier;
	TMenuItem *N4;
	TMenuItem *MnuFlatten;
	TMenuItem *MimArrange;
	TMenuItem *MnuBringFront;
	TMenuItem *MnuSendBack;
	TMenuItem *N1;
	TMenuItem *MnuLevelUp;
	TMenuItem *MnuLevelDown;
	TMenuItem *N7;
	TMenuItem *MnuScaled;
	TMenuItem *N3;
	TMenuItem *MnuFlipHorz;
	TMenuItem *MnuFlipVert;
	TMenuItem *N5;
	TMenuItem *MnuRotate90;
	TMenuItem *MnuRotate180;
	TMenuItem *MnuRotate270;
	TMenuItem *N2;
	TMenuItem *MnuDelete;
	TOpenPictureDialog *OpenPictureDialog;
	TSaveDialog *SaveDialog;
private:	// User declarations
public:		// User declarations
	__fastcall TForm4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm4 *Form4;
//---------------------------------------------------------------------------
#endif
