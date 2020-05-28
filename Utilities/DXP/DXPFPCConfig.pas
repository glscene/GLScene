// DXPFPCConfig
{
   FPC Globals for DXPExpert.<p>

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit DXPFPCConfig;

interface

uses Classes, DXPConfig, SysUtils;

type

   // TDXPFPCConfig
   //
   TDXPFPCConfig = class (TDXPConfig)
      private
         { Private Declarations }
         FOptions : TStrings;

      public
         { Public Declarations }

         constructor Create; override;
         destructor Destroy; override;

         procedure SaveToStream(aStream : TStream); override;
         procedure LoadFromStream(aStream : TStream); override;

         property Options : TStrings read FOptions;
   end;

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------
implementation
// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------

const
   cFPCConfigVersion = 'FPCcfg-00.01';
   cDefaultFPCoptions = '-OG -O2 -Op3 -Or -Sg -Sh -Sa -Ci -vwnh -Xs -XX';

// Create
//
constructor TDXPFPCConfig.Create;
begin
   inherited;
   FOptions:=TStringList.Create;
   TStringList(FOptions).CaseSensitive:=True;
   FOptions.CommaText:=cDefaultFPCoptions;
end;

// Destroy
//
destructor TDXPFPCConfig.Destroy;
begin
   FOptions.Free;
   inherited;
end;

// SaveToStream
//
procedure TDXPFPCConfig.SaveToStream(aStream : TStream);
begin
   WriteString(aStream, cFPCConfigVersion);
   WriteString(aStream, FOptions.CommaText);
end;

// LoadFromStream
//
procedure TDXPFPCConfig.LoadFromStream(aStream : TStream);
begin
   if ReadString(aStream)<>cFPCConfigVersion then
      raise Exception.Create(ClassName+': invalid stream');
   FOptions.CommaText:=ReadString(aStream);
end;

end.
