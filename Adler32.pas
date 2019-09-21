{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Adler32 calculation

  Version 1.1.1 (2019-09-20)

  Last change 2019-09-20

  ©2018-2019 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Adler32

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect  - github.com/TheLazyTomcat/Lib.StrRect        

===============================================================================}
unit Adler32;

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}

{$DEFINE LargeBuffer}

interface

uses
  SysUtils, Classes, AuxTypes;

type
  TAdler32 = UInt32;
  PAdler32 = ^TAdler32;

type
  EADLER32Exception = class(Exception);

  EADLER32NoStream = class(EADLER32Exception);

const
  InitialAdler32 = TAdler32($00000001);

Function Adler32ToStr(Adler32: TAdler32): String;{$IFDEF CanInline} inline; {$ENDIF}
Function StrToAdler32(const Str: String): TAdler32;
Function TryStrToAdler32(const Str: String; out Adler32: TAdler32): Boolean;
Function StrToAdler32Def(const Str: String; Default: TAdler32): TAdler32;
Function CompareAdler32(A,B: TAdler32): Integer;
Function SameAdler32(A,B: TAdler32): Boolean;{$IFDEF CanInline} inline; {$ENDIF}

Function BufferAdler32(Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32; overload;

Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32; overload;

Function AnsiStringAdler32(const Str: AnsiString): TAdler32;{$IFDEF CanInline} inline; {$ENDIF}
Function WideStringAdler32(const Str: WideString): TAdler32;{$IFDEF CanInline} inline; {$ENDIF}
Function StringAdler32(const Str: String): TAdler32;{$IFDEF CanInline} inline; {$ENDIF}

Function StreamAdler32(Stream: TStream; Count: Int64 = -1): TAdler32;
Function FileAdler32(const FileName: String): TAdler32;

//------------------------------------------------------------------------------

type
  TAdler32Context = type Pointer;

Function Adler32_Init: TAdler32Context;
procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
Function Adler32_Final(var Context: TAdler32Context; const Buffer; Size: TMemSize): TAdler32; overload;
Function Adler32_Final(var Context: TAdler32Context): TAdler32; overload;
Function Adler32_Hash(const Buffer; Size: TMemSize): TAdler32;

implementation

uses
  StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

const
{$IFDEF LargeBuffer}
  BufferSize = $100000; // 1MiB buffer
{$ELSE}
  BufferSize = 4096;    // 4KiB buffer
{$ENDIF}

  Adler32Modulo   = 65521;
  Adler32NMRounds = 5552; // number of rounds that can be done without calculating modulo

type
  TAdler32Context_Internal = record
    Adler32:  TAdler32;
  end;
  PAdler32Context_Internal = ^TAdler32Context_Internal;

//==============================================================================

Function Adler32ToStr(Adler32: TAdler32): String;
begin
Result := IntToHex(Adler32,8);
end;

//------------------------------------------------------------------------------

Function StrToAdler32(const Str: String): TAdler32;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TAdler32(StrToInt(Str))
    else
      Result := TAdler32(StrToInt('$' + Str));
  end
else Result := InitialAdler32;
end;

//------------------------------------------------------------------------------

Function TryStrToAdler32(const Str: String; out Adler32: TAdler32): Boolean;
begin
try
  Adler32 := StrToAdler32(Str);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToAdler32Def(const Str: String; Default: TAdler32): TAdler32;
begin
If not TryStrToAdler32(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareAdler32(A,B: TAdler32): Integer;
begin
If A < B then
  Result := -1
else If A > B then
  Result := 1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function SameAdler32(A,B: TAdler32): Boolean;
begin
Result := UInt32(A) = UInt32(B);
end;

//==============================================================================

Function BufferAdler32(Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32;
var
  SumA: UInt32;
  SumB: UInt32;
  Buff: PByte;
  i:    TMemSize;
begin
If Size > 0 then
  begin
    SumA := Adler32 and $FFFF;
    SumB := (Adler32 shr 16) and $FFFF;
    Buff := PByte(@Buffer);
    // rounds with deferred modulo
    while Size >= Adler32NMRounds do
      begin
        For i := 0 to Pred(Adler32NMRounds) do
          begin
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            SumA := SumA + PByte(PtrUInt(Buff) + PtrUInt(i))^;
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
            SumB := SumB + SumA;
          end;
        SumA := SumA mod Adler32Modulo;
        SumB := SumB mod Adler32Modulo;
        Inc(Buff,Adler32NMRounds);
        Dec(Size,Adler32NMRounds);
      end;
    // remaining bytes
    If Size > 0 then
      begin
        For i := 0 to Pred(Size) do
          begin
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            SumA := (SumA + PByte(PtrUInt(Buff) + PtrUInt(i))^);
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
            SumB := (SumB + SumA);
          end;
        SumA := SumA mod Adler32Modulo;
        SumB := SumB mod Adler32Modulo;
      end;
    // construct result
    Result := (SumB shl 16) or (SumA and $FFFF);
  end
else Result := Adler32;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32;
begin
Result := BufferAdler32(InitialAdler32,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringAdler32(const Str: AnsiString): TAdler32;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := BufferAdler32(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringAdler32(const Str: WideString): TAdler32;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := BufferAdler32(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringAdler32(const Str: String): TAdler32;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := BufferAdler32(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamAdler32(Stream: TStream; Count: Int64 = -1): TAdler32;
var
  Buffer:     Pointer;
  BytesRead:  Integer;

  Function Min(A,B: Int64): Int64;
  begin
    If A < B then Result := A
      else Result := B;
  end;

begin
If Assigned(Stream) then
  begin
    If Count = 0 then
      Count := Stream.Size - Stream.Position;
    If Count < 0 then
      begin
        Stream.Position := 0;
        Count := Stream.Size;
      end;
    GetMem(Buffer,BufferSize);
    try
      Result := InitialAdler32;
      repeat
        BytesRead := Stream.Read(Buffer^,Min(BufferSize,Count));
        Result := BufferAdler32(Result,Buffer^,BytesRead);
        Dec(Count,BytesRead);
      until BytesRead < BufferSize;
    finally
      FreeMem(Buffer,BufferSize);
    end;
  end
else raise EADLER32NoStream.Create('StreamAdler32: Stream is not assigned.');
end;

//------------------------------------------------------------------------------

Function FileAdler32(const FileName: String): TAdler32;
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName), fmOpenRead or fmShareDenyWrite);
try
  Result := StreamAdler32(FileStream);
finally
  FileStream.Free;
end;
end;

//==============================================================================

Function Adler32_Init: TAdler32Context;
begin
Result := AllocMem(SizeOf(TAdler32Context_Internal));
PAdler32Context_Internal(Result)^.Adler32 := InitialAdler32;
end;

//------------------------------------------------------------------------------

procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
begin
PAdler32Context_Internal(Context)^.Adler32 := BufferAdler32(PAdler32Context_Internal(Context)^.Adler32,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Adler32_Final(var Context: TAdler32Context; const Buffer; Size: TMemSize): TAdler32;
begin
Adler32_Update(Context,Buffer,Size);
Result := Adler32_Final(Context);
end;

//------------------------------------------------------------------------------

Function Adler32_Final(var Context: TAdler32Context): TAdler32;
begin
Result := PAdler32Context_Internal(Context)^.Adler32;
FreeMem(Context,SizeOf(TAdler32Context_Internal));
Context := nil;
end;

//------------------------------------------------------------------------------

Function Adler32_Hash(const Buffer; Size: TMemSize): TAdler32;
begin
Result := BufferAdler32(Buffer,Size);
end;

end.
