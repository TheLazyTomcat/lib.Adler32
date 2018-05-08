unit Adler32;

interface

uses
  AuxTypes;

type
  TAdler32 = UInt32;
  PAdler32 = ^TAdler32;

const
  InitialAdler32 = TAdler32($00000001);

Function BufferAdler32(Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32;

implementation

const
  Adler32Modulo   = 65521;
  Adler32NMRounds = 5552; // number of rounds that can be done without calculating modulo

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
            SumA := SumA + PByte(PtrUInt(Buff) + PtrUInt(i))^;
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
            SumA := (SumA + PByte(PtrUInt(Buff) + PtrUInt(i))^);
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

end.
