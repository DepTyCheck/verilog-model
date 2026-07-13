-- Seed: 18019913200984531872,3566912872917928779

use std.reflection.all;

entity ptwzg is
  port (khqpsq : inout value_mirror; muznrfdn : inout enumeration_subtype_mirror; zxkgk : inout integer);
end ptwzg;

architecture yi of ptwzg is
  
begin
  
end yi;

use std.reflection.all;

entity vaooj is
  port (pnsjbdgs : inout integer_value_mirror; oqsnn : inout physical_subtype_mirror; rg : inout file_value_mirror; bovchbolj : inout time);
end vaooj;

use std.reflection.all;

architecture rufmobyclz of vaooj is
  signal eczi : integer;
  shared variable tts : enumeration_subtype_mirror;
  shared variable bmxqifey : value_mirror;
  signal f : integer;
  shared variable ikvwxyq : enumeration_subtype_mirror;
  shared variable lhcfp : value_mirror;
  signal psvnadnnve : integer;
  shared variable gcljkqveu : enumeration_subtype_mirror;
  shared variable znrsivkjkg : value_mirror;
begin
  ocaarysdq : entity work.ptwzg
    port map (khqpsq => znrsivkjkg, muznrfdn => gcljkqveu, zxkgk => psvnadnnve);
  whnr : entity work.ptwzg
    port map (khqpsq => lhcfp, muznrfdn => ikvwxyq, zxkgk => f);
  zkeuaky : entity work.ptwzg
    port map (khqpsq => bmxqifey, muznrfdn => tts, zxkgk => eczi);
  
  -- Single-driven assignments
  bovchbolj <= 2 min;
end rufmobyclz;

use std.reflection.all;

entity qk is
  port (escjgnvc : inout enumeration_value_mirror; ghkuoff : inout integer_subtype_mirror; lmnq : inout time);
end qk;

use std.reflection.all;

architecture fw of qk is
  signal qswxz : time;
  shared variable zxqaq : file_value_mirror;
  shared variable cwaqcbie : physical_subtype_mirror;
  shared variable rqxskvhsko : integer_value_mirror;
begin
  rurua : entity work.vaooj
    port map (pnsjbdgs => rqxskvhsko, oqsnn => cwaqcbie, rg => zxqaq, bovchbolj => qswxz);
  
  -- Single-driven assignments
  lmnq <= lmnq;
end fw;



-- Seed after: 15428014151349138924,3566912872917928779
