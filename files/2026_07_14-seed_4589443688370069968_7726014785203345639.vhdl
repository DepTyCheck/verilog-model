-- Seed: 4589443688370069968,7726014785203345639

use std.reflection.all;

entity js is
  port ( ojpfsteie : inout array_subtype_mirror
  ; adon : inout array_value_mirror
  ; gge : inout enumeration_value_mirror
  ; edfmfpajbo : inout file_subtype_mirror
  );
end js;

architecture quclv of js is
  
begin
  
end quclv;

use std.reflection.all;

entity dmbsc is
  port (brcaqly : inout real; mkgoqlurs : in time; puwokauyyu : inout floating_value_mirror; wpexelomnk : out time_vector(4 downto 2));
end dmbsc;

use std.reflection.all;

architecture g of dmbsc is
  shared variable w : file_subtype_mirror;
  shared variable je : enumeration_value_mirror;
  shared variable fg : array_value_mirror;
  shared variable jkfselvo : array_subtype_mirror;
  shared variable b : file_subtype_mirror;
  shared variable menaw : enumeration_value_mirror;
  shared variable bnqzp : array_value_mirror;
  shared variable f : array_subtype_mirror;
  shared variable vrrojkasyr : file_subtype_mirror;
  shared variable gznmualvbv : enumeration_value_mirror;
  shared variable wzaufug : array_value_mirror;
  shared variable fi : array_subtype_mirror;
begin
  sl : entity work.js
    port map (ojpfsteie => fi, adon => wzaufug, gge => gznmualvbv, edfmfpajbo => vrrojkasyr);
  gozfol : entity work.js
    port map (ojpfsteie => f, adon => bnqzp, gge => menaw, edfmfpajbo => b);
  ggblwn : entity work.js
    port map (ojpfsteie => jkfselvo, adon => fg, gge => je, edfmfpajbo => w);
  
  -- Single-driven assignments
  brcaqly <= 8#2_0_2_0.5_7#;
  wpexelomnk <= (1 min, 8#7_5_3.74# us, 2#1# ns);
end g;



-- Seed after: 12639956312305407196,7726014785203345639
