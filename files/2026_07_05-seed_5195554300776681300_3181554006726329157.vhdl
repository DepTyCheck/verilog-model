-- Seed: 5195554300776681300,3181554006726329157

use std.reflection.all;

entity pi is
  port (klwkp : inout array_subtype_mirror; fy : inout protected_value_mirror);
end pi;

architecture k of pi is
  
begin
  
end k;

use std.reflection.all;

entity bprj is
  port (er : inout enumeration_subtype_mirror; pwy : inout value_mirror; ixailap : out integer);
end bprj;

use std.reflection.all;

architecture i of bprj is
  shared variable nekns : protected_value_mirror;
  shared variable ewzehkdp : array_subtype_mirror;
  shared variable vgdpghrkr : protected_value_mirror;
  shared variable xwcosjnlz : array_subtype_mirror;
begin
  jw : entity work.pi
    port map (klwkp => xwcosjnlz, fy => vgdpghrkr);
  p : entity work.pi
    port map (klwkp => ewzehkdp, fy => nekns);
  
  -- Single-driven assignments
  ixailap <= 2#1#;
end i;

entity pu is
  port (exmaorqbp : linkage severity_level);
end pu;

use std.reflection.all;

architecture znpyl of pu is
  shared variable hosvzh : protected_value_mirror;
  shared variable szpd : array_subtype_mirror;
  signal pqgwdq : integer;
  shared variable vmjblyx : value_mirror;
  shared variable m : enumeration_subtype_mirror;
  shared variable isspuwzud : protected_value_mirror;
  shared variable a : array_subtype_mirror;
begin
  kbtykvid : entity work.pi
    port map (klwkp => a, fy => isspuwzud);
  narlxgbwk : entity work.bprj
    port map (er => m, pwy => vmjblyx, ixailap => pqgwdq);
  wod : entity work.pi
    port map (klwkp => szpd, fy => hosvzh);
end znpyl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity igqu is
  port (npubdh : buffer std_logic; w : inout subtype_mirror);
end igqu;

use std.reflection.all;

architecture oh of igqu is
  shared variable xzpcwvocpe : protected_value_mirror;
  shared variable q : array_subtype_mirror;
  shared variable oqlz : protected_value_mirror;
  shared variable wb : array_subtype_mirror;
  signal m : severity_level;
begin
  x : entity work.pu
    port map (exmaorqbp => m);
  jzyqixpva : entity work.pi
    port map (klwkp => wb, fy => oqlz);
  ic : entity work.pi
    port map (klwkp => q, fy => xzpcwvocpe);
  
  -- Multi-driven assignments
  npubdh <= npubdh;
  npubdh <= npubdh;
end oh;



-- Seed after: 12655879965384539615,3181554006726329157
