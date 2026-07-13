-- Seed: 8311744884978752165,3566912872917928779

use std.reflection.all;

entity gyzlw is
  port (dqpaonbsz : inout real; jehmabtbv : inout enumeration_subtype_mirror);
end gyzlw;

architecture w of gyzlw is
  
begin
  -- Single-driven assignments
  dqpaonbsz <= 313.4_0_0_4_1;
end w;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ix is
  port (yrzhqla : inout floating_subtype_mirror; mjujattyyz : inout enumeration_value_mirror; rbbrmfde : buffer std_logic);
end ix;

use std.reflection.all;

architecture lskrmguahr of ix is
  shared variable yhcbqxeh : enumeration_subtype_mirror;
  signal mm : real;
  shared variable qjgstw : enumeration_subtype_mirror;
  signal vhdbat : real;
  shared variable ns : enumeration_subtype_mirror;
  signal zvxlddlrcd : real;
  shared variable auleuqvg : enumeration_subtype_mirror;
  signal b : real;
begin
  dimxnb : entity work.gyzlw
    port map (dqpaonbsz => b, jehmabtbv => auleuqvg);
  rr : entity work.gyzlw
    port map (dqpaonbsz => zvxlddlrcd, jehmabtbv => ns);
  scw : entity work.gyzlw
    port map (dqpaonbsz => vhdbat, jehmabtbv => qjgstw);
  p : entity work.gyzlw
    port map (dqpaonbsz => mm, jehmabtbv => yhcbqxeh);
  
  -- Multi-driven assignments
  rbbrmfde <= rbbrmfde;
  rbbrmfde <= rbbrmfde;
  rbbrmfde <= rbbrmfde;
  rbbrmfde <= 'H';
end lskrmguahr;



-- Seed after: 3992319728274865252,3566912872917928779
