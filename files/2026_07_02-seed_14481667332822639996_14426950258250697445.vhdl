-- Seed: 14481667332822639996,14426950258250697445

use std.reflection.all;

entity fh is
  port (jpo : inout record_value_mirror);
end fh;

architecture sgjpe of fh is
  
begin
  
end sgjpe;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity g is
  port (ykzwj : inout enumeration_subtype_mirror; pzkeiwpdm : buffer time; ljtk : out std_logic_vector(1 to 4));
end g;

use std.reflection.all;

architecture youlunzda of g is
  shared variable eamvly : record_value_mirror;
  shared variable atxbrffwj : record_value_mirror;
  shared variable cwedark : record_value_mirror;
  shared variable mms : record_value_mirror;
begin
  sue : entity work.fh
    port map (jpo => mms);
  rt : entity work.fh
    port map (jpo => cwedark);
  xhvykxo : entity work.fh
    port map (jpo => atxbrffwj);
  ycptrzxh : entity work.fh
    port map (jpo => eamvly);
  
  -- Single-driven assignments
  pzkeiwpdm <= 0 sec;
  
  -- Multi-driven assignments
  ljtk <= ljtk;
  ljtk <= ('U', 'W', 'L', '1');
  ljtk <= ('L', '1', 'Z', 'Z');
end youlunzda;

entity f is
  port (ig : linkage boolean);
end f;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture vtknpalmbm of f is
  shared variable vrjgj : record_value_mirror;
  shared variable hkrmqrldyx : record_value_mirror;
  signal gbovyz : std_logic_vector(1 to 4);
  signal r : time;
  shared variable ccbtkz : enumeration_subtype_mirror;
begin
  ynaffqu : entity work.g
    port map (ykzwj => ccbtkz, pzkeiwpdm => r, ljtk => gbovyz);
  lrj : entity work.fh
    port map (jpo => hkrmqrldyx);
  lex : entity work.fh
    port map (jpo => vrjgj);
end vtknpalmbm;



-- Seed after: 3533005562362632578,14426950258250697445
