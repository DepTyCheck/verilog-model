-- Seed: 5617891487742797981,2983771601630957889

use std.reflection.all;

entity zv is
  port ( variable vu : inout floating_subtype_mirror_pt
  ; variable loshzo : inout access_subtype_mirror_pt
  ; variable ieey : inout enumeration_value_mirror_pt
  );
end zv;

architecture sxnjsuxp of zv is
  
begin
  
end sxnjsuxp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity dravckk is
  port (dgsn : buffer bit; jrigwt : buffer std_logic_vector(1 to 0); variable ucuga : inout enumeration_subtype_mirror_pt);
end dravckk;

use std.reflection.all;

architecture cvwuxliaaj of dravckk is
  shared variable kdqpn : enumeration_value_mirror_pt;
  shared variable ha : access_subtype_mirror_pt;
  shared variable aogvybr : floating_subtype_mirror_pt;
  shared variable qqhsj : enumeration_value_mirror_pt;
  shared variable xrugyaljua : access_subtype_mirror_pt;
  shared variable kbod : floating_subtype_mirror_pt;
begin
  pwbo : entity work.zv
    port map (vu => kbod, loshzo => xrugyaljua, ieey => qqhsj);
  hrmd : entity work.zv
    port map (vu => aogvybr, loshzo => ha, ieey => kdqpn);
  
  -- Single-driven assignments
  dgsn <= '0';
  
  -- Multi-driven assignments
  jrigwt <= jrigwt;
  jrigwt <= jrigwt;
end cvwuxliaaj;



-- Seed after: 5205347357070310767,2983771601630957889
