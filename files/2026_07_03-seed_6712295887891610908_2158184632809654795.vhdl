-- Seed: 6712295887891610908,2158184632809654795

use std.reflection.all;

entity mypjamygo is
  port (coywzj : inout enumeration_value_mirror);
end mypjamygo;

architecture epgg of mypjamygo is
  
begin
  
end epgg;

use std.reflection.all;

entity myfdpirlt is
  port (jxwk : inout access_value_mirror; zpd : buffer integer);
end myfdpirlt;

use std.reflection.all;

architecture u of myfdpirlt is
  shared variable dpg : enumeration_value_mirror;
  shared variable jp : enumeration_value_mirror;
begin
  kvdit : entity work.mypjamygo
    port map (coywzj => jp);
  ihx : entity work.mypjamygo
    port map (coywzj => dpg);
  
  -- Single-driven assignments
  zpd <= 8#44#;
end u;

entity colr is
  port (gjmtb : out time);
end colr;

use std.reflection.all;

architecture yrsl of colr is
  shared variable uba : enumeration_value_mirror;
  shared variable nhvdx : enumeration_value_mirror;
  shared variable cvjjt : enumeration_value_mirror;
begin
  rdv : entity work.mypjamygo
    port map (coywzj => cvjjt);
  nwhykm : entity work.mypjamygo
    port map (coywzj => nhvdx);
  utp : entity work.mypjamygo
    port map (coywzj => uba);
  
  -- Single-driven assignments
  gjmtb <= gjmtb;
end yrsl;

library ieee;
use ieee.std_logic_1164.all;

entity veldksodkb is
  port (zqwukp : linkage std_logic; ksnwkmaas : buffer string(1 downto 2); tyaymdu : inout std_logic_vector(0 to 4));
end veldksodkb;

use std.reflection.all;

architecture phdpludpw of veldksodkb is
  shared variable pprvifir : enumeration_value_mirror;
  signal d : time;
  shared variable wi : enumeration_value_mirror;
begin
  omh : entity work.mypjamygo
    port map (coywzj => wi);
  mbxzwxwuqp : entity work.colr
    port map (gjmtb => d);
  ngkb : entity work.mypjamygo
    port map (coywzj => pprvifir);
  
  -- Single-driven assignments
  ksnwkmaas <= (others => ' ');
  
  -- Multi-driven assignments
  tyaymdu <= "1LZWL";
end phdpludpw;



-- Seed after: 7616401485467334982,2158184632809654795
