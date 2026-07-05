-- Seed: 974110356252280385,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity idhwr is
  port (ssjx : inout string(5 to 2); xjmhz : buffer integer; pnig : inout array_subtype_mirror; dm : linkage std_logic);
end idhwr;

architecture tcpm of idhwr is
  
begin
  
end tcpm;

use std.reflection.all;

entity zndnm is
  port (zdbobk : linkage time; xf : inout record_subtype_mirror; l : inout physical_subtype_mirror);
end zndnm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture qxbammvk of zndnm is
  shared variable zpwtbty : array_subtype_mirror;
  signal revvzdnwe : integer;
  signal rthpkwbtxg : string(5 to 2);
  shared variable xp : array_subtype_mirror;
  signal vovurdyhcq : integer;
  signal gi : string(5 to 2);
  signal kjcjb : std_logic;
  shared variable keqplwriet : array_subtype_mirror;
  signal zubmo : integer;
  signal u : string(5 to 2);
begin
  ztthygv : entity work.idhwr
    port map (ssjx => u, xjmhz => zubmo, pnig => keqplwriet, dm => kjcjb);
  vhg : entity work.idhwr
    port map (ssjx => gi, xjmhz => vovurdyhcq, pnig => xp, dm => kjcjb);
  utibdett : entity work.idhwr
    port map (ssjx => rthpkwbtxg, xjmhz => revvzdnwe, pnig => zpwtbty, dm => kjcjb);
  
  -- Multi-driven assignments
  kjcjb <= 'Z';
  kjcjb <= 'Z';
  kjcjb <= '-';
end qxbammvk;

use std.reflection.all;

entity ixko is
  port (vqujqpa : inout enumeration_value_mirror; yswaoiuj : buffer bit_vector(3 downto 2); qxl : inout record_subtype_mirror; hitrmnr : out real);
end ixko;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture gr of ixko is
  signal t : std_logic;
  shared variable potyede : array_subtype_mirror;
  signal a : integer;
  signal j : string(5 to 2);
  signal baomzdswv : std_logic;
  shared variable iqq : array_subtype_mirror;
  signal akanszwnv : integer;
  signal cbfg : string(5 to 2);
begin
  tbpbmmz : entity work.idhwr
    port map (ssjx => cbfg, xjmhz => akanszwnv, pnig => iqq, dm => baomzdswv);
  fkpgt : entity work.idhwr
    port map (ssjx => j, xjmhz => a, pnig => potyede, dm => t);
  
  -- Single-driven assignments
  hitrmnr <= 0_4_0_0_2.3330;
  yswaoiuj <= ('1', '1');
end gr;



-- Seed after: 12446644953329616021,3181554006726329157
