-- Seed: 16528869791541342476,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity tueuxred is
  port (jijkdlltjk : out std_logic_vector(0 to 4); r : inout array_subtype_mirror);
end tueuxred;

architecture cvqnuk of tueuxred is
  
begin
  
end cvqnuk;

use std.reflection.all;

entity hqalliomxm is
  port (owhilmr : inout record_value_mirror; zuv : inout array_subtype_mirror);
end hqalliomxm;

architecture rqspabgic of hqalliomxm is
  
begin
  
end rqspabgic;

use std.reflection.all;

entity zbuuntl is
  port (ndgdyo : inout access_value_mirror; pbatao : in bit; i : inout floating_value_mirror);
end zbuuntl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture jqjggpuuic of zbuuntl is
  shared variable qghp : array_subtype_mirror;
  signal avyposljnf : std_logic_vector(0 to 4);
  shared variable wkyqnaa : array_subtype_mirror;
  shared variable verlioedjw : array_subtype_mirror;
  signal oirxxfwix : std_logic_vector(0 to 4);
begin
  cvzkiajufd : entity work.tueuxred
    port map (jijkdlltjk => oirxxfwix, r => verlioedjw);
  kgynbkxyt : entity work.tueuxred
    port map (jijkdlltjk => oirxxfwix, r => wkyqnaa);
  fyfm : entity work.tueuxred
    port map (jijkdlltjk => avyposljnf, r => qghp);
  
  -- Multi-driven assignments
  avyposljnf <= oirxxfwix;
  oirxxfwix <= oirxxfwix;
  avyposljnf <= ('H', '1', '-', 'U', '-');
  oirxxfwix <= oirxxfwix;
end jqjggpuuic;

use std.reflection.all;

entity dgrxr is
  port (bmn : inout file_value_mirror);
end dgrxr;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture pt of dgrxr is
  shared variable xkpzs : array_subtype_mirror;
  shared variable sfmmdrjwj : record_value_mirror;
  shared variable ludujn : floating_value_mirror;
  signal w : bit;
  shared variable lsvlgqua : access_value_mirror;
  shared variable cswfzi : floating_value_mirror;
  signal p : bit;
  shared variable mhy : access_value_mirror;
  shared variable vmhdcycx : array_subtype_mirror;
  signal jpqn : std_logic_vector(0 to 4);
begin
  xwz : entity work.tueuxred
    port map (jijkdlltjk => jpqn, r => vmhdcycx);
  ljy : entity work.zbuuntl
    port map (ndgdyo => mhy, pbatao => p, i => cswfzi);
  htyq : entity work.zbuuntl
    port map (ndgdyo => lsvlgqua, pbatao => w, i => ludujn);
  wngii : entity work.hqalliomxm
    port map (owhilmr => sfmmdrjwj, zuv => xkpzs);
  
  -- Single-driven assignments
  p <= '0';
end pt;



-- Seed after: 11541946077025166133,7726014785203345639
