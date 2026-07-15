-- Seed: 14163344330023651536,2983771601630957889

use std.reflection.all;

entity ge is
  port (variable z : inout access_value_mirror_pt; hmn : buffer real; variable hrog : inout file_subtype_mirror_pt);
end ge;

architecture pit of ge is
  
begin
  -- Single-driven assignments
  hmn <= 0213.4;
end pit;

use std.reflection.all;

entity qdaurid is
  port (variable ssowghetls : inout file_value_mirror_pt; variable mcmgwerej : inout record_value_mirror_pt);
end qdaurid;

use std.reflection.all;

architecture hgsrrubitb of qdaurid is
  shared variable xwgruhfdw : file_subtype_mirror_pt;
  signal gupdhzxj : real;
  shared variable mnjmtfe : access_value_mirror_pt;
  shared variable j : file_subtype_mirror_pt;
  signal nslkrrxn : real;
  shared variable jllpq : access_value_mirror_pt;
  shared variable xuysljs : file_subtype_mirror_pt;
  signal blathu : real;
  shared variable uaaj : access_value_mirror_pt;
begin
  mxpoeq : entity work.ge
    port map (z => uaaj, hmn => blathu, hrog => xuysljs);
  vfuxmpi : entity work.ge
    port map (z => jllpq, hmn => nslkrrxn, hrog => j);
  oxyrzmhrk : entity work.ge
    port map (z => mnjmtfe, hmn => gupdhzxj, hrog => xwgruhfdw);
end hgsrrubitb;

use std.reflection.all;

entity ztvnojp is
  port (variable y : inout file_value_mirror_pt; variable fw : inout record_value_mirror_pt);
end ztvnojp;

use std.reflection.all;

architecture adhlbau of ztvnojp is
  shared variable gnre : file_subtype_mirror_pt;
  signal mteibem : real;
  shared variable e : access_value_mirror_pt;
  shared variable yr : file_subtype_mirror_pt;
  signal x : real;
  shared variable lwjuuomeh : access_value_mirror_pt;
begin
  i : entity work.ge
    port map (z => lwjuuomeh, hmn => x, hrog => yr);
  dtv : entity work.ge
    port map (z => e, hmn => mteibem, hrog => gnre);
end adhlbau;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ngh is
  port ( og : linkage integer
  ; tgurlhvrt : out std_logic_vector(3 to 2)
  ; y : linkage boolean_vector(3 to 1)
  ; variable nmvjyo : inout floating_subtype_mirror_pt
  );
end ngh;

use std.reflection.all;

architecture pleuj of ngh is
  shared variable mactnoa : file_subtype_mirror_pt;
  signal ff : real;
  shared variable vc : access_value_mirror_pt;
  shared variable iuhizniyx : record_value_mirror_pt;
  shared variable h : file_value_mirror_pt;
  shared variable pefnkjilk : file_subtype_mirror_pt;
  signal i : real;
  shared variable doxbqbn : access_value_mirror_pt;
  shared variable bmwtquc : record_value_mirror_pt;
  shared variable fh : file_value_mirror_pt;
begin
  ltk : entity work.qdaurid
    port map (ssowghetls => fh, mcmgwerej => bmwtquc);
  rorf : entity work.ge
    port map (z => doxbqbn, hmn => i, hrog => pefnkjilk);
  cyne : entity work.qdaurid
    port map (ssowghetls => h, mcmgwerej => iuhizniyx);
  wdepf : entity work.ge
    port map (z => vc, hmn => ff, hrog => mactnoa);
  
  -- Multi-driven assignments
  tgurlhvrt <= (others => '0');
  tgurlhvrt <= tgurlhvrt;
  tgurlhvrt <= tgurlhvrt;
  tgurlhvrt <= (others => '0');
end pleuj;



-- Seed after: 2992404989155993264,2983771601630957889
