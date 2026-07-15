-- Seed: 15311907182131800735,2983771601630957889

use std.reflection.all;

entity gwc is
  port (variable kllrlbxff : inout physical_value_mirror_pt; zosrsmoycc : inout bit; variable xdhzxhu : inout protected_value_mirror_pt);
end gwc;

architecture mukhvis of gwc is
  
begin
  -- Single-driven assignments
  zosrsmoycc <= '0';
end mukhvis;

use std.reflection.all;

entity nzbypom is
  port (d : linkage time; variable vlzvb : inout subtype_mirror_pt; hwdgto : linkage bit; variable ady : inout subtype_mirror_pt);
end nzbypom;

use std.reflection.all;

architecture kchy of nzbypom is
  shared variable rqvmvuloi : protected_value_mirror_pt;
  signal evkj : bit;
  shared variable mainx : physical_value_mirror_pt;
begin
  rumeuudc : entity work.gwc
    port map (kllrlbxff => mainx, zosrsmoycc => evkj, xdhzxhu => rqvmvuloi);
end kchy;

use std.reflection.all;

entity oeahobxmh is
  port (r : out boolean; variable ur : inout record_value_mirror_pt; variable aflqych : inout integer_subtype_mirror_pt);
end oeahobxmh;

use std.reflection.all;

architecture lez of oeahobxmh is
  shared variable pfsjrrppea : protected_value_mirror_pt;
  signal lvha : bit;
  shared variable umm : physical_value_mirror_pt;
  shared variable snzyahms : protected_value_mirror_pt;
  signal lxvk : bit;
  shared variable fggmufwuql : physical_value_mirror_pt;
  shared variable otwtdqbm : subtype_mirror_pt;
  signal oegesf : bit;
  shared variable eg : subtype_mirror_pt;
  signal ex : time;
begin
  nbfcwzk : entity work.nzbypom
    port map (d => ex, vlzvb => eg, hwdgto => oegesf, ady => otwtdqbm);
  xkt : entity work.gwc
    port map (kllrlbxff => fggmufwuql, zosrsmoycc => lxvk, xdhzxhu => snzyahms);
  pdzfa : entity work.gwc
    port map (kllrlbxff => umm, zosrsmoycc => lvha, xdhzxhu => pfsjrrppea);
  
  -- Single-driven assignments
  r <= FALSE;
end lez;

use std.reflection.all;

entity csnkicyyz is
  port (variable hi : inout array_subtype_mirror_pt);
end csnkicyyz;

use std.reflection.all;

architecture so of csnkicyyz is
  shared variable phbppankav : protected_value_mirror_pt;
  signal q : bit;
  shared variable zk : physical_value_mirror_pt;
  shared variable hqtbwvzm : protected_value_mirror_pt;
  signal fflygre : bit;
  shared variable fnierug : physical_value_mirror_pt;
begin
  xrvymvoby : entity work.gwc
    port map (kllrlbxff => fnierug, zosrsmoycc => fflygre, xdhzxhu => hqtbwvzm);
  m : entity work.gwc
    port map (kllrlbxff => zk, zosrsmoycc => q, xdhzxhu => phbppankav);
end so;



-- Seed after: 11621980185481320924,2983771601630957889
