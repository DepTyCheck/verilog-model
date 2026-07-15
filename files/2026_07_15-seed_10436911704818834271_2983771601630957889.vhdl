-- Seed: 10436911704818834271,2983771601630957889

use std.reflection.all;

entity vzyingsckj is
  port (variable uo : inout access_subtype_mirror_pt);
end vzyingsckj;

architecture v of vzyingsckj is
  
begin
  
end v;

use std.reflection.all;

entity g is
  port (variable okpsrp : inout protected_value_mirror_pt; xzt : out boolean; variable pt : inout record_subtype_mirror_pt);
end g;

use std.reflection.all;

architecture dkgdlzw of g is
  shared variable ysvlmj : access_subtype_mirror_pt;
  shared variable lsdfnzvy : access_subtype_mirror_pt;
  shared variable lfnribc : access_subtype_mirror_pt;
  shared variable mzmsphflym : access_subtype_mirror_pt;
begin
  yjuwfx : entity work.vzyingsckj
    port map (uo => mzmsphflym);
  k : entity work.vzyingsckj
    port map (uo => lfnribc);
  jwsepg : entity work.vzyingsckj
    port map (uo => lsdfnzvy);
  dbpcyvzh : entity work.vzyingsckj
    port map (uo => ysvlmj);
  
  -- Single-driven assignments
  xzt <= xzt;
end dkgdlzw;



-- Seed after: 14764493866848237060,2983771601630957889
