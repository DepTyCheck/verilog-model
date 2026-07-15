-- Seed: 12572467768762038220,2983771601630957889

use std.reflection.all;

entity em is
  port ( variable evok : inout enumeration_value_mirror_pt
  ; variable wdevfxzu : inout file_subtype_mirror_pt
  ; variable yb : inout access_subtype_mirror_pt
  ; pga : in integer
  );
end em;

architecture ply of em is
  
begin
  
end ply;

use std.reflection.all;

entity df is
  port (variable e : inout integer_subtype_mirror_pt);
end df;

use std.reflection.all;

architecture osapkcqap of df is
  signal evcetehsch : integer;
  shared variable fdpnltldco : access_subtype_mirror_pt;
  shared variable qoctvefotw : file_subtype_mirror_pt;
  shared variable hckfeu : enumeration_value_mirror_pt;
  signal nyvjjv : integer;
  shared variable rmmwidw : access_subtype_mirror_pt;
  shared variable aejti : file_subtype_mirror_pt;
  shared variable ckwzvdvrj : enumeration_value_mirror_pt;
  signal wbnst : integer;
  shared variable xspaskcp : access_subtype_mirror_pt;
  shared variable snemyvas : file_subtype_mirror_pt;
  shared variable yxva : enumeration_value_mirror_pt;
begin
  a : entity work.em
    port map (evok => yxva, wdevfxzu => snemyvas, yb => xspaskcp, pga => wbnst);
  aihhg : entity work.em
    port map (evok => ckwzvdvrj, wdevfxzu => aejti, yb => rmmwidw, pga => nyvjjv);
  wcbspsk : entity work.em
    port map (evok => hckfeu, wdevfxzu => qoctvefotw, yb => fdpnltldco, pga => evcetehsch);
  
  -- Single-driven assignments
  wbnst <= 8#1_5#;
  evcetehsch <= wbnst;
  nyvjjv <= wbnst;
end osapkcqap;



-- Seed after: 3329963684990558540,2983771601630957889
