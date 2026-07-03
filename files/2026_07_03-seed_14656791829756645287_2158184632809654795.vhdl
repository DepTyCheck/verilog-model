-- Seed: 14656791829756645287,2158184632809654795

use std.reflection.all;

entity tqcvkxbt is
  port (aplfyjm : inout array_subtype_mirror; yhcgciepd : in bit);
end tqcvkxbt;

architecture zyuodiolr of tqcvkxbt is
  
begin
  
end zyuodiolr;

use std.reflection.all;

entity xuaoln is
  port (yiaehj : linkage time; t : inout record_value_mirror; nzvsbeyvw : buffer bit);
end xuaoln;

use std.reflection.all;

architecture s of xuaoln is
  signal cihmke : bit;
  shared variable yro : array_subtype_mirror;
  shared variable fb : array_subtype_mirror;
begin
  woaokpxwj : entity work.tqcvkxbt
    port map (aplfyjm => fb, yhcgciepd => nzvsbeyvw);
  yzdhnh : entity work.tqcvkxbt
    port map (aplfyjm => yro, yhcgciepd => cihmke);
  
  -- Single-driven assignments
  nzvsbeyvw <= nzvsbeyvw;
  cihmke <= nzvsbeyvw;
end s;

use std.reflection.all;

entity gzsowgmi is
  port (ehxukozlrk : inout access_subtype_mirror; zaglupqd : inout integer_subtype_mirror);
end gzsowgmi;

use std.reflection.all;

architecture htrte of gzsowgmi is
  shared variable agsgm : array_subtype_mirror;
  signal u : bit;
  shared variable rj : record_value_mirror;
  signal odhfha : time;
  signal miytrqof : bit;
  shared variable aotkcgkqai : record_value_mirror;
  signal pdlkrhxs : time;
  signal zdxlraxp : bit;
  shared variable ic : record_value_mirror;
  signal svxpnxcao : time;
begin
  pp : entity work.xuaoln
    port map (yiaehj => svxpnxcao, t => ic, nzvsbeyvw => zdxlraxp);
  eujtykehk : entity work.xuaoln
    port map (yiaehj => pdlkrhxs, t => aotkcgkqai, nzvsbeyvw => miytrqof);
  updowz : entity work.xuaoln
    port map (yiaehj => odhfha, t => rj, nzvsbeyvw => u);
  gygkzfqq : entity work.tqcvkxbt
    port map (aplfyjm => agsgm, yhcgciepd => zdxlraxp);
end htrte;



-- Seed after: 164831793647070007,2158184632809654795
