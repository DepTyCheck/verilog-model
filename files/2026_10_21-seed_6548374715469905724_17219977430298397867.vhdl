-- Seed: 6548374715469905724,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity wxowsaa is
  port (knatc : linkage std_logic; usilciydwi : buffer real; ztxvbf : out real; c : linkage severity_level);
end wxowsaa;

architecture upjfrolj of wxowsaa is
  
begin
  -- Single-driven assignments
  ztxvbf <= usilciydwi;
end upjfrolj;

entity jukotubpjz is
  port (kgbj : linkage real_vector(3 to 2));
end jukotubpjz;

library ieee;
use ieee.std_logic_1164.all;

architecture q of jukotubpjz is
  signal ls : severity_level;
  signal foio : real;
  signal mi : real;
  signal lwiizwo : severity_level;
  signal fzvmulamfm : real;
  signal aozvlanwpv : real;
  signal gtcjqnmz : std_logic;
  signal upvss : severity_level;
  signal ooa : real;
  signal dzsvunami : real;
  signal gvfpaxv : std_logic;
  signal lkx : severity_level;
  signal huhialbm : real;
  signal ebw : real;
  signal gb : std_logic;
begin
  bk : entity work.wxowsaa
    port map (knatc => gb, usilciydwi => ebw, ztxvbf => huhialbm, c => lkx);
  apxtbby : entity work.wxowsaa
    port map (knatc => gvfpaxv, usilciydwi => dzsvunami, ztxvbf => ooa, c => upvss);
  sjhzqbe : entity work.wxowsaa
    port map (knatc => gtcjqnmz, usilciydwi => aozvlanwpv, ztxvbf => fzvmulamfm, c => lwiizwo);
  ckkjau : entity work.wxowsaa
    port map (knatc => gvfpaxv, usilciydwi => mi, ztxvbf => foio, c => ls);
  
  -- Multi-driven assignments
  gvfpaxv <= 'U';
end q;

entity pavpxfp is
  port (vivd : buffer integer; z : out real);
end pavpxfp;

library ieee;
use ieee.std_logic_1164.all;

architecture feislak of pavpxfp is
  signal ccwmy : severity_level;
  signal fwjx : real;
  signal lyzrf : severity_level;
  signal svmyt : real;
  signal rnnnlmi : real;
  signal j : severity_level;
  signal dlsqz : real;
  signal yadhd : real;
  signal h : std_logic;
begin
  td : entity work.wxowsaa
    port map (knatc => h, usilciydwi => yadhd, ztxvbf => dlsqz, c => j);
  aliblvd : entity work.wxowsaa
    port map (knatc => h, usilciydwi => rnnnlmi, ztxvbf => svmyt, c => lyzrf);
  vxkyv : entity work.wxowsaa
    port map (knatc => h, usilciydwi => fwjx, ztxvbf => z, c => ccwmy);
  
  -- Single-driven assignments
  vivd <= 16#1#;
  
  -- Multi-driven assignments
  h <= h;
end feislak;



-- Seed after: 1844339934669382231,17219977430298397867
