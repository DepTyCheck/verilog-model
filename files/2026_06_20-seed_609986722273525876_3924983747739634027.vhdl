-- Seed: 609986722273525876,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity hhhxr is
  port (gedtlnib : inout std_logic_vector(0 downto 0); ovlgj : inout std_logic_vector(0 downto 3); xn : in severity_level; ly : inout time);
end hhhxr;

architecture vzkoxdl of hhhxr is
  
begin
  -- Multi-driven assignments
  gedtlnib <= "H";
end vzkoxdl;

entity n is
  port (humtsph : buffer time; d : inout time);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture wtiwvnitb of n is
  signal kfo : std_logic_vector(0 downto 0);
  signal uel : time;
  signal lcgcx : time;
  signal jkcx : std_logic_vector(0 downto 3);
  signal dtxmobusrg : severity_level;
  signal r : std_logic_vector(0 downto 3);
  signal bfxrofpm : std_logic_vector(0 downto 0);
begin
  xkvetmzm : entity work.hhhxr
    port map (gedtlnib => bfxrofpm, ovlgj => r, xn => dtxmobusrg, ly => d);
  abfkao : entity work.hhhxr
    port map (gedtlnib => bfxrofpm, ovlgj => jkcx, xn => dtxmobusrg, ly => lcgcx);
  amn : entity work.hhhxr
    port map (gedtlnib => bfxrofpm, ovlgj => r, xn => dtxmobusrg, ly => uel);
  iqj : entity work.hhhxr
    port map (gedtlnib => kfo, ovlgj => r, xn => dtxmobusrg, ly => humtsph);
  
  -- Single-driven assignments
  dtxmobusrg <= NOTE;
end wtiwvnitb;

entity rkrckuic is
  port (zfsmeaui : out real);
end rkrckuic;

architecture suwrpabou of rkrckuic is
  signal vsg : time;
  signal eks : time;
  signal yntyuupci : time;
  signal ttb : time;
begin
  bcqzyaguj : entity work.n
    port map (humtsph => ttb, d => yntyuupci);
  ycstmdpvu : entity work.n
    port map (humtsph => eks, d => vsg);
end suwrpabou;

library ieee;
use ieee.std_logic_1164.all;

entity tffcvtjpr is
  port (lylj : out std_logic_vector(1 to 2); qdidulr : inout time_vector(1 to 1));
end tffcvtjpr;

architecture lkjizhsk of tffcvtjpr is
  
begin
  -- Multi-driven assignments
  lylj <= "-H";
  lylj <= "UX";
  lylj <= "HL";
end lkjizhsk;



-- Seed after: 17746874092767439240,3924983747739634027
