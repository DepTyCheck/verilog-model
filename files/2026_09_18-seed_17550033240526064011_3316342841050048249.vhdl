-- Seed: 17550033240526064011,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity usqwtic is
  port (nydlm : in time; wnslsz : out std_logic; frzfgaz : linkage std_logic_vector(4 to 1));
end usqwtic;

architecture wztykfek of usqwtic is
  
begin
  -- Multi-driven assignments
  wnslsz <= wnslsz;
  wnslsz <= wnslsz;
  wnslsz <= 'X';
end wztykfek;

entity gfqhumsb is
  port (i : linkage time; gubdutw : in integer; akok : buffer boolean_vector(4 to 4); vsl : out time);
end gfqhumsb;

library ieee;
use ieee.std_logic_1164.all;

architecture lfolwmux of gfqhumsb is
  signal nmtde : time;
  signal mt : std_logic;
  signal tluk : time;
  signal zjjg : std_logic_vector(4 to 1);
  signal jxh : std_logic_vector(4 to 1);
  signal ehvts : std_logic;
begin
  inbahj : entity work.usqwtic
    port map (nydlm => vsl, wnslsz => ehvts, frzfgaz => jxh);
  towual : entity work.usqwtic
    port map (nydlm => vsl, wnslsz => ehvts, frzfgaz => zjjg);
  eadcekiq : entity work.usqwtic
    port map (nydlm => tluk, wnslsz => mt, frzfgaz => jxh);
  ttxc : entity work.usqwtic
    port map (nydlm => nmtde, wnslsz => ehvts, frzfgaz => jxh);
  
  -- Single-driven assignments
  vsl <= 3 min;
  nmtde <= tluk;
  tluk <= 16#4_8_A.CD44# ps;
  akok <= (others => FALSE);
  
  -- Multi-driven assignments
  mt <= 'X';
  jxh <= zjjg;
  ehvts <= 'U';
end lfolwmux;

library ieee;
use ieee.std_logic_1164.all;

entity ppe is
  port (ad : out severity_level; hh : inout std_logic; oq : buffer std_logic_vector(3 downto 4));
end ppe;

library ieee;
use ieee.std_logic_1164.all;

architecture k of ppe is
  signal maimoxcn : std_logic;
  signal nlbmujznf : std_logic_vector(4 to 1);
  signal z : time;
begin
  mch : entity work.usqwtic
    port map (nydlm => z, wnslsz => hh, frzfgaz => nlbmujznf);
  q : entity work.usqwtic
    port map (nydlm => z, wnslsz => maimoxcn, frzfgaz => oq);
  
  -- Single-driven assignments
  ad <= NOTE;
  z <= z;
  
  -- Multi-driven assignments
  oq <= "";
  oq <= nlbmujznf;
end k;

entity aq is
  port (rkksskasn : buffer integer);
end aq;

architecture gfhhkpzkfg of aq is
  
begin
  -- Single-driven assignments
  rkksskasn <= rkksskasn;
end gfhhkpzkfg;



-- Seed after: 1103039089829119588,3316342841050048249
