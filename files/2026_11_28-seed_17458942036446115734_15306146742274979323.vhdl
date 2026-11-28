-- Seed: 17458942036446115734,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity cofcoppv is
  port (hqjthqrivb : linkage std_logic; uyvp : in std_logic_vector(3 downto 1));
end cofcoppv;

architecture okvxcaknsp of cofcoppv is
  
begin
  
end okvxcaknsp;

entity opjnyuhgfg is
  port (xqz : inout integer; dfqegfrds : linkage boolean_vector(2 downto 3));
end opjnyuhgfg;

library ieee;
use ieee.std_logic_1164.all;

architecture zyveudsogh of opjnyuhgfg is
  signal kep : std_logic_vector(3 downto 1);
  signal rsyixno : std_logic;
begin
  cmdzzkp : entity work.cofcoppv
    port map (hqjthqrivb => rsyixno, uyvp => kep);
  iciivwpp : entity work.cofcoppv
    port map (hqjthqrivb => rsyixno, uyvp => kep);
  
  -- Single-driven assignments
  xqz <= xqz;
end zyveudsogh;

entity pd is
  port (cv : buffer severity_level);
end pd;

library ieee;
use ieee.std_logic_1164.all;

architecture isofidwx of pd is
  signal rikhzs : std_logic_vector(3 downto 1);
  signal xxfgpgcdqt : std_logic;
  signal uiyqgmknq : boolean_vector(2 downto 3);
  signal orwn : integer;
begin
  erlw : entity work.opjnyuhgfg
    port map (xqz => orwn, dfqegfrds => uiyqgmknq);
  yzju : entity work.cofcoppv
    port map (hqjthqrivb => xxfgpgcdqt, uyvp => rikhzs);
  
  -- Single-driven assignments
  cv <= NOTE;
  
  -- Multi-driven assignments
  rikhzs <= ('L', '-', 'X');
  xxfgpgcdqt <= xxfgpgcdqt;
  rikhzs <= "0XH";
  xxfgpgcdqt <= 'H';
end isofidwx;

library ieee;
use ieee.std_logic_1164.all;

entity wkqwbgerz is
  port (mvjkc : linkage std_logic_vector(4 downto 4); szau : linkage time);
end wkqwbgerz;

architecture bqf of wkqwbgerz is
  signal cufki : severity_level;
  signal xuwd : boolean_vector(2 downto 3);
  signal detlupzbp : integer;
  signal ym : severity_level;
  signal htgl : severity_level;
begin
  sbptey : entity work.pd
    port map (cv => htgl);
  jrckayzwdc : entity work.pd
    port map (cv => ym);
  silu : entity work.opjnyuhgfg
    port map (xqz => detlupzbp, dfqegfrds => xuwd);
  qtqn : entity work.pd
    port map (cv => cufki);
end bqf;



-- Seed after: 4903051454359083115,15306146742274979323
