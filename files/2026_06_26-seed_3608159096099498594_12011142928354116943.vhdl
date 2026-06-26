-- Seed: 3608159096099498594,12011142928354116943

entity kdzc is
  port (i : buffer real_vector(2 downto 2));
end kdzc;

architecture rkvmtsj of kdzc is
  
begin
  
end rkvmtsj;

entity e is
  port (krcf : inout boolean_vector(1 downto 3); dtlt : out real);
end e;

architecture szcqzq of e is
  signal otyxmk : real_vector(2 downto 2);
  signal yrve : real_vector(2 downto 2);
begin
  u : entity work.kdzc
    port map (i => yrve);
  ggzdt : entity work.kdzc
    port map (i => otyxmk);
  
  -- Single-driven assignments
  dtlt <= 8#4_2_3_4.1515#;
  krcf <= (others => TRUE);
end szcqzq;

entity rlbqh is
  port (rmjvv : linkage real_vector(3 downto 4));
end rlbqh;

architecture crf of rlbqh is
  signal mhspgbzcg : real_vector(2 downto 2);
begin
  snz : entity work.kdzc
    port map (i => mhspgbzcg);
end crf;

library ieee;
use ieee.std_logic_1164.all;

entity szmxivsgo is
  port (sdywn : linkage integer; sjn : inout time; z : out std_logic_vector(4 downto 0));
end szmxivsgo;

architecture miod of szmxivsgo is
  signal brtdoamf : real_vector(2 downto 2);
  signal ep : real_vector(3 downto 4);
  signal oazrhvzm : real;
  signal wqtmtoqz : boolean_vector(1 downto 3);
  signal wzsyrqj : real_vector(2 downto 2);
begin
  lppzrk : entity work.kdzc
    port map (i => wzsyrqj);
  rslqt : entity work.e
    port map (krcf => wqtmtoqz, dtlt => oazrhvzm);
  qfe : entity work.rlbqh
    port map (rmjvv => ep);
  xpgfmkmz : entity work.kdzc
    port map (i => brtdoamf);
  
  -- Multi-driven assignments
  z <= ('L', 'Z', 'X', 'Z', 'H');
  z <= ('H', '1', 'U', '1', 'L');
  z <= ('L', '0', 'Z', '-', 'U');
end miod;



-- Seed after: 12556490549306370266,12011142928354116943
