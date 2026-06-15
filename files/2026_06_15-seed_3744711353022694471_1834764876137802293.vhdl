-- Seed: 3744711353022694471,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity tddstvciat is
  port (cqvvxp : out real; hi : in bit; pvrecxrvw : inout std_logic; jht : in std_logic);
end tddstvciat;

architecture i of tddstvciat is
  
begin
  -- Single-driven assignments
  cqvvxp <= 2#1_1.11#;
end i;

library ieee;
use ieee.std_logic_1164.all;

entity qxdpkoijr is
  port (ystcsdaigw : in std_logic; fnuk : out integer; pdn : inout integer; wvmf : buffer integer);
end qxdpkoijr;

library ieee;
use ieee.std_logic_1164.all;

architecture xtigug of qxdpkoijr is
  signal prz : bit;
  signal az : real;
  signal sceamtbjf : real;
  signal wdlknurkh : std_logic;
  signal y : bit;
  signal xzjf : real;
begin
  orooxogs : entity work.tddstvciat
    port map (cqvvxp => xzjf, hi => y, pvrecxrvw => wdlknurkh, jht => ystcsdaigw);
  pcounqwh : entity work.tddstvciat
    port map (cqvvxp => sceamtbjf, hi => y, pvrecxrvw => wdlknurkh, jht => wdlknurkh);
  rh : entity work.tddstvciat
    port map (cqvvxp => az, hi => prz, pvrecxrvw => wdlknurkh, jht => ystcsdaigw);
  
  -- Single-driven assignments
  y <= '1';
  fnuk <= 2#01#;
  prz <= '0';
  
  -- Multi-driven assignments
  wdlknurkh <= 'W';
  wdlknurkh <= '0';
  wdlknurkh <= 'X';
end xtigug;



-- Seed after: 1126198644243240825,1834764876137802293
