-- Seed: 4456800375449416570,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity oyujb is
  port (enivg : out std_logic; ekhmletvvw : buffer integer; pxj : in boolean);
end oyujb;

architecture ee of oyujb is
  
begin
  -- Multi-driven assignments
  enivg <= 'X';
  enivg <= 'W';
end ee;

entity s is
  port (izkvgmu : inout time_vector(4 downto 2); vrsdkaxzgp : in time; ehcllcya : inout severity_level);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture idzaldawl of s is
  signal wnvgzlvnbp : boolean;
  signal xpogep : integer;
  signal rlevzryn : std_logic;
  signal anxzuz : boolean;
  signal wlboaizm : integer;
  signal ktupzyf : std_logic;
begin
  k : entity work.oyujb
    port map (enivg => ktupzyf, ekhmletvvw => wlboaizm, pxj => anxzuz);
  qglawtswb : entity work.oyujb
    port map (enivg => rlevzryn, ekhmletvvw => xpogep, pxj => wnvgzlvnbp);
  
  -- Single-driven assignments
  izkvgmu <= (1 hr, 2#0.10110# fs, 2#000# ms);
  wnvgzlvnbp <= FALSE;
  anxzuz <= FALSE;
  ehcllcya <= FAILURE;
end idzaldawl;



-- Seed after: 14038893094923299291,3687118713772291287
