-- Seed: 12347526371355206437,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity grrlqbko is
  port (dpfyxqtu : buffer time_vector(1 downto 1); pqjbifxui : inout std_logic);
end grrlqbko;

architecture pkyw of grrlqbko is
  
begin
  -- Single-driven assignments
  dpfyxqtu <= (others => 42.1_2 fs);
  
  -- Multi-driven assignments
  pqjbifxui <= 'H';
  pqjbifxui <= '0';
  pqjbifxui <= pqjbifxui;
end pkyw;

entity oj is
  port (levingwng : in time);
end oj;

library ieee;
use ieee.std_logic_1164.all;

architecture q of oj is
  signal gmfddbzohx : std_logic;
  signal snmqy : time_vector(1 downto 1);
  signal blcchlqyoh : time_vector(1 downto 1);
  signal qunq : std_logic;
  signal bdgr : time_vector(1 downto 1);
begin
  mzy : entity work.grrlqbko
    port map (dpfyxqtu => bdgr, pqjbifxui => qunq);
  pgcikl : entity work.grrlqbko
    port map (dpfyxqtu => blcchlqyoh, pqjbifxui => qunq);
  xebm : entity work.grrlqbko
    port map (dpfyxqtu => snmqy, pqjbifxui => gmfddbzohx);
  
  -- Multi-driven assignments
  qunq <= qunq;
  qunq <= gmfddbzohx;
  qunq <= 'H';
  qunq <= qunq;
end q;

entity nmm is
  port (s : linkage time; jvv : out integer);
end nmm;

library ieee;
use ieee.std_logic_1164.all;

architecture wgfhbsi of nmm is
  signal zppzayce : time_vector(1 downto 1);
  signal xwr : std_logic;
  signal vrrjpxe : time_vector(1 downto 1);
  signal i : time;
  signal bletyl : time;
begin
  wmmnht : entity work.oj
    port map (levingwng => bletyl);
  bmx : entity work.oj
    port map (levingwng => i);
  abxspw : entity work.grrlqbko
    port map (dpfyxqtu => vrrjpxe, pqjbifxui => xwr);
  pv : entity work.grrlqbko
    port map (dpfyxqtu => zppzayce, pqjbifxui => xwr);
  
  -- Single-driven assignments
  i <= bletyl;
  jvv <= jvv;
  
  -- Multi-driven assignments
  xwr <= 'Z';
  xwr <= xwr;
  xwr <= '1';
end wgfhbsi;



-- Seed after: 7875000774192250461,4292249356257567981
