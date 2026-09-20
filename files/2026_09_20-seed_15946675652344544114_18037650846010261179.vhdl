-- Seed: 15946675652344544114,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity mswfsk is
  port (z : out std_logic; ywwxylq : in std_logic_vector(2 downto 0));
end mswfsk;

architecture hnr of mswfsk is
  
begin
  -- Multi-driven assignments
  z <= z;
  z <= 'W';
  z <= z;
end hnr;

entity mexpliqo is
  port (szqqfqz : buffer boolean);
end mexpliqo;

library ieee;
use ieee.std_logic_1164.all;

architecture kxjr of mexpliqo is
  signal wdztlbxs : std_logic;
  signal pzqr : std_logic_vector(2 downto 0);
  signal j : std_logic;
  signal udqoapr : std_logic_vector(2 downto 0);
  signal clcuahjo : std_logic;
begin
  l : entity work.mswfsk
    port map (z => clcuahjo, ywwxylq => udqoapr);
  b : entity work.mswfsk
    port map (z => j, ywwxylq => pzqr);
  wbbl : entity work.mswfsk
    port map (z => wdztlbxs, ywwxylq => udqoapr);
  
  -- Single-driven assignments
  szqqfqz <= FALSE;
  
  -- Multi-driven assignments
  udqoapr <= "XZ-";
  j <= 'W';
  clcuahjo <= 'X';
  wdztlbxs <= 'Z';
end kxjr;

entity wkeo is
  port (csuaztgih : inout time);
end wkeo;

library ieee;
use ieee.std_logic_1164.all;

architecture kffrelgmv of wkeo is
  signal gkh : std_logic_vector(2 downto 0);
  signal bnnxv : std_logic;
  signal ss : boolean;
  signal iwhf : boolean;
begin
  cyzoxsk : entity work.mexpliqo
    port map (szqqfqz => iwhf);
  hsvo : entity work.mexpliqo
    port map (szqqfqz => ss);
  zdbnbjkcv : entity work.mswfsk
    port map (z => bnnxv, ywwxylq => gkh);
  amcandwgn : entity work.mswfsk
    port map (z => bnnxv, ywwxylq => gkh);
  
  -- Single-driven assignments
  csuaztgih <= csuaztgih;
  
  -- Multi-driven assignments
  bnnxv <= '-';
  bnnxv <= bnnxv;
  bnnxv <= 'H';
end kffrelgmv;



-- Seed after: 1675739146353511107,18037650846010261179
