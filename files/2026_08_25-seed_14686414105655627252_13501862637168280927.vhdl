-- Seed: 14686414105655627252,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity edcng is
  port (byvmks : in time; c : inout time; lq : inout std_logic; d : inout time);
end edcng;

architecture wkohma of edcng is
  
begin
  -- Single-driven assignments
  c <= 16#CD# fs;
  d <= 8#1_7_6_6_4# ms;
  
  -- Multi-driven assignments
  lq <= '-';
  lq <= '0';
  lq <= lq;
  lq <= lq;
end wkohma;

library ieee;
use ieee.std_logic_1164.all;

entity aetydllcf is
  port (tuaeaecpa : in time; umhatrc : linkage std_logic_vector(4 downto 0); uuyg : out integer);
end aetydllcf;

library ieee;
use ieee.std_logic_1164.all;

architecture mwgyo of aetydllcf is
  signal uxpbwlu : std_logic;
  signal rutiun : time;
  signal gkkjhefuh : time;
begin
  yuu : entity work.edcng
    port map (byvmks => gkkjhefuh, c => rutiun, lq => uxpbwlu, d => gkkjhefuh);
  
  -- Single-driven assignments
  uuyg <= uuyg;
  
  -- Multi-driven assignments
  uxpbwlu <= 'U';
end mwgyo;

entity ehvsdptwkb is
  port (ovdbgxlg : in real; rmt : buffer integer; tm : out real; jgxtbklcw : out integer);
end ehvsdptwkb;

library ieee;
use ieee.std_logic_1164.all;

architecture ephmsy of ehvsdptwkb is
  signal o : time;
  signal bkkbceuez : std_logic;
  signal b : time;
  signal d : time;
begin
  gd : entity work.edcng
    port map (byvmks => d, c => b, lq => bkkbceuez, d => o);
  
  -- Multi-driven assignments
  bkkbceuez <= 'H';
  bkkbceuez <= '1';
end ephmsy;



-- Seed after: 13708557894422959113,13501862637168280927
