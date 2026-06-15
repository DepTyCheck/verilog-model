-- Seed: 6498017501351144210,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity usz is
  port (zistgiix : in time; nmhjbeud : buffer std_logic; cavhnhdi : out time);
end usz;

architecture vuerhz of usz is
  
begin
  -- Single-driven assignments
  cavhnhdi <= 2_0_1 ns;
  
  -- Multi-driven assignments
  nmhjbeud <= 'H';
  nmhjbeud <= '0';
  nmhjbeud <= '0';
  nmhjbeud <= '1';
end vuerhz;

library ieee;
use ieee.std_logic_1164.all;

entity ccynvao is
  port (h : out severity_level; j : buffer integer; tcufz : linkage std_logic);
end ccynvao;

architecture ihhryjnnd of ccynvao is
  
begin
  -- Single-driven assignments
  j <= 2#0_0_0_0#;
  h <= ERROR;
end ihhryjnnd;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (unej : in std_logic_vector(3 to 0); nqbuyw : out time; kghbhgvnp : in std_logic; spu : buffer integer);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture alpuv of j is
  signal ywtjfyz : integer;
  signal jri : severity_level;
  signal yvrv : time;
  signal qraiwaj : std_logic;
  signal wvzyeegws : integer;
  signal ajhsd : severity_level;
  signal hfswazsd : std_logic;
begin
  x : entity work.usz
    port map (zistgiix => nqbuyw, nmhjbeud => hfswazsd, cavhnhdi => nqbuyw);
  nm : entity work.ccynvao
    port map (h => ajhsd, j => wvzyeegws, tcufz => kghbhgvnp);
  hmfymmxs : entity work.usz
    port map (zistgiix => nqbuyw, nmhjbeud => qraiwaj, cavhnhdi => yvrv);
  tc : entity work.ccynvao
    port map (h => jri, j => ywtjfyz, tcufz => kghbhgvnp);
  
  -- Single-driven assignments
  spu <= 2_3_4_1_1;
  
  -- Multi-driven assignments
  hfswazsd <= 'H';
  hfswazsd <= '-';
end alpuv;



-- Seed after: 5257829577185962556,1834764876137802293
