-- Seed: 2741337921042842275,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity rv is
  port (oaly : buffer std_logic; njcrgqlyx : in std_logic_vector(1 downto 2); wj : buffer bit_vector(1 downto 2); f : in integer);
end rv;

architecture wkdal of rv is
  
begin
  -- Single-driven assignments
  wj <= (others => '0');
  
  -- Multi-driven assignments
  oaly <= 'W';
  oaly <= 'Z';
end wkdal;

library ieee;
use ieee.std_logic_1164.all;

entity xmg is
  port (muek : buffer std_logic_vector(2 downto 4));
end xmg;

library ieee;
use ieee.std_logic_1164.all;

architecture evoit of xmg is
  signal gppjlet : integer;
  signal efobpujon : bit_vector(1 downto 2);
  signal kpxkm : std_logic;
  signal r : integer;
  signal wblwycemc : bit_vector(1 downto 2);
  signal ryb : std_logic_vector(1 downto 2);
  signal kddjplhov : std_logic;
begin
  lcpri : entity work.rv
    port map (oaly => kddjplhov, njcrgqlyx => ryb, wj => wblwycemc, f => r);
  fvfunlv : entity work.rv
    port map (oaly => kpxkm, njcrgqlyx => muek, wj => efobpujon, f => gppjlet);
  
  -- Single-driven assignments
  r <= 8#6_6#;
  gppjlet <= 8#6_3_1_5#;
  
  -- Multi-driven assignments
  muek <= (others => '0');
  muek <= "";
  muek <= "";
  muek <= "";
end evoit;



-- Seed after: 1696999520282017503,3687118713772291287
