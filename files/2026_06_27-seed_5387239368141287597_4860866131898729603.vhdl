-- Seed: 5387239368141287597,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity kdcdeya is
  port (ffcguyxj : in bit_vector(0 downto 3); wfh : linkage character; vas : out std_logic_vector(1 downto 1); dynunfoj : out time);
end kdcdeya;

architecture mu of kdcdeya is
  
begin
  -- Single-driven assignments
  dynunfoj <= 1 hr;
  
  -- Multi-driven assignments
  vas <= (others => 'X');
  vas <= "L";
  vas <= "-";
end mu;

entity cm is
  port (cpdins : buffer boolean);
end cm;

library ieee;
use ieee.std_logic_1164.all;

architecture ziu of cm is
  signal t : time;
  signal oisu : std_logic_vector(1 downto 1);
  signal moauf : character;
  signal lnfheovyde : bit_vector(0 downto 3);
  signal wo : time;
  signal qddt : std_logic_vector(1 downto 1);
  signal jrspzr : character;
  signal di : bit_vector(0 downto 3);
begin
  svnldp : entity work.kdcdeya
    port map (ffcguyxj => di, wfh => jrspzr, vas => qddt, dynunfoj => wo);
  adlxtckjv : entity work.kdcdeya
    port map (ffcguyxj => lnfheovyde, wfh => moauf, vas => oisu, dynunfoj => t);
  
  -- Multi-driven assignments
  qddt <= "H";
  oisu <= (others => 'L');
  qddt <= (others => 'H');
end ziu;



-- Seed after: 15214300986877338031,4860866131898729603
