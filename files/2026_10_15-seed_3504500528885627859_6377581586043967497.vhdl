-- Seed: 3504500528885627859,6377581586043967497

entity keelgt is
  port (hqrbvhacs : out severity_level; u : in real; g : in boolean_vector(0 downto 0));
end keelgt;

architecture ojtpretoko of keelgt is
  
begin
  -- Single-driven assignments
  hqrbvhacs <= WARNING;
end ojtpretoko;

library ieee;
use ieee.std_logic_1164.all;

entity dr is
  port (bjvqs : inout std_logic; tqn : in std_logic_vector(1 downto 3); ciwleauyo : inout severity_level; yv : out std_logic);
end dr;

architecture liluqdzave of dr is
  signal mc : real;
  signal dvhkunmpn : severity_level;
  signal jwl : real;
  signal ivsj : severity_level;
  signal ey : boolean_vector(0 downto 0);
  signal qc : real;
  signal pwxv : severity_level;
begin
  ooufua : entity work.keelgt
    port map (hqrbvhacs => pwxv, u => qc, g => ey);
  cataum : entity work.keelgt
    port map (hqrbvhacs => ivsj, u => jwl, g => ey);
  olafbrwf : entity work.keelgt
    port map (hqrbvhacs => dvhkunmpn, u => qc, g => ey);
  yf : entity work.keelgt
    port map (hqrbvhacs => ciwleauyo, u => mc, g => ey);
  
  -- Single-driven assignments
  jwl <= qc;
  
  -- Multi-driven assignments
  yv <= 'Z';
end liluqdzave;



-- Seed after: 17248602423964250604,6377581586043967497
