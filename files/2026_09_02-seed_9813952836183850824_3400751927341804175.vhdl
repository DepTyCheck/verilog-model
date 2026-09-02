-- Seed: 9813952836183850824,3400751927341804175

entity azp is
  port (xbxqhx : out real);
end azp;

architecture z of azp is
  
begin
  
end z;

library ieee;
use ieee.std_logic_1164.all;

entity uhzkj is
  port ( erlh : inout severity_level
  ; rnaqwfnmp : in time_vector(3 downto 3)
  ; lzrtzend : in std_logic_vector(4 to 0)
  ; qgpsbatsl : inout std_logic_vector(3 to 0)
  );
end uhzkj;

architecture bqulbtje of uhzkj is
  signal vejuimxn : real;
begin
  scyayu : entity work.azp
    port map (xbxqhx => vejuimxn);
end bqulbtje;

library ieee;
use ieee.std_logic_1164.all;

entity hf is
  port (oeb : in time; mu : linkage std_logic_vector(1 to 0));
end hf;

architecture ptfeubk of hf is
  signal azhsu : real;
begin
  gaonfiq : entity work.azp
    port map (xbxqhx => azhsu);
end ptfeubk;

library ieee;
use ieee.std_logic_1164.all;

entity woeozd is
  port (gyepjaiy : buffer real; gpnfhqgl : buffer std_logic_vector(3 to 1));
end woeozd;

architecture izdiawfz of woeozd is
  signal yzug : time;
begin
  fvlxrtco : entity work.azp
    port map (xbxqhx => gyepjaiy);
  pnewn : entity work.hf
    port map (oeb => yzug, mu => gpnfhqgl);
  
  -- Single-driven assignments
  yzug <= yzug;
  
  -- Multi-driven assignments
  gpnfhqgl <= (others => '0');
  gpnfhqgl <= gpnfhqgl;
  gpnfhqgl <= gpnfhqgl;
end izdiawfz;



-- Seed after: 286831930625398151,3400751927341804175
