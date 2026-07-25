-- Seed: 12008478045610240220,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity voi is
  port (pmnicz : in boolean_vector(2 to 2); pce : out real; grecbzag : in std_logic_vector(2 to 2));
end voi;

architecture o of voi is
  
begin
  -- Single-driven assignments
  pce <= pce;
end o;

entity dbgetc is
  port (povx : out integer);
end dbgetc;

library ieee;
use ieee.std_logic_1164.all;

architecture zubs of dbgetc is
  signal hpddzj : std_logic_vector(2 to 2);
  signal ryitilyx : real;
  signal rwkuwgqh : boolean_vector(2 to 2);
begin
  qbwg : entity work.voi
    port map (pmnicz => rwkuwgqh, pce => ryitilyx, grecbzag => hpddzj);
  
  -- Single-driven assignments
  povx <= 2;
  
  -- Multi-driven assignments
  hpddzj <= "Z";
end zubs;

library ieee;
use ieee.std_logic_1164.all;

entity aedm is
  port (azfexn : linkage real; ruqbulej : in std_logic_vector(1 downto 1));
end aedm;

library ieee;
use ieee.std_logic_1164.all;

architecture nx of aedm is
  signal rj : real;
  signal hgkyqch : boolean_vector(2 to 2);
  signal hraxpzp : std_logic_vector(2 to 2);
  signal wqjn : real;
  signal ftfagw : std_logic_vector(2 to 2);
  signal exw : real;
  signal jgit : boolean_vector(2 to 2);
begin
  cmqwf : entity work.voi
    port map (pmnicz => jgit, pce => exw, grecbzag => ftfagw);
  yhnr : entity work.voi
    port map (pmnicz => jgit, pce => wqjn, grecbzag => hraxpzp);
  iq : entity work.voi
    port map (pmnicz => hgkyqch, pce => rj, grecbzag => ruqbulej);
  
  -- Single-driven assignments
  jgit <= jgit;
  hgkyqch <= jgit;
  
  -- Multi-driven assignments
  ftfagw <= (others => 'W');
  ftfagw <= (others => 'L');
  ftfagw <= (others => 'U');
  ftfagw <= hraxpzp;
end nx;



-- Seed after: 7206491613468664605,5306691039457971049
