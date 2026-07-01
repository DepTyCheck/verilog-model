-- Seed: 13348055715456362532,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity qwiwato is
  port (maiyzmdj : linkage std_logic_vector(1 downto 4); p : in std_logic_vector(1 to 3));
end qwiwato;

architecture nrfkfb of qwiwato is
  
begin
  
end nrfkfb;

entity dshvejydcw is
  port (vyzdytiuo : linkage time; kiy : in time);
end dshvejydcw;

library ieee;
use ieee.std_logic_1164.all;

architecture yfdkj of dshvejydcw is
  signal fpv : std_logic_vector(1 to 3);
  signal oossd : std_logic_vector(1 downto 4);
begin
  chm : entity work.qwiwato
    port map (maiyzmdj => oossd, p => fpv);
end yfdkj;

entity pwjcmr is
  port (smmu : buffer character; emirdw : in integer);
end pwjcmr;

library ieee;
use ieee.std_logic_1164.all;

architecture umtfnxhkxu of pwjcmr is
  signal igedxqjov : std_logic_vector(1 to 3);
  signal ejklv : std_logic_vector(1 downto 4);
  signal nlnlqllz : std_logic_vector(1 to 3);
  signal ociride : std_logic_vector(1 to 3);
  signal zugfvuxrc : std_logic_vector(1 downto 4);
begin
  dsntkn : entity work.qwiwato
    port map (maiyzmdj => zugfvuxrc, p => ociride);
  nsvlbdkrft : entity work.qwiwato
    port map (maiyzmdj => zugfvuxrc, p => nlnlqllz);
  nlwqan : entity work.qwiwato
    port map (maiyzmdj => ejklv, p => igedxqjov);
  
  -- Single-driven assignments
  smmu <= 'f';
  
  -- Multi-driven assignments
  zugfvuxrc <= "";
  igedxqjov <= "1WL";
  igedxqjov <= ('U', '1', 'H');
  zugfvuxrc <= "";
end umtfnxhkxu;



-- Seed after: 792499625946447691,6882842853887419669
