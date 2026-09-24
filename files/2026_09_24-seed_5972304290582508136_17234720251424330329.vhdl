-- Seed: 5972304290582508136,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity bmdcdmun is
  port (lprf : inout std_logic_vector(0 downto 3); hhclcnwb : linkage time; utebtslsc : linkage std_logic_vector(4 downto 3); qdy : buffer std_logic);
end bmdcdmun;

architecture zavng of bmdcdmun is
  
begin
  -- Multi-driven assignments
  lprf <= (others => '0');
  qdy <= '1';
  qdy <= '0';
  lprf <= "";
end zavng;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (r : in std_logic_vector(1 downto 4); jppf : in integer);
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture bipnc of z is
  signal cqy : std_logic;
  signal nruznst : std_logic_vector(4 downto 3);
  signal pupfwuxkkk : time;
  signal iyrraxs : std_logic;
  signal jtvf : std_logic_vector(4 downto 3);
  signal wnw : time;
  signal dfwggltucc : std_logic_vector(0 downto 3);
begin
  xvk : entity work.bmdcdmun
    port map (lprf => dfwggltucc, hhclcnwb => wnw, utebtslsc => jtvf, qdy => iyrraxs);
  cvp : entity work.bmdcdmun
    port map (lprf => dfwggltucc, hhclcnwb => pupfwuxkkk, utebtslsc => nruznst, qdy => cqy);
end bipnc;



-- Seed after: 17057146758087716342,17234720251424330329
