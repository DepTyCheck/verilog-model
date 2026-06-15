-- Seed: 5549870819679740656,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity xpn is
  port (psufwv : buffer time; gexkirgh : linkage integer_vector(3 downto 2); btakaaonv : in std_logic_vector(4 to 3); l : out std_logic);
end xpn;

architecture znjxtfc of xpn is
  
begin
  -- Single-driven assignments
  psufwv <= 0_3_4_4 us;
end znjxtfc;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (xlqlmu : linkage bit; xxr : linkage std_logic_vector(2 to 3));
end b;

architecture ghkhynnlb of b is
  
begin
  
end ghkhynnlb;

library ieee;
use ieee.std_logic_1164.all;

entity xxybfuyx is
  port (kmdmnscat : out std_logic_vector(3 downto 1));
end xxybfuyx;

library ieee;
use ieee.std_logic_1164.all;

architecture vz of xxybfuyx is
  signal mlsrn : bit;
  signal itszxvri : std_logic;
  signal h : std_logic_vector(4 to 3);
  signal jvm : integer_vector(3 downto 2);
  signal swmwn : time;
  signal xmv : std_logic_vector(2 to 3);
  signal wvdb : bit;
  signal mqfvilfom : std_logic;
  signal fhyf : std_logic_vector(4 to 3);
  signal xikplcs : integer_vector(3 downto 2);
  signal wgnz : time;
begin
  rof : entity work.xpn
    port map (psufwv => wgnz, gexkirgh => xikplcs, btakaaonv => fhyf, l => mqfvilfom);
  u : entity work.b
    port map (xlqlmu => wvdb, xxr => xmv);
  ozqrhh : entity work.xpn
    port map (psufwv => swmwn, gexkirgh => jvm, btakaaonv => h, l => itszxvri);
  c : entity work.b
    port map (xlqlmu => mlsrn, xxr => xmv);
  
  -- Multi-driven assignments
  fhyf <= (others => '0');
end vz;



-- Seed after: 17450322965473761111,1834764876137802293
