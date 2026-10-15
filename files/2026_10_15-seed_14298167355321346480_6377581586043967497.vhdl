-- Seed: 14298167355321346480,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity ln is
  port (typld : in time; ntiwwogp : buffer std_logic_vector(1 downto 3));
end ln;

architecture rn of ln is
  
begin
  -- Multi-driven assignments
  ntiwwogp <= (others => '0');
  ntiwwogp <= "";
end rn;

library ieee;
use ieee.std_logic_1164.all;

entity fij is
  port (gnbjavzik : in std_logic; ybm : buffer std_logic; phydzlk : inout std_logic_vector(1 downto 3); hklfxnbrvf : out time);
end fij;

architecture xsjkfyx of fij is
  signal fwditiyrel : time;
begin
  zhfbxx : entity work.ln
    port map (typld => fwditiyrel, ntiwwogp => phydzlk);
  ncbyl : entity work.ln
    port map (typld => hklfxnbrvf, ntiwwogp => phydzlk);
  t : entity work.ln
    port map (typld => hklfxnbrvf, ntiwwogp => phydzlk);
  
  -- Single-driven assignments
  hklfxnbrvf <= hklfxnbrvf;
  fwditiyrel <= hklfxnbrvf;
  
  -- Multi-driven assignments
  phydzlk <= "";
  phydzlk <= "";
  phydzlk <= phydzlk;
  phydzlk <= phydzlk;
end xsjkfyx;

entity goomezlbhc is
  port (sjzk : in integer);
end goomezlbhc;

library ieee;
use ieee.std_logic_1164.all;

architecture e of goomezlbhc is
  signal hbsb : std_logic;
  signal z : std_logic;
  signal xn : std_logic_vector(1 downto 3);
  signal wsfzbf : time;
  signal efkt : std_logic_vector(1 downto 3);
  signal ashitt : time;
begin
  bhgkfcs : entity work.ln
    port map (typld => ashitt, ntiwwogp => efkt);
  d : entity work.ln
    port map (typld => wsfzbf, ntiwwogp => efkt);
  dlfqqxv : entity work.ln
    port map (typld => wsfzbf, ntiwwogp => xn);
  vi : entity work.fij
    port map (gnbjavzik => z, ybm => hbsb, phydzlk => efkt, hklfxnbrvf => ashitt);
  
  -- Single-driven assignments
  wsfzbf <= 2#0.0_1_1_0# fs;
  
  -- Multi-driven assignments
  efkt <= (others => '0');
  efkt <= xn;
  z <= z;
end e;



-- Seed after: 14026343663662114506,6377581586043967497
