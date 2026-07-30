-- Seed: 1272882563185722880,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity ffrkwb is
  port (lqco : in std_logic_vector(3 to 3); tdwrdahaie : out std_logic);
end ffrkwb;

architecture cogvbozwjc of ffrkwb is
  
begin
  
end cogvbozwjc;

library ieee;
use ieee.std_logic_1164.all;

entity liqqofv is
  port (aqbiaea : buffer std_logic; tnmrlgiatd : buffer std_logic; yrsgalp : buffer integer; fpdkpeq : in std_logic);
end liqqofv;

library ieee;
use ieee.std_logic_1164.all;

architecture ewwemrvnzn of liqqofv is
  signal qdch : std_logic;
  signal lqp : std_logic_vector(3 to 3);
  signal ynyubrvgba : std_logic;
  signal ayuw : std_logic_vector(3 to 3);
  signal ivpa : std_logic_vector(3 to 3);
begin
  z : entity work.ffrkwb
    port map (lqco => ivpa, tdwrdahaie => aqbiaea);
  sijqawwzmz : entity work.ffrkwb
    port map (lqco => ayuw, tdwrdahaie => ynyubrvgba);
  saq : entity work.ffrkwb
    port map (lqco => lqp, tdwrdahaie => qdch);
  
  -- Single-driven assignments
  yrsgalp <= yrsgalp;
  
  -- Multi-driven assignments
  ivpa <= ivpa;
  tnmrlgiatd <= '-';
  ivpa <= (others => '-');
  aqbiaea <= aqbiaea;
end ewwemrvnzn;

library ieee;
use ieee.std_logic_1164.all;

entity enaf is
  port (nkuupswyu : in std_logic_vector(3 to 0));
end enaf;

library ieee;
use ieee.std_logic_1164.all;

architecture rfk of enaf is
  signal snhn : std_logic;
  signal blhjz : std_logic_vector(3 to 3);
  signal tcs : std_logic;
  signal kluartds : std_logic_vector(3 to 3);
begin
  byygscp : entity work.ffrkwb
    port map (lqco => kluartds, tdwrdahaie => tcs);
  nwnfcdzan : entity work.ffrkwb
    port map (lqco => kluartds, tdwrdahaie => tcs);
  zwg : entity work.ffrkwb
    port map (lqco => blhjz, tdwrdahaie => snhn);
  svdqmofhbq : entity work.ffrkwb
    port map (lqco => kluartds, tdwrdahaie => tcs);
  
  -- Multi-driven assignments
  tcs <= 'Z';
  tcs <= 'W';
  kluartds <= "Z";
  snhn <= tcs;
end rfk;



-- Seed after: 13267671950064200426,4122021602305298647
