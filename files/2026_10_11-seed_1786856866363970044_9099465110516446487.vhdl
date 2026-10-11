-- Seed: 1786856866363970044,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity bjccam is
  port (ivxeqjf : in boolean; ejdcjq : buffer time; zxdtipvjjm : buffer std_logic_vector(3 downto 4); mjv : linkage integer);
end bjccam;

architecture zrmrnuhpmy of bjccam is
  
begin
  -- Single-driven assignments
  ejdcjq <= 8#1_7_6_0.30545# fs;
  
  -- Multi-driven assignments
  zxdtipvjjm <= zxdtipvjjm;
  zxdtipvjjm <= zxdtipvjjm;
end zrmrnuhpmy;

entity e is
  port (pjxtao : linkage time; fx : out integer);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture zf of e is
  signal prmvuiv : integer;
  signal acclsy : time;
  signal koti : boolean;
  signal afykzbmf : std_logic_vector(3 downto 4);
  signal id : time;
  signal hrv : integer;
  signal bujsyhlvos : std_logic_vector(3 downto 4);
  signal egmqrvg : time;
  signal qgzheqm : boolean;
begin
  dnypupe : entity work.bjccam
    port map (ivxeqjf => qgzheqm, ejdcjq => egmqrvg, zxdtipvjjm => bujsyhlvos, mjv => hrv);
  rx : entity work.bjccam
    port map (ivxeqjf => qgzheqm, ejdcjq => id, zxdtipvjjm => afykzbmf, mjv => fx);
  ssz : entity work.bjccam
    port map (ivxeqjf => koti, ejdcjq => acclsy, zxdtipvjjm => bujsyhlvos, mjv => prmvuiv);
  
  -- Single-driven assignments
  koti <= qgzheqm;
  qgzheqm <= FALSE;
  
  -- Multi-driven assignments
  bujsyhlvos <= (others => '0');
end zf;



-- Seed after: 18131199795952881288,9099465110516446487
