-- Seed: 2612415016188898299,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity hskoiisw is
  port (pwy : buffer std_logic_vector(1 downto 2));
end hskoiisw;

architecture hk of hskoiisw is
  
begin
  
end hk;

library ieee;
use ieee.std_logic_1164.all;

entity mwpufditvs is
  port (uwb : in boolean; mqlrkdup : out string(1 to 2); qoi : out std_logic);
end mwpufditvs;

library ieee;
use ieee.std_logic_1164.all;

architecture ugaxvk of mwpufditvs is
  signal lkszbswcxh : std_logic_vector(1 downto 2);
  signal dclp : std_logic_vector(1 downto 2);
  signal cnm : std_logic_vector(1 downto 2);
begin
  tiatucpccr : entity work.hskoiisw
    port map (pwy => cnm);
  mrpuh : entity work.hskoiisw
    port map (pwy => cnm);
  bffnulvz : entity work.hskoiisw
    port map (pwy => dclp);
  ydy : entity work.hskoiisw
    port map (pwy => lkszbswcxh);
  
  -- Single-driven assignments
  mqlrkdup <= "yz";
end ugaxvk;



-- Seed after: 17676055849213944031,12011142928354116943
