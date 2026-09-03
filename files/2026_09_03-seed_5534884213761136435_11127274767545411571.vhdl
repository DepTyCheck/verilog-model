-- Seed: 5534884213761136435,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity odwkkcdld is
  port (nz : buffer std_logic; xfd : in time; dfpanpljxm : out std_logic);
end odwkkcdld;

architecture zqwcaqqd of odwkkcdld is
  
begin
  
end zqwcaqqd;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (mziccwy : buffer std_logic);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture atm of g is
  signal dwggsp : std_logic;
  signal hntqhek : std_logic;
  signal byy : time;
  signal zv : std_logic;
begin
  f : entity work.odwkkcdld
    port map (nz => zv, xfd => byy, dfpanpljxm => mziccwy);
  wudvul : entity work.odwkkcdld
    port map (nz => zv, xfd => byy, dfpanpljxm => hntqhek);
  lafdrvk : entity work.odwkkcdld
    port map (nz => dwggsp, xfd => byy, dfpanpljxm => hntqhek);
end atm;

library ieee;
use ieee.std_logic_1164.all;

entity edtjoj is
  port (gatltp : in severity_level; r : buffer integer; oauja : buffer integer; rvdjicjjyx : linkage std_logic_vector(0 to 1));
end edtjoj;

library ieee;
use ieee.std_logic_1164.all;

architecture sgxd of edtjoj is
  signal flphzxuznm : time;
  signal djbpzujalt : std_logic;
begin
  yixzar : entity work.g
    port map (mziccwy => djbpzujalt);
  tqbiyb : entity work.odwkkcdld
    port map (nz => djbpzujalt, xfd => flphzxuznm, dfpanpljxm => djbpzujalt);
  
  -- Single-driven assignments
  oauja <= oauja;
  flphzxuznm <= flphzxuznm;
  
  -- Multi-driven assignments
  djbpzujalt <= djbpzujalt;
end sgxd;



-- Seed after: 10787435777694452008,11127274767545411571
