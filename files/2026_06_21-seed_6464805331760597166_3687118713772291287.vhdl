-- Seed: 6464805331760597166,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity duoxi is
  port (h : linkage character; dmronr : buffer std_logic; jwhqyr : in std_logic_vector(4 downto 2));
end duoxi;

architecture ame of duoxi is
  
begin
  -- Multi-driven assignments
  dmronr <= 'X';
  dmronr <= 'X';
  dmronr <= 'L';
end ame;

library ieee;
use ieee.std_logic_1164.all;

entity smecgm is
  port (eevfy : in time; hfaskcjpgx : inout std_logic; md : inout severity_level);
end smecgm;

library ieee;
use ieee.std_logic_1164.all;

architecture mmw of smecgm is
  signal u : character;
  signal rnzmiwrxqs : std_logic_vector(4 downto 2);
  signal ohv : std_logic;
  signal uymeutrh : character;
begin
  nl : entity work.duoxi
    port map (h => uymeutrh, dmronr => ohv, jwhqyr => rnzmiwrxqs);
  g : entity work.duoxi
    port map (h => u, dmronr => hfaskcjpgx, jwhqyr => rnzmiwrxqs);
  
  -- Single-driven assignments
  md <= FAILURE;
end mmw;

library ieee;
use ieee.std_logic_1164.all;

entity eufjnh is
  port (g : out character; qjsyot : in integer_vector(0 to 0); gqmda : buffer std_logic);
end eufjnh;

library ieee;
use ieee.std_logic_1164.all;

architecture kvwq of eufjnh is
  signal fwesr : std_logic_vector(4 downto 2);
  signal dtet : character;
  signal an : std_logic_vector(4 downto 2);
  signal twuerlof : std_logic;
  signal q : character;
  signal bca : std_logic_vector(4 downto 2);
  signal xr : character;
begin
  ldkepuqcjw : entity work.duoxi
    port map (h => xr, dmronr => gqmda, jwhqyr => bca);
  antzkf : entity work.duoxi
    port map (h => q, dmronr => twuerlof, jwhqyr => an);
  eccmtuhjvu : entity work.duoxi
    port map (h => dtet, dmronr => gqmda, jwhqyr => fwesr);
  a : entity work.duoxi
    port map (h => g, dmronr => gqmda, jwhqyr => fwesr);
  
  -- Multi-driven assignments
  gqmda <= 'L';
  an <= "XXH";
end kvwq;



-- Seed after: 6925785085492621829,3687118713772291287
