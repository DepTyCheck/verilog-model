-- Seed: 8274861325724428136,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity bcbpv is
  port (mypdeyy : out boolean_vector(3 downto 3); oz : in std_logic_vector(4 downto 4); dag : out time);
end bcbpv;

architecture fejch of bcbpv is
  
begin
  -- Single-driven assignments
  dag <= dag;
  mypdeyy <= mypdeyy;
end fejch;

entity knehtdb is
  port (m : out time);
end knehtdb;

library ieee;
use ieee.std_logic_1164.all;

architecture gm of knehtdb is
  signal bitnht : time;
  signal ymzfev : boolean_vector(3 downto 3);
  signal oqxagcuov : std_logic_vector(4 downto 4);
  signal ytwmnwjtb : boolean_vector(3 downto 3);
  signal ah : time;
  signal zfr : boolean_vector(3 downto 3);
  signal cgjlsnkxq : time;
  signal dx : std_logic_vector(4 downto 4);
  signal qaha : boolean_vector(3 downto 3);
begin
  chafwzbi : entity work.bcbpv
    port map (mypdeyy => qaha, oz => dx, dag => cgjlsnkxq);
  zxhmun : entity work.bcbpv
    port map (mypdeyy => zfr, oz => dx, dag => ah);
  vqpbup : entity work.bcbpv
    port map (mypdeyy => ytwmnwjtb, oz => oqxagcuov, dag => m);
  lrqnig : entity work.bcbpv
    port map (mypdeyy => ymzfev, oz => dx, dag => bitnht);
end gm;



-- Seed after: 4884615635280449036,4177195558088809003
