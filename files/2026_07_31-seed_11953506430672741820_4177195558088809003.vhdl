-- Seed: 11953506430672741820,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity kp is
  port (vkuohenc : out time; uqa : in std_logic; jqiulpnuyw : in std_logic_vector(0 to 3); wplr : in std_logic_vector(1 to 1));
end kp;

architecture kjjccbr of kp is
  
begin
  
end kjjccbr;

entity zsoczrf is
  port (shpuhp : buffer integer);
end zsoczrf;

library ieee;
use ieee.std_logic_1164.all;

architecture zumjpz of zsoczrf is
  signal pkucwuwxx : std_logic_vector(1 to 1);
  signal zllmdrb : time;
  signal pyuwyy : time;
  signal mgdswyww : std_logic_vector(1 to 1);
  signal nqbomqqodj : std_logic_vector(0 to 3);
  signal x : std_logic;
  signal jba : time;
begin
  ngrdytvx : entity work.kp
    port map (vkuohenc => jba, uqa => x, jqiulpnuyw => nqbomqqodj, wplr => mgdswyww);
  cmmlzoqw : entity work.kp
    port map (vkuohenc => pyuwyy, uqa => x, jqiulpnuyw => nqbomqqodj, wplr => mgdswyww);
  ytmzdaetnp : entity work.kp
    port map (vkuohenc => zllmdrb, uqa => x, jqiulpnuyw => nqbomqqodj, wplr => pkucwuwxx);
  
  -- Single-driven assignments
  shpuhp <= 2#0#;
  
  -- Multi-driven assignments
  x <= x;
  x <= x;
  mgdswyww <= mgdswyww;
  x <= x;
end zumjpz;

entity jfkn is
  port (hdjbfau : linkage real; tbp : in boolean_vector(2 downto 1));
end jfkn;

library ieee;
use ieee.std_logic_1164.all;

architecture tbwru of jfkn is
  signal mvwreuh : std_logic_vector(1 to 1);
  signal spffxb : std_logic_vector(0 to 3);
  signal ttyvns : std_logic;
  signal ozmipjsmon : time;
begin
  xjx : entity work.kp
    port map (vkuohenc => ozmipjsmon, uqa => ttyvns, jqiulpnuyw => spffxb, wplr => mvwreuh);
  
  -- Multi-driven assignments
  mvwreuh <= mvwreuh;
  spffxb <= ('-', 'Z', 'X', 'H');
  spffxb <= ('Z', '1', 'X', 'L');
  ttyvns <= 'L';
end tbwru;



-- Seed after: 7312895460429695965,4177195558088809003
