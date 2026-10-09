-- Seed: 11894059845623990722,8891552411914730853

entity mip is
  port (edcwahx : in time);
end mip;

architecture izwsswuw of mip is
  
begin
  
end izwsswuw;

library ieee;
use ieee.std_logic_1164.all;

entity yvmlscsu is
  port (edfu : out real; nse : out std_logic);
end yvmlscsu;

architecture lkx of yvmlscsu is
  signal qetmpx : time;
  signal puluz : time;
begin
  d : entity work.mip
    port map (edcwahx => puluz);
  xumpzeez : entity work.mip
    port map (edcwahx => qetmpx);
  
  -- Multi-driven assignments
  nse <= '-';
  nse <= 'U';
end lkx;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (jagnxg : inout std_logic_vector(1 downto 1); zvgxng : out real);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture rqwips of f is
  signal miihjh : time;
  signal bptp : time;
  signal zworkd : std_logic;
  signal pf : real;
begin
  pawrbmifqa : entity work.yvmlscsu
    port map (edfu => pf, nse => zworkd);
  sgkgfhlgtf : entity work.mip
    port map (edcwahx => bptp);
  hiuqdvnmd : entity work.mip
    port map (edcwahx => miihjh);
  
  -- Multi-driven assignments
  jagnxg <= (others => 'Z');
end rqwips;



-- Seed after: 15802013204706221079,8891552411914730853
