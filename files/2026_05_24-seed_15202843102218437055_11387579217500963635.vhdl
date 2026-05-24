-- Seed: 15202843102218437055,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity gefkb is
  port (wbbzvfrv : inout std_logic_vector(4 downto 3));
end gefkb;



architecture srwr of gefkb is
  
begin
  
end srwr;



entity gte is
  port (bwue : out time_vector(0 to 0); t : linkage time_vector(1 downto 4); ohgbqqpoa : in integer);
end gte;

library ieee;
use ieee.std_logic_1164.all;

architecture gsmnzgnfc of gte is
  signal mqey : std_logic_vector(4 downto 3);
begin
  aqzzewn : entity work.gefkb
    port map (wbbzvfrv => mqey);
end gsmnzgnfc;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (ixmndrxb : in integer_vector(4 to 1); jlddd : out integer; hdnhdmcqr : in integer; lo : buffer std_logic);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture xiexu of g is
  signal tzv : std_logic_vector(4 downto 3);
  signal xv : integer;
  signal elbzfsy : time_vector(1 downto 4);
  signal ezm : time_vector(0 to 0);
begin
  egvmsdqx : entity work.gte
    port map (bwue => ezm, t => elbzfsy, ohgbqqpoa => xv);
  dle : entity work.gefkb
    port map (wbbzvfrv => tzv);
end xiexu;



-- Seed after: 10310815460329596151,11387579217500963635
