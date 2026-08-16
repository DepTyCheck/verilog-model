-- Seed: 5828098326156198864,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity enlghcwvon is
  port (tn : buffer real; ouedvvg : out time; apm : in std_logic_vector(4 downto 4); sfytqpzd : in boolean);
end enlghcwvon;

architecture vww of enlghcwvon is
  
begin
  -- Single-driven assignments
  ouedvvg <= 8#3.0403# us;
end vww;

entity gzwmz is
  port (dvddrthdly : inout integer; lfvibgsfh : out severity_level; rgxihvmo : buffer string(5 to 1); j : buffer boolean);
end gzwmz;

architecture rlxhqevwk of gzwmz is
  
begin
  -- Single-driven assignments
  lfvibgsfh <= ERROR;
end rlxhqevwk;

library ieee;
use ieee.std_logic_1164.all;

entity rmesowldyp is
  port (hzjmpjss : out std_logic; vrk : inout integer);
end rmesowldyp;

library ieee;
use ieee.std_logic_1164.all;

architecture opntkxxu of rmesowldyp is
  signal iv : std_logic_vector(4 downto 4);
  signal dge : time;
  signal eu : real;
  signal q : boolean;
  signal zozfqsfi : std_logic_vector(4 downto 4);
  signal dvc : time;
  signal yskqdafd : real;
  signal zgr : boolean;
  signal kipqzle : std_logic_vector(4 downto 4);
  signal kxkabjv : time;
  signal c : real;
begin
  rdpd : entity work.enlghcwvon
    port map (tn => c, ouedvvg => kxkabjv, apm => kipqzle, sfytqpzd => zgr);
  ke : entity work.enlghcwvon
    port map (tn => yskqdafd, ouedvvg => dvc, apm => zozfqsfi, sfytqpzd => q);
  uxu : entity work.enlghcwvon
    port map (tn => eu, ouedvvg => dge, apm => iv, sfytqpzd => zgr);
  
  -- Multi-driven assignments
  iv <= kipqzle;
  kipqzle <= kipqzle;
  hzjmpjss <= '-';
  hzjmpjss <= hzjmpjss;
end opntkxxu;



-- Seed after: 10159414794701523990,13857275728440271305
