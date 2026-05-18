-- Seed: 12016739147670030908,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity eej is
  port (mjgkc : in std_logic_vector(4 downto 4));
end eej;



architecture zpctqk of eej is
  
begin
  
end zpctqk;

library ieee;
use ieee.std_logic_1164.all;

entity ga is
  port (cnebinpti : inout real_vector(0 downto 1); tjttempb : out std_logic_vector(3 downto 3));
end ga;

library ieee;
use ieee.std_logic_1164.all;

architecture lsmuqmrj of ga is
  signal kjlfy : std_logic_vector(4 downto 4);
begin
  jegcjcvsnx : entity work.eej
    port map (mjgkc => tjttempb);
  zyo : entity work.eej
    port map (mjgkc => kjlfy);
  mjrlj : entity work.eej
    port map (mjgkc => tjttempb);
  o : entity work.eej
    port map (mjgkc => tjttempb);
end lsmuqmrj;

library ieee;
use ieee.std_logic_1164.all;

entity yjdj is
  port (sfxpz : linkage integer_vector(4 to 1); hydha : linkage std_logic_vector(0 downto 3));
end yjdj;

library ieee;
use ieee.std_logic_1164.all;

architecture qp of yjdj is
  signal s : real_vector(0 downto 1);
  signal lvqtgl : std_logic_vector(3 downto 3);
  signal zjgbfi : real_vector(0 downto 1);
begin
  lp : entity work.ga
    port map (cnebinpti => zjgbfi, tjttempb => lvqtgl);
  phgajkalv : entity work.eej
    port map (mjgkc => lvqtgl);
  eek : entity work.ga
    port map (cnebinpti => s, tjttempb => lvqtgl);
end qp;



-- Seed after: 735541462968286113,18238119570016518405
