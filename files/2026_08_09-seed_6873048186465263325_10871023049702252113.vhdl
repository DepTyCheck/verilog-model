-- Seed: 6873048186465263325,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity hjcwdux is
  port (vgnpkvc : out std_logic; bpldbzvukb : linkage std_logic_vector(0 downto 1); a : linkage integer; jqeumtpua : in boolean_vector(2 to 3));
end hjcwdux;

architecture qimyilntx of hjcwdux is
  
begin
  -- Multi-driven assignments
  vgnpkvc <= '0';
  vgnpkvc <= vgnpkvc;
  vgnpkvc <= vgnpkvc;
end qimyilntx;

library ieee;
use ieee.std_logic_1164.all;

entity nkkjxuqya is
  port (j : out string(5 downto 5); hzke : in std_logic_vector(3 downto 0));
end nkkjxuqya;

architecture iywo of nkkjxuqya is
  
begin
  
end iywo;

library ieee;
use ieee.std_logic_1164.all;

entity nwqcaqy is
  port (n : in std_logic_vector(3 downto 4));
end nwqcaqy;

library ieee;
use ieee.std_logic_1164.all;

architecture rymwllwow of nwqcaqy is
  signal gngjjp : integer;
  signal aqrli : std_logic;
  signal xyaqrrvgix : integer;
  signal gpyozlt : boolean_vector(2 to 3);
  signal ahsvphl : integer;
  signal yr : std_logic;
begin
  bkzoin : entity work.hjcwdux
    port map (vgnpkvc => yr, bpldbzvukb => n, a => ahsvphl, jqeumtpua => gpyozlt);
  otogm : entity work.hjcwdux
    port map (vgnpkvc => yr, bpldbzvukb => n, a => xyaqrrvgix, jqeumtpua => gpyozlt);
  fvbcnm : entity work.hjcwdux
    port map (vgnpkvc => aqrli, bpldbzvukb => n, a => gngjjp, jqeumtpua => gpyozlt);
  
  -- Single-driven assignments
  gpyozlt <= gpyozlt;
end rymwllwow;

entity lpfdxpwnrp is
  port (ylps : buffer time);
end lpfdxpwnrp;

library ieee;
use ieee.std_logic_1164.all;

architecture qs of lpfdxpwnrp is
  signal bczh : boolean_vector(2 to 3);
  signal ql : integer;
  signal agppdz : std_logic;
  signal mvybebmatz : boolean_vector(2 to 3);
  signal bynhx : integer;
  signal qssoj : std_logic_vector(0 downto 1);
  signal uffhr : std_logic;
begin
  ffzmkjlnwy : entity work.hjcwdux
    port map (vgnpkvc => uffhr, bpldbzvukb => qssoj, a => bynhx, jqeumtpua => mvybebmatz);
  vab : entity work.hjcwdux
    port map (vgnpkvc => agppdz, bpldbzvukb => qssoj, a => ql, jqeumtpua => bczh);
  
  -- Single-driven assignments
  mvybebmatz <= (FALSE, TRUE);
  bczh <= (TRUE, FALSE);
  ylps <= ylps;
  
  -- Multi-driven assignments
  qssoj <= qssoj;
  agppdz <= uffhr;
  agppdz <= 'X';
  uffhr <= 'Z';
end qs;



-- Seed after: 7138004411637387388,10871023049702252113
