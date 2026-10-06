-- Seed: 14849088131430840112,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (cpcv : out std_logic; wovwqyis : in std_logic; aalz : linkage boolean_vector(1 downto 1); ilksigb : inout std_logic);
end l;

architecture n of l is
  
begin
  
end n;

library ieee;
use ieee.std_logic_1164.all;

entity adm is
  port (i : in std_logic; ttljofsln : in real; vaxvpgh : buffer std_logic; saetufh : out time_vector(0 to 3));
end adm;

library ieee;
use ieee.std_logic_1164.all;

architecture x of adm is
  signal hrgzfrugu : std_logic;
  signal nrdr : boolean_vector(1 downto 1);
  signal xwuabe : std_logic;
  signal whdfqxojyo : std_logic;
  signal cmnljpar : boolean_vector(1 downto 1);
  signal uqywih : std_logic;
  signal yitbn : std_logic;
  signal odbxpatmsp : boolean_vector(1 downto 1);
  signal bcz : std_logic;
  signal trsghdfvo : boolean_vector(1 downto 1);
begin
  royyselebs : entity work.l
    port map (cpcv => vaxvpgh, wovwqyis => vaxvpgh, aalz => trsghdfvo, ilksigb => vaxvpgh);
  qqinubk : entity work.l
    port map (cpcv => bcz, wovwqyis => vaxvpgh, aalz => odbxpatmsp, ilksigb => yitbn);
  katktxim : entity work.l
    port map (cpcv => uqywih, wovwqyis => i, aalz => cmnljpar, ilksigb => uqywih);
  vmdkkltr : entity work.l
    port map (cpcv => whdfqxojyo, wovwqyis => xwuabe, aalz => nrdr, ilksigb => hrgzfrugu);
  
  -- Single-driven assignments
  saetufh <= saetufh;
  
  -- Multi-driven assignments
  vaxvpgh <= whdfqxojyo;
  yitbn <= vaxvpgh;
  whdfqxojyo <= bcz;
end x;



-- Seed after: 8320081231097294085,3042374792655995433
