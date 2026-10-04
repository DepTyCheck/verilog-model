-- Seed: 9836927502880508282,15795020531041709203

entity kjtqm is
  port (meo : inout real; jc : linkage time; axqzk : in severity_level; mthrju : linkage time);
end kjtqm;

architecture yrkxrwgn of kjtqm is
  
begin
  
end yrkxrwgn;

library ieee;
use ieee.std_logic_1164.all;

entity dakcvmkblk is
  port (r : out std_logic_vector(1 to 0); iuinvsr : inout integer; txwx : linkage std_logic; qpdmklbc : out std_logic);
end dakcvmkblk;

architecture oxrzb of dakcvmkblk is
  signal otxkwk : time;
  signal jsfstaurnq : severity_level;
  signal jq : time;
  signal yyntojsf : real;
begin
  froialcm : entity work.kjtqm
    port map (meo => yyntojsf, jc => jq, axqzk => jsfstaurnq, mthrju => otxkwk);
  
  -- Single-driven assignments
  jsfstaurnq <= jsfstaurnq;
  iuinvsr <= iuinvsr;
end oxrzb;

library ieee;
use ieee.std_logic_1164.all;

entity er is
  port (guonvitigz : in integer; tixrzdtzz : buffer std_logic_vector(0 downto 3); jmnx : out time);
end er;

library ieee;
use ieee.std_logic_1164.all;

architecture zcefrnocr of er is
  signal zzmqub : std_logic;
  signal ugjhth : std_logic;
  signal bfucla : integer;
  signal t : std_logic_vector(1 to 0);
  signal wgcyuznxbs : std_logic;
  signal cwjy : integer;
  signal nyfaannwcc : std_logic_vector(1 to 0);
  signal iprol : severity_level;
  signal ugd : time;
  signal byhoxyta : real;
begin
  lhr : entity work.kjtqm
    port map (meo => byhoxyta, jc => ugd, axqzk => iprol, mthrju => jmnx);
  lidd : entity work.dakcvmkblk
    port map (r => nyfaannwcc, iuinvsr => cwjy, txwx => wgcyuznxbs, qpdmklbc => wgcyuznxbs);
  cwpp : entity work.dakcvmkblk
    port map (r => t, iuinvsr => bfucla, txwx => ugjhth, qpdmklbc => zzmqub);
  
  -- Single-driven assignments
  iprol <= iprol;
  
  -- Multi-driven assignments
  ugjhth <= wgcyuznxbs;
  nyfaannwcc <= (others => '0');
  ugjhth <= 'Z';
  tixrzdtzz <= tixrzdtzz;
end zcefrnocr;



-- Seed after: 11055164349296690083,15795020531041709203
