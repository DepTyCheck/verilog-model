-- Seed: 9236227573701565454,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity isvwvp is
  port (sb : linkage std_logic; nvjfq : linkage time_vector(4 downto 0); ywae : linkage time);
end isvwvp;

architecture cmej of isvwvp is
  
begin
  
end cmej;

library ieee;
use ieee.std_logic_1164.all;

entity alhqeite is
  port (waxkvz : linkage boolean_vector(1 to 1); umnrm : linkage std_logic_vector(0 downto 4));
end alhqeite;

library ieee;
use ieee.std_logic_1164.all;

architecture ckears of alhqeite is
  signal hgto : time;
  signal u : time_vector(4 downto 0);
  signal emtzk : time;
  signal kskwphol : time_vector(4 downto 0);
  signal fguyqkjgh : time;
  signal eirwfjzluc : time_vector(4 downto 0);
  signal obmukn : std_logic;
  signal hawtq : time;
  signal zuhufrl : time_vector(4 downto 0);
  signal dnhleiosk : std_logic;
begin
  lv : entity work.isvwvp
    port map (sb => dnhleiosk, nvjfq => zuhufrl, ywae => hawtq);
  wzzj : entity work.isvwvp
    port map (sb => obmukn, nvjfq => eirwfjzluc, ywae => fguyqkjgh);
  ezlynhp : entity work.isvwvp
    port map (sb => dnhleiosk, nvjfq => kskwphol, ywae => emtzk);
  nrksw : entity work.isvwvp
    port map (sb => dnhleiosk, nvjfq => u, ywae => hgto);
end ckears;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (rkvnvhhurh : out character; qekbv : in string(5 downto 2); bxutkslmq : buffer severity_level; lgishihfl : out std_logic_vector(0 to 3));
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture kal of w is
  signal zptmlm : time;
  signal z : time_vector(4 downto 0);
  signal o : std_logic;
  signal cjegzby : boolean_vector(1 to 1);
  signal qjq : std_logic_vector(0 downto 4);
  signal m : boolean_vector(1 to 1);
  signal rzhfinj : time;
  signal vsgj : time_vector(4 downto 0);
  signal h : std_logic;
begin
  keldcrwh : entity work.isvwvp
    port map (sb => h, nvjfq => vsgj, ywae => rzhfinj);
  qqddde : entity work.alhqeite
    port map (waxkvz => m, umnrm => qjq);
  a : entity work.alhqeite
    port map (waxkvz => cjegzby, umnrm => qjq);
  bgocz : entity work.isvwvp
    port map (sb => o, nvjfq => z, ywae => zptmlm);
  
  -- Single-driven assignments
  bxutkslmq <= ERROR;
  rkvnvhhurh <= 'e';
  
  -- Multi-driven assignments
  lgishihfl <= "L1UX";
end kal;



-- Seed after: 894282448723590278,13694093582652240945
