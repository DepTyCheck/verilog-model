-- Seed: 16735382084957606614,9951735690217599971

library ieee;
use ieee.std_logic_1164.all;

entity mvmmbv is
  port (qbucjsgz : linkage std_logic; cqf : in std_logic_vector(2 downto 0));
end mvmmbv;



architecture ecojxzra of mvmmbv is
  
begin
  
end ecojxzra;

library ieee;
use ieee.std_logic_1164.all;

entity lypepe is
  port (prscs : in std_logic_vector(4 downto 0); pov : out severity_level; eacugarb : linkage bit_vector(0 to 3); fy : in time);
end lypepe;

library ieee;
use ieee.std_logic_1164.all;

architecture wjf of lypepe is
  signal dwane : std_logic_vector(2 downto 0);
  signal hae : std_logic;
begin
  tnumqkmjai : entity work.mvmmbv
    port map (qbucjsgz => hae, cqf => dwane);
  htuogahs : entity work.mvmmbv
    port map (qbucjsgz => hae, cqf => dwane);
end wjf;

library ieee;
use ieee.std_logic_1164.all;

entity wry is
  port (fkhy : inout std_logic);
end wry;

library ieee;
use ieee.std_logic_1164.all;

architecture oczchuw of wry is
  signal hlwc : std_logic;
  signal puuu : std_logic_vector(2 downto 0);
  signal bg : std_logic;
  signal yovgbanwu : std_logic_vector(2 downto 0);
  signal umqr : std_logic_vector(2 downto 0);
begin
  rwc : entity work.mvmmbv
    port map (qbucjsgz => fkhy, cqf => umqr);
  y : entity work.mvmmbv
    port map (qbucjsgz => fkhy, cqf => yovgbanwu);
  svf : entity work.mvmmbv
    port map (qbucjsgz => bg, cqf => puuu);
  uaxfnyc : entity work.mvmmbv
    port map (qbucjsgz => hlwc, cqf => umqr);
end oczchuw;



entity ldkicryqi is
  port (rqmoex : out time_vector(1 downto 4));
end ldkicryqi;

library ieee;
use ieee.std_logic_1164.all;

architecture lyr of ldkicryqi is
  signal yaflpputg : time;
  signal fhq : bit_vector(0 to 3);
  signal gfr : severity_level;
  signal ibfiankjhe : std_logic_vector(4 downto 0);
  signal hight : time;
  signal bsc : severity_level;
  signal nenerxbenv : std_logic_vector(4 downto 0);
  signal zroni : time;
  signal t : bit_vector(0 to 3);
  signal plfdifx : severity_level;
  signal vxyqb : std_logic_vector(4 downto 0);
begin
  ic : entity work.lypepe
    port map (prscs => vxyqb, pov => plfdifx, eacugarb => t, fy => zroni);
  l : entity work.lypepe
    port map (prscs => nenerxbenv, pov => bsc, eacugarb => t, fy => hight);
  cnsobw : entity work.lypepe
    port map (prscs => ibfiankjhe, pov => gfr, eacugarb => fhq, fy => yaflpputg);
end lyr;



-- Seed after: 9532602691767318737,9951735690217599971
