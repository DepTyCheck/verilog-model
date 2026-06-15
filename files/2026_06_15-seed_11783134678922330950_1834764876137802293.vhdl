-- Seed: 11783134678922330950,1834764876137802293

entity ctpp is
  port (ugxxlpswtm : inout integer; jzscgalkz : buffer real_vector(2 to 3));
end ctpp;

architecture otdutr of ctpp is
  
begin
  -- Single-driven assignments
  jzscgalkz <= (8#3.6446#, 2#0_1_0_1.0_0#);
  ugxxlpswtm <= 1024;
end otdutr;

library ieee;
use ieee.std_logic_1164.all;

entity snzois is
  port (tee : out std_logic_vector(4 downto 4); elkfrhbkux : in std_logic_vector(2 to 0); rwyokahg : out real; ecn : in time_vector(3 downto 2));
end snzois;

architecture temjskwb of snzois is
  signal shkslkz : real_vector(2 to 3);
  signal hoh : integer;
begin
  lceajwnk : entity work.ctpp
    port map (ugxxlpswtm => hoh, jzscgalkz => shkslkz);
  
  -- Single-driven assignments
  rwyokahg <= 2#1.10100#;
end temjskwb;

entity nguq is
  port (dixwww : in time);
end nguq;

architecture gmbyetcvk of nguq is
  signal josuxwtsgo : real_vector(2 to 3);
  signal xjywbzfh : integer;
  signal u : real_vector(2 to 3);
  signal c : integer;
begin
  efhod : entity work.ctpp
    port map (ugxxlpswtm => c, jzscgalkz => u);
  icpeumc : entity work.ctpp
    port map (ugxxlpswtm => xjywbzfh, jzscgalkz => josuxwtsgo);
end gmbyetcvk;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (maiqyabay : inout std_logic_vector(4 downto 3));
end e;

architecture nhuf of e is
  signal xhfyjvgksq : real_vector(2 to 3);
  signal javqq : integer;
  signal fjjzuuxau : real_vector(2 to 3);
  signal mlos : integer;
  signal sz : real_vector(2 to 3);
  signal qhhnlbayai : integer;
  signal h : time;
begin
  ztjaream : entity work.nguq
    port map (dixwww => h);
  qhxwiy : entity work.ctpp
    port map (ugxxlpswtm => qhhnlbayai, jzscgalkz => sz);
  wsbzomfe : entity work.ctpp
    port map (ugxxlpswtm => mlos, jzscgalkz => fjjzuuxau);
  ljczjob : entity work.ctpp
    port map (ugxxlpswtm => javqq, jzscgalkz => xhfyjvgksq);
  
  -- Single-driven assignments
  h <= 8#7# ms;
  
  -- Multi-driven assignments
  maiqyabay <= "ZH";
end nhuf;



-- Seed after: 6395959616878160504,1834764876137802293
