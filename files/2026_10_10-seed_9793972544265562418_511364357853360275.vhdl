-- Seed: 9793972544265562418,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity lsmoef is
  port (fzobs : buffer real; anx : buffer std_logic_vector(0 downto 1); gxry : inout time);
end lsmoef;

architecture witfsxm of lsmoef is
  
begin
  -- Multi-driven assignments
  anx <= "";
  anx <= anx;
  anx <= anx;
end witfsxm;

library ieee;
use ieee.std_logic_1164.all;

entity qzaqwuad is
  port (olhiudwoo : out real; uxnxtaycjh : buffer std_logic_vector(4 to 0); cw : inout integer; yhnh : linkage std_logic);
end qzaqwuad;

library ieee;
use ieee.std_logic_1164.all;

architecture rqimf of qzaqwuad is
  signal fdsa : time;
  signal mwfsmeiia : time;
  signal s : std_logic_vector(0 downto 1);
  signal bihxhpygv : real;
  signal tayafptik : time;
  signal rviv : real;
begin
  ozfl : entity work.lsmoef
    port map (fzobs => rviv, anx => uxnxtaycjh, gxry => tayafptik);
  vvlgzmb : entity work.lsmoef
    port map (fzobs => bihxhpygv, anx => s, gxry => mwfsmeiia);
  dqhui : entity work.lsmoef
    port map (fzobs => olhiudwoo, anx => uxnxtaycjh, gxry => fdsa);
end rqimf;

library ieee;
use ieee.std_logic_1164.all;

entity agqrripdb is
  port (zyuzz : out real_vector(4 to 3); zsowcr : buffer std_logic_vector(1 to 0); um : linkage std_logic);
end agqrripdb;

library ieee;
use ieee.std_logic_1164.all;

architecture vqiyj of agqrripdb is
  signal fe : time;
  signal fjymp : std_logic_vector(0 downto 1);
  signal bncro : real;
begin
  jrp : entity work.lsmoef
    port map (fzobs => bncro, anx => fjymp, gxry => fe);
  
  -- Single-driven assignments
  zyuzz <= zyuzz;
  
  -- Multi-driven assignments
  zsowcr <= (others => '0');
  fjymp <= zsowcr;
  zsowcr <= (others => '0');
end vqiyj;

entity tt is
  port (ccr : buffer character; qbnxumsv : inout character);
end tt;

library ieee;
use ieee.std_logic_1164.all;

architecture yvfrqjwjus of tt is
  signal usqkrlwxpo : time;
  signal gmr : real;
  signal jwxeip : time;
  signal bywjfokiz : std_logic_vector(0 downto 1);
  signal cgwlklh : real;
  signal ykjttup : std_logic;
  signal ofwh : integer;
  signal o : std_logic_vector(0 downto 1);
  signal cbhsp : real;
begin
  lxdmzjd : entity work.qzaqwuad
    port map (olhiudwoo => cbhsp, uxnxtaycjh => o, cw => ofwh, yhnh => ykjttup);
  bhspiht : entity work.lsmoef
    port map (fzobs => cgwlklh, anx => bywjfokiz, gxry => jwxeip);
  cwt : entity work.lsmoef
    port map (fzobs => gmr, anx => o, gxry => usqkrlwxpo);
end yvfrqjwjus;



-- Seed after: 11298545950996824024,511364357853360275
