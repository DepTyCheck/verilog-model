-- Seed: 15849533164952422863,8067602802092121131

entity sglyz is
  port (oa : inout real; wrfjbppjmz : linkage real_vector(1 downto 4); lnzlzxumg : out integer; vcstge : linkage time);
end sglyz;

architecture xdzsbgxig of sglyz is
  
begin
  -- Single-driven assignments
  oa <= 8#621.4_0#;
end xdzsbgxig;

entity mdolsv is
  port (yj : buffer integer);
end mdolsv;

architecture b of mdolsv is
  signal zbnjnwfxr : time;
  signal nyozs : integer;
  signal casbkd : real_vector(1 downto 4);
  signal vxpcctctdq : real;
  signal wuwitwrown : time;
  signal oqmtq : real_vector(1 downto 4);
  signal dy : real;
begin
  leml : entity work.sglyz
    port map (oa => dy, wrfjbppjmz => oqmtq, lnzlzxumg => yj, vcstge => wuwitwrown);
  lemp : entity work.sglyz
    port map (oa => vxpcctctdq, wrfjbppjmz => casbkd, lnzlzxumg => nyozs, vcstge => zbnjnwfxr);
end b;

library ieee;
use ieee.std_logic_1164.all;

entity luw is
  port (rw : out bit; fcadkulco : buffer std_logic);
end luw;

architecture kjt of luw is
  signal vghspqn : time;
  signal mxzknmbdp : integer;
  signal v : real_vector(1 downto 4);
  signal anogrrg : real;
  signal tcscl : time;
  signal t : integer;
  signal yb : real_vector(1 downto 4);
  signal ifwxmm : real;
  signal xbh : time;
  signal inlubuqlaq : integer;
  signal a : real_vector(1 downto 4);
  signal jxvo : real;
  signal kxnrelepm : time;
  signal ke : integer;
  signal vow : real_vector(1 downto 4);
  signal elpdk : real;
begin
  ctfe : entity work.sglyz
    port map (oa => elpdk, wrfjbppjmz => vow, lnzlzxumg => ke, vcstge => kxnrelepm);
  zwdcz : entity work.sglyz
    port map (oa => jxvo, wrfjbppjmz => a, lnzlzxumg => inlubuqlaq, vcstge => xbh);
  de : entity work.sglyz
    port map (oa => ifwxmm, wrfjbppjmz => yb, lnzlzxumg => t, vcstge => tcscl);
  bajwxqxfni : entity work.sglyz
    port map (oa => anogrrg, wrfjbppjmz => v, lnzlzxumg => mxzknmbdp, vcstge => vghspqn);
  
  -- Multi-driven assignments
  fcadkulco <= fcadkulco;
end kjt;



-- Seed after: 10520527979890191424,8067602802092121131
