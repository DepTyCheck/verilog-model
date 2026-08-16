-- Seed: 7332126337572934386,13857275728440271305

entity mms is
  port (gkzddui : in real; dmluafemqd : in time);
end mms;

architecture pythldiwt of mms is
  
begin
  
end pythldiwt;

entity ivmaww is
  port (rb : in bit);
end ivmaww;

architecture wqfujxfa of ivmaww is
  signal bpbbfb : time;
  signal lvhwl : time;
  signal qdzubcni : real;
  signal npqjn : time;
  signal zsuwb : time;
  signal tv : real;
begin
  jlep : entity work.mms
    port map (gkzddui => tv, dmluafemqd => zsuwb);
  qg : entity work.mms
    port map (gkzddui => tv, dmluafemqd => npqjn);
  lyhzkho : entity work.mms
    port map (gkzddui => qdzubcni, dmluafemqd => lvhwl);
  tzd : entity work.mms
    port map (gkzddui => tv, dmluafemqd => bpbbfb);
  
  -- Single-driven assignments
  npqjn <= zsuwb;
end wqfujxfa;

library ieee;
use ieee.std_logic_1164.all;

entity gddlcemi is
  port (hbj : out std_logic; gp : out integer; blzeoxmo : in integer; gyt : inout integer);
end gddlcemi;

architecture k of gddlcemi is
  signal dkyosupy : time;
  signal qpin : real;
begin
  j : entity work.mms
    port map (gkzddui => qpin, dmluafemqd => dkyosupy);
  
  -- Single-driven assignments
  qpin <= 3.32;
  gyt <= 314;
  
  -- Multi-driven assignments
  hbj <= hbj;
end k;



-- Seed after: 14526007997535396995,13857275728440271305
