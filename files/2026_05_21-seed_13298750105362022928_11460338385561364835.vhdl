-- Seed: 13298750105362022928,11460338385561364835



entity r is
  port (bqgigkui : linkage time; jezyjp : buffer boolean; gsbhcklpp : linkage real);
end r;



architecture npcoeqeyjr of r is
  
begin
  
end npcoeqeyjr;



entity goehq is
  port (qjifcpweqh : inout real; eql : out time);
end goehq;



architecture rmtxcd of goehq is
  signal qwlqwp : real;
  signal cjofvrb : boolean;
  signal fookal : time;
  signal crdnan : real;
  signal bkuzd : boolean;
  signal lzrjtasrym : time;
  signal xlkumf : boolean;
  signal hyiffvwbug : time;
begin
  mbokbnsu : entity work.r
    port map (bqgigkui => hyiffvwbug, jezyjp => xlkumf, gsbhcklpp => qjifcpweqh);
  vdab : entity work.r
    port map (bqgigkui => lzrjtasrym, jezyjp => bkuzd, gsbhcklpp => crdnan);
  mfwfdu : entity work.r
    port map (bqgigkui => fookal, jezyjp => cjofvrb, gsbhcklpp => qwlqwp);
end rmtxcd;

library ieee;
use ieee.std_logic_1164.all;

entity grmjkfolyl is
  port (zjiyaclaha : inout integer; bk : linkage integer; nftme : linkage integer; lirs : out std_logic);
end grmjkfolyl;



architecture bglccy of grmjkfolyl is
  signal xkgzlkpfq : time;
  signal zsntkn : real;
begin
  rtrjfoj : entity work.goehq
    port map (qjifcpweqh => zsntkn, eql => xkgzlkpfq);
end bglccy;

library ieee;
use ieee.std_logic_1164.all;

entity ivts is
  port (jhluzyfog : linkage real; ybaizxyx : inout real; ul : in time; dmzu : inout std_logic);
end ivts;



architecture ggvxcov of ivts is
  signal hcc : real;
  signal tmtevp : boolean;
  signal tpmgnx : boolean;
  signal xjrgk : time;
  signal dkbuqngh : time;
begin
  rokr : entity work.goehq
    port map (qjifcpweqh => ybaizxyx, eql => dkbuqngh);
  urobb : entity work.r
    port map (bqgigkui => xjrgk, jezyjp => tpmgnx, gsbhcklpp => ybaizxyx);
  yxsy : entity work.r
    port map (bqgigkui => ul, jezyjp => tmtevp, gsbhcklpp => hcc);
end ggvxcov;



-- Seed after: 18400004725181864715,11460338385561364835
