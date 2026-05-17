-- Seed: 11388246500103131660,8581541265164261811



entity lepq is
  port (t : out time; byxemwlav : inout real);
end lepq;



architecture vuuk of lepq is
  
begin
  
end vuuk;



entity xdlgakricr is
  port (yeqfqp : inout integer);
end xdlgakricr;



architecture u of xdlgakricr is
  signal cdhrbo : real;
  signal lbrgxkd : time;
  signal a : real;
  signal fahptdz : time;
  signal dtof : real;
  signal nnffqzbs : time;
begin
  ympi : entity work.lepq
    port map (t => nnffqzbs, byxemwlav => dtof);
  vvlvqfblub : entity work.lepq
    port map (t => fahptdz, byxemwlav => a);
  xqdo : entity work.lepq
    port map (t => lbrgxkd, byxemwlav => cdhrbo);
end u;



entity duqu is
  port (uhpayp : inout real);
end duqu;



architecture zxksuy of duqu is
  signal t : integer;
  signal vjicjk : time;
  signal kszidq : real;
  signal x : time;
  signal td : real;
  signal efzixhkbp : time;
begin
  vobssq : entity work.lepq
    port map (t => efzixhkbp, byxemwlav => td);
  iuoedmmifr : entity work.lepq
    port map (t => x, byxemwlav => kszidq);
  dhohqyds : entity work.lepq
    port map (t => vjicjk, byxemwlav => uhpayp);
  ezub : entity work.xdlgakricr
    port map (yeqfqp => t);
end zxksuy;

library ieee;
use ieee.std_logic_1164.all;

entity dufj is
  port (betrg : out integer; y : inout real; zaw : out std_logic; ftkncwk : out integer);
end dufj;



architecture slgjszw of dufj is
  signal al : real;
  signal xispdsykrt : time;
begin
  zi : entity work.lepq
    port map (t => xispdsykrt, byxemwlav => y);
  imwndcpst : entity work.duqu
    port map (uhpayp => al);
end slgjszw;



-- Seed after: 8049712789936914189,8581541265164261811
