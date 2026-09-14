-- Seed: 3320645851229793597,13196211255131729027

entity htb is
  port (lktxnxut : inout boolean; nhcmew : inout boolean; rpoovqmxey : out real);
end htb;

architecture ynznoyv of htb is
  
begin
  -- Single-driven assignments
  rpoovqmxey <= 32.3031;
  nhcmew <= FALSE;
end ynznoyv;

entity alvochziwi is
  port (alvgvglde : buffer time);
end alvochziwi;

architecture typuh of alvochziwi is
  signal axmap : real;
  signal o : boolean;
  signal qfknln : boolean;
begin
  errhjkoq : entity work.htb
    port map (lktxnxut => qfknln, nhcmew => o, rpoovqmxey => axmap);
  
  -- Single-driven assignments
  alvgvglde <= 2#10# ns;
end typuh;

entity wiobw is
  port (qmz : out bit_vector(1 to 1));
end wiobw;

architecture h of wiobw is
  signal iyit : time;
  signal pben : real;
  signal qthb : boolean;
  signal e : boolean;
begin
  tpupa : entity work.htb
    port map (lktxnxut => e, nhcmew => qthb, rpoovqmxey => pben);
  kmwoqqgwc : entity work.alvochziwi
    port map (alvgvglde => iyit);
  
  -- Single-driven assignments
  qmz <= (others => '0');
end h;

library ieee;
use ieee.std_logic_1164.all;

entity gphvjamso is
  port (pfkj : inout boolean; kzdeekije : out std_logic_vector(3 to 3); usl : buffer real; xm : buffer string(1 to 5));
end gphvjamso;

architecture atvqpuemo of gphvjamso is
  signal e : boolean;
  signal a : time;
begin
  ncif : entity work.alvochziwi
    port map (alvgvglde => a);
  ujdips : entity work.htb
    port map (lktxnxut => e, nhcmew => pfkj, rpoovqmxey => usl);
  
  -- Single-driven assignments
  xm <= "psnaz";
  
  -- Multi-driven assignments
  kzdeekije <= "1";
  kzdeekije <= kzdeekije;
end atvqpuemo;



-- Seed after: 10397607233602122007,13196211255131729027
