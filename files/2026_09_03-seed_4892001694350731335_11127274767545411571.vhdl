-- Seed: 4892001694350731335,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity xrox is
  port (wvwyti : inout boolean; vukuzymmx : inout time_vector(1 downto 4); f : out bit; jznawir : linkage std_logic_vector(1 downto 2));
end xrox;

architecture tto of xrox is
  
begin
  -- Single-driven assignments
  vukuzymmx <= (others => 0 ns);
  wvwyti <= TRUE;
  f <= f;
end tto;

entity r is
  port (vskkiog : linkage boolean; vbg : inout integer_vector(0 downto 2); zcdhu : in boolean);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture h of r is
  signal hau : std_logic_vector(1 downto 2);
  signal yyawpkzpfy : bit;
  signal qvvsnzhpwd : time_vector(1 downto 4);
  signal eftavbaepi : boolean;
  signal npgjnc : bit;
  signal jiekxgn : time_vector(1 downto 4);
  signal ncykdcg : boolean;
  signal x : std_logic_vector(1 downto 2);
  signal tcyllgq : bit;
  signal eqwy : time_vector(1 downto 4);
  signal gkhvd : boolean;
begin
  zydlc : entity work.xrox
    port map (wvwyti => gkhvd, vukuzymmx => eqwy, f => tcyllgq, jznawir => x);
  krsrouo : entity work.xrox
    port map (wvwyti => ncykdcg, vukuzymmx => jiekxgn, f => npgjnc, jznawir => x);
  dmxkwvbtr : entity work.xrox
    port map (wvwyti => eftavbaepi, vukuzymmx => qvvsnzhpwd, f => yyawpkzpfy, jznawir => hau);
  
  -- Multi-driven assignments
  x <= x;
end h;

entity zdch is
  port (amxm : buffer time; jdfgtfle : linkage time);
end zdch;

library ieee;
use ieee.std_logic_1164.all;

architecture i of zdch is
  signal kwa : std_logic_vector(1 downto 2);
  signal xhhsohgs : bit;
  signal yvz : time_vector(1 downto 4);
  signal ubz : boolean;
  signal eayqdn : bit;
  signal iyknwawrv : time_vector(1 downto 4);
  signal cdd : boolean;
  signal ze : std_logic_vector(1 downto 2);
  signal uhjvzpzcd : bit;
  signal b : time_vector(1 downto 4);
  signal uabrifcx : boolean;
begin
  rw : entity work.xrox
    port map (wvwyti => uabrifcx, vukuzymmx => b, f => uhjvzpzcd, jznawir => ze);
  bmvmtttg : entity work.xrox
    port map (wvwyti => cdd, vukuzymmx => iyknwawrv, f => eayqdn, jznawir => ze);
  jzw : entity work.xrox
    port map (wvwyti => ubz, vukuzymmx => yvz, f => xhhsohgs, jznawir => kwa);
  
  -- Single-driven assignments
  amxm <= 16#6_7_0_F.776# ps;
end i;



-- Seed after: 9842733434247753745,11127274767545411571
