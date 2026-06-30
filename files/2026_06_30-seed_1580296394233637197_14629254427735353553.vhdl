-- Seed: 1580296394233637197,14629254427735353553

entity sadksflbao is
  port (dd : in real; w : linkage real; sscqhjpyo : in bit_vector(3 to 3); npymzjksb : linkage real);
end sadksflbao;

architecture xbdo of sadksflbao is
  
begin
  
end xbdo;

library ieee;
use ieee.std_logic_1164.all;

entity sqb is
  port (tbuxk : out bit; kdps : out std_logic; eexwmkqoh : inout time);
end sqb;

architecture od of sqb is
  signal yj : real;
  signal rzafxgnx : bit_vector(3 to 3);
  signal lpymoyq : real;
  signal o : real;
  signal uaaiy : bit_vector(3 to 3);
  signal hcghyt : real;
  signal lnvho : bit_vector(3 to 3);
  signal jhdlbje : real;
  signal rio : real;
begin
  xyj : entity work.sadksflbao
    port map (dd => rio, w => jhdlbje, sscqhjpyo => lnvho, npymzjksb => hcghyt);
  rphu : entity work.sadksflbao
    port map (dd => rio, w => rio, sscqhjpyo => uaaiy, npymzjksb => o);
  leq : entity work.sadksflbao
    port map (dd => hcghyt, w => lpymoyq, sscqhjpyo => rzafxgnx, npymzjksb => yj);
  
  -- Single-driven assignments
  tbuxk <= '1';
  
  -- Multi-driven assignments
  kdps <= '0';
  kdps <= 'W';
  kdps <= '0';
  kdps <= '0';
end od;

entity mhxj is
  port (swamswx : inout real);
end mhxj;

architecture dwglejuit of mhxj is
  signal fuwfyv : bit_vector(3 to 3);
  signal nxojovlrc : real;
begin
  ilc : entity work.sadksflbao
    port map (dd => swamswx, w => nxojovlrc, sscqhjpyo => fuwfyv, npymzjksb => swamswx);
  
  -- Single-driven assignments
  fuwfyv <= (others => '0');
end dwglejuit;



-- Seed after: 8165999543464239099,14629254427735353553
