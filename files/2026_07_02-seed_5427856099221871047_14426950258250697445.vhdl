-- Seed: 5427856099221871047,14426950258250697445

use std.reflection.all;

entity xkqvyvehm is
  port (nukpiro : inout value_mirror);
end xkqvyvehm;

architecture pyb of xkqvyvehm is
  
begin
  
end pyb;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity bbgjwficd is
  port (pakd : inout value_mirror; x : buffer std_logic; aoo : linkage std_logic_vector(0 to 2); edlel : inout value_mirror);
end bbgjwficd;

use std.reflection.all;

architecture jvxrsbac of bbgjwficd is
  shared variable glxiej : value_mirror;
begin
  ldh : entity work.xkqvyvehm
    port map (nukpiro => edlel);
  jf : entity work.xkqvyvehm
    port map (nukpiro => glxiej);
  jz : entity work.xkqvyvehm
    port map (nukpiro => pakd);
  
  -- Multi-driven assignments
  x <= 'H';
  x <= 'X';
  x <= x;
end jvxrsbac;

use std.reflection.all;

entity ikdz is
  port (xtvlyeslpf : inout integer_value_mirror);
end ikdz;

use std.reflection.all;

architecture kp of ikdz is
  shared variable gqyarsoltg : value_mirror;
  shared variable qpooc : value_mirror;
  shared variable zqb : value_mirror;
begin
  osv : entity work.xkqvyvehm
    port map (nukpiro => zqb);
  dwghse : entity work.xkqvyvehm
    port map (nukpiro => qpooc);
  xvdkzqhjjs : entity work.xkqvyvehm
    port map (nukpiro => gqyarsoltg);
end kp;

use std.reflection.all;

entity dublvo is
  port (kaui : out integer_vector(4 to 0); c : in real; v : inout integer_subtype_mirror);
end dublvo;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ayf of dublvo is
  shared variable lsnefh : value_mirror;
  signal nqglde : std_logic_vector(0 to 2);
  signal xibjkovrg : std_logic;
  shared variable owtrmumok : value_mirror;
  shared variable mrhqvpe : value_mirror;
  signal xzbmhuf : std_logic_vector(0 to 2);
  signal nfvijlwpu : std_logic;
  shared variable b : value_mirror;
begin
  avcat : entity work.bbgjwficd
    port map (pakd => b, x => nfvijlwpu, aoo => xzbmhuf, edlel => mrhqvpe);
  ynlj : entity work.bbgjwficd
    port map (pakd => owtrmumok, x => xibjkovrg, aoo => nqglde, edlel => lsnefh);
  
  -- Single-driven assignments
  kaui <= (others => 0);
  
  -- Multi-driven assignments
  xibjkovrg <= 'U';
  nqglde <= ('L', 'H', 'W');
end ayf;



-- Seed after: 15501534549917444530,14426950258250697445
