-- Seed: 2524266817369456065,5362988889992816031



entity o is
  port (aofmsairwb : in time; sbxnzb : inout time; gqguirz : out real);
end o;



architecture bi of o is
  
begin
  
end bi;



entity ga is
  port (i : out real; pmblpa : linkage boolean; zpiln : buffer time);
end ga;



architecture lcij of ga is
  signal pfavjyfq : real;
  signal tjk : time;
  signal oythdcaf : time;
  signal hfciwxtv : time;
begin
  gltjuw : entity work.o
    port map (aofmsairwb => hfciwxtv, sbxnzb => zpiln, gqguirz => i);
  bkgexeyhyk : entity work.o
    port map (aofmsairwb => oythdcaf, sbxnzb => tjk, gqguirz => pfavjyfq);
end lcij;

library ieee;
use ieee.std_logic_1164.all;

entity feon is
  port (gjo : linkage integer; qislist : buffer time; vxhdavno : in std_logic);
end feon;



architecture u of feon is
  signal s : real;
  signal htphmkzdr : time;
  signal qkbnx : time;
begin
  bsa : entity work.o
    port map (aofmsairwb => qkbnx, sbxnzb => htphmkzdr, gqguirz => s);
end u;



-- Seed after: 11749586794523068164,5362988889992816031
