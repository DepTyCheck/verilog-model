-- Seed: 17296585244055637955,11181851762153539145



entity xcfcsrl is
  port (dgr : linkage integer; chir : buffer time; ynjfmsx : buffer integer);
end xcfcsrl;



architecture wk of xcfcsrl is
  
begin
  
end wk;



entity mp is
  port (bj : inout integer_vector(0 downto 3));
end mp;



architecture xzd of mp is
  signal d : integer;
  signal eqtfzyiymm : time;
  signal zs : integer;
begin
  pypvkhd : entity work.xcfcsrl
    port map (dgr => zs, chir => eqtfzyiymm, ynjfmsx => d);
end xzd;



entity dexmdvyl is
  port (n : linkage time);
end dexmdvyl;



architecture x of dexmdvyl is
  signal r : integer_vector(0 downto 3);
  signal zsenpc : time;
  signal ld : integer;
begin
  dudfrporqf : entity work.xcfcsrl
    port map (dgr => ld, chir => zsenpc, ynjfmsx => ld);
  ceuwoxmex : entity work.mp
    port map (bj => r);
end x;

library ieee;
use ieee.std_logic_1164.all;

entity bgyqeu is
  port (qa : linkage std_logic; fyicbptwwg : out std_logic);
end bgyqeu;



architecture bfqkepxc of bgyqeu is
  signal nrftz : time;
  signal xoegr : integer;
begin
  xvolbkhs : entity work.xcfcsrl
    port map (dgr => xoegr, chir => nrftz, ynjfmsx => xoegr);
end bfqkepxc;



-- Seed after: 10795042352763131737,11181851762153539145
