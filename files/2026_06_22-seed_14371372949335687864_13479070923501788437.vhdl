-- Seed: 14371372949335687864,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity tlcebjut is
  port (yxewtyve : inout std_logic);
end tlcebjut;

architecture g of tlcebjut is
  
begin
  -- Multi-driven assignments
  yxewtyve <= '0';
  yxewtyve <= 'U';
  yxewtyve <= '1';
  yxewtyve <= 'W';
end g;

entity zxsy is
  port (rbdnqnbdxe : inout real);
end zxsy;

library ieee;
use ieee.std_logic_1164.all;

architecture vo of zxsy is
  signal pwnschfgnn : std_logic;
begin
  i : entity work.tlcebjut
    port map (yxewtyve => pwnschfgnn);
  
  -- Single-driven assignments
  rbdnqnbdxe <= 1_0.0;
end vo;

entity jnyt is
  port (qmpt : buffer boolean_vector(3 to 2));
end jnyt;

architecture kciztfpm of jnyt is
  
begin
  -- Single-driven assignments
  qmpt <= (others => TRUE);
end kciztfpm;

entity jp is
  port (h : inout bit_vector(4 to 1); twqlqzxv : linkage bit; ihbhyjl : in bit_vector(1 to 0); yuvgqehzjs : inout integer);
end jp;

architecture tpg of jp is
  signal mrgrdh : boolean_vector(3 to 2);
  signal kxcbckzh : real;
begin
  eqgbvzdej : entity work.zxsy
    port map (rbdnqnbdxe => kxcbckzh);
  nomugou : entity work.jnyt
    port map (qmpt => mrgrdh);
end tpg;



-- Seed after: 8776638323507510772,13479070923501788437
