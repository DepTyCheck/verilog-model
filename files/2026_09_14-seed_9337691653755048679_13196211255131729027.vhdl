-- Seed: 9337691653755048679,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity xarmf is
  port (fo : out integer; wpan : buffer std_logic; t : linkage bit_vector(2 to 1); boi : out std_logic);
end xarmf;

architecture pzppxpcx of xarmf is
  
begin
  -- Single-driven assignments
  fo <= 8#1#;
  
  -- Multi-driven assignments
  boi <= '0';
  wpan <= 'X';
end pzppxpcx;

entity o is
  port (lf : in integer_vector(0 to 1));
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture njy of o is
  signal wvvszguk : bit_vector(2 to 1);
  signal mpptnfi : std_logic;
  signal wtdj : integer;
  signal wigqoq : bit_vector(2 to 1);
  signal mxc : integer;
  signal z : std_logic;
  signal eikg : bit_vector(2 to 1);
  signal kkhjjt : integer;
  signal caprek : bit_vector(2 to 1);
  signal kaaq : std_logic;
  signal vakbeoa : integer;
begin
  emknma : entity work.xarmf
    port map (fo => vakbeoa, wpan => kaaq, t => caprek, boi => kaaq);
  zybcdt : entity work.xarmf
    port map (fo => kkhjjt, wpan => kaaq, t => eikg, boi => z);
  ez : entity work.xarmf
    port map (fo => mxc, wpan => kaaq, t => wigqoq, boi => kaaq);
  eddoa : entity work.xarmf
    port map (fo => wtdj, wpan => mpptnfi, t => wvvszguk, boi => kaaq);
  
  -- Multi-driven assignments
  z <= '1';
end njy;



-- Seed after: 6138285270889141664,13196211255131729027
