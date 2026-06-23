-- Seed: 13481041168319354246,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (poxkqes : out std_logic; qijsjbizes : out std_logic; tjaf : linkage std_logic_vector(0 downto 2); ythljeri : linkage std_logic);
end o;

architecture fllvhxtirf of o is
  
begin
  -- Multi-driven assignments
  poxkqes <= 'W';
  qijsjbizes <= '1';
  qijsjbizes <= 'H';
end fllvhxtirf;

library ieee;
use ieee.std_logic_1164.all;

entity si is
  port (jf : out std_logic; boxcbi : out std_logic; gl : inout time);
end si;

architecture r of si is
  
begin
  -- Single-driven assignments
  gl <= 8#0_1_3.675# ps;
  
  -- Multi-driven assignments
  boxcbi <= '1';
  boxcbi <= 'L';
end r;

entity wysygn is
  port (onvdktakdy : linkage time; miymhxdp : in bit_vector(1 downto 2); qtvavsuy : inout real);
end wysygn;

library ieee;
use ieee.std_logic_1164.all;

architecture ylysggki of wysygn is
  signal sscnhw : std_logic_vector(0 downto 2);
  signal pdfdoz : std_logic;
  signal kb : std_logic;
  signal xrteofe : std_logic_vector(0 downto 2);
  signal xfjrg : std_logic;
  signal bnpbx : std_logic;
  signal ccmboubxhq : std_logic_vector(0 downto 2);
  signal dmityslexr : std_logic;
  signal zda : std_logic;
begin
  ylkimfgsv : entity work.o
    port map (poxkqes => zda, qijsjbizes => dmityslexr, tjaf => ccmboubxhq, ythljeri => bnpbx);
  oibrzz : entity work.o
    port map (poxkqes => xfjrg, qijsjbizes => zda, tjaf => xrteofe, ythljeri => zda);
  xhsovkcagv : entity work.o
    port map (poxkqes => kb, qijsjbizes => pdfdoz, tjaf => sscnhw, ythljeri => xfjrg);
  
  -- Single-driven assignments
  qtvavsuy <= 16#7BA6.8#;
  
  -- Multi-driven assignments
  zda <= 'X';
  kb <= 'U';
end ylysggki;

entity nnfbbdps is
  port (ffvvbpw : buffer real);
end nnfbbdps;

architecture nibwfpe of nnfbbdps is
  
begin
  -- Single-driven assignments
  ffvvbpw <= 4.2_1_0_1_1;
end nibwfpe;



-- Seed after: 4303114376740050914,8421704836678237495
