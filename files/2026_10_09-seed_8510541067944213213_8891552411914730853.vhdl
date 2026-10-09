-- Seed: 8510541067944213213,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity dzn is
  port (sbie : inout std_logic; vq : out time);
end dzn;

architecture xu of dzn is
  
begin
  -- Single-driven assignments
  vq <= 3 sec;
  
  -- Multi-driven assignments
  sbie <= 'X';
  sbie <= '-';
  sbie <= sbie;
end xu;

library ieee;
use ieee.std_logic_1164.all;

entity igcubf is
  port (bze : buffer string(3 downto 1); evwgdwmpca : out std_logic; e : inout real; wvljxhx : buffer integer);
end igcubf;

architecture fraaa of igcubf is
  signal fwapovpjmb : time;
begin
  mcdit : entity work.dzn
    port map (sbie => evwgdwmpca, vq => fwapovpjmb);
  
  -- Single-driven assignments
  e <= 4.2;
  wvljxhx <= wvljxhx;
  
  -- Multi-driven assignments
  evwgdwmpca <= 'X';
  evwgdwmpca <= evwgdwmpca;
  evwgdwmpca <= 'L';
  evwgdwmpca <= 'L';
end fraaa;

library ieee;
use ieee.std_logic_1164.all;

entity vaugwfpm is
  port (gtlusq : out std_logic; vbcqwd : inout real; mmctyfktn : inout bit; n : out real);
end vaugwfpm;

architecture imhw of vaugwfpm is
  
begin
  -- Multi-driven assignments
  gtlusq <= gtlusq;
end imhw;

library ieee;
use ieee.std_logic_1164.all;

entity dbd is
  port (iymc : out time; f : in std_logic_vector(4 downto 1); fmgew : linkage std_logic_vector(0 downto 3));
end dbd;

library ieee;
use ieee.std_logic_1164.all;

architecture bfulbbgsqv of dbd is
  signal ttztzgn : integer;
  signal fvoxvhju : real;
  signal ueqrhz : string(3 downto 1);
  signal oxexcxjek : time;
  signal yflkfl : std_logic;
begin
  rxdt : entity work.dzn
    port map (sbie => yflkfl, vq => oxexcxjek);
  fvmygxlpl : entity work.igcubf
    port map (bze => ueqrhz, evwgdwmpca => yflkfl, e => fvoxvhju, wvljxhx => ttztzgn);
  
  -- Single-driven assignments
  iymc <= 16#A_8# ns;
  
  -- Multi-driven assignments
  yflkfl <= yflkfl;
end bfulbbgsqv;



-- Seed after: 4366544054370524072,8891552411914730853
