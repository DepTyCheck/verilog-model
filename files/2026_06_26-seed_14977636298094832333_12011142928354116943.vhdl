-- Seed: 14977636298094832333,12011142928354116943

entity gjby is
  port (zvygsllgt : out severity_level; ckljgzq : inout bit_vector(2 to 3); auqjjeqmam : linkage time);
end gjby;

architecture zxllpeqk of gjby is
  
begin
  -- Single-driven assignments
  ckljgzq <= ('0', '1');
  zvygsllgt <= FAILURE;
end zxllpeqk;

library ieee;
use ieee.std_logic_1164.all;

entity haxv is
  port (iel : inout std_logic; owxb : linkage real; iqdrnba : out std_logic_vector(2 downto 1); qoay : out bit);
end haxv;

architecture ldhvu of haxv is
  signal qqep : time;
  signal vhzozp : bit_vector(2 to 3);
  signal ffi : severity_level;
  signal btzy : time;
  signal vfwuxfjk : bit_vector(2 to 3);
  signal swlwrcz : severity_level;
begin
  ixspsdnqf : entity work.gjby
    port map (zvygsllgt => swlwrcz, ckljgzq => vfwuxfjk, auqjjeqmam => btzy);
  rq : entity work.gjby
    port map (zvygsllgt => ffi, ckljgzq => vhzozp, auqjjeqmam => qqep);
  
  -- Single-driven assignments
  qoay <= '0';
  
  -- Multi-driven assignments
  iel <= '0';
  iqdrnba <= "00";
  iqdrnba <= "ZZ";
  iel <= 'W';
end ldhvu;



-- Seed after: 15565569781503509171,12011142928354116943
