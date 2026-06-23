-- Seed: 9997600842686548863,8421704836678237495

entity h is
  port (npvdi : out real; xbponafxss : buffer character; yuu : in integer_vector(0 to 4); fg : in time);
end h;

architecture i of h is
  
begin
  -- Single-driven assignments
  xbponafxss <= 'g';
  npvdi <= 16#7_C_9_1.4#;
end i;

library ieee;
use ieee.std_logic_1164.all;

entity cnaap is
  port (ttp : buffer std_logic; ucm : inout time_vector(4 to 3));
end cnaap;

architecture n of cnaap is
  signal gnebhjpw : integer_vector(0 to 4);
  signal ctdpvr : character;
  signal jtbu : real;
  signal edbcbmmb : time;
  signal qikmjo : integer_vector(0 to 4);
  signal u : character;
  signal dbhufibev : real;
begin
  qykuwa : entity work.h
    port map (npvdi => dbhufibev, xbponafxss => u, yuu => qikmjo, fg => edbcbmmb);
  khsofazgtq : entity work.h
    port map (npvdi => jtbu, xbponafxss => ctdpvr, yuu => gnebhjpw, fg => edbcbmmb);
  
  -- Single-driven assignments
  ucm <= (others => 0 ns);
  edbcbmmb <= 16#8.E9# us;
  qikmjo <= (8#26#, 3_4, 16#A#, 2#0001#, 313);
  
  -- Multi-driven assignments
  ttp <= 'U';
  ttp <= 'Z';
  ttp <= '1';
end n;



-- Seed after: 10716056649009856027,8421704836678237495
