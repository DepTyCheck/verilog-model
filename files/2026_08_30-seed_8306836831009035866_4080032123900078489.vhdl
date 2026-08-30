-- Seed: 8306836831009035866,4080032123900078489

entity lrrkqm is
  port (apzp : inout real; suwn : inout string(4 to 5); rjfyngp : out bit_vector(3 downto 0); wgjzwtzfh : linkage severity_level);
end lrrkqm;

architecture xxfplp of lrrkqm is
  
begin
  -- Single-driven assignments
  suwn <= "nn";
end xxfplp;

library ieee;
use ieee.std_logic_1164.all;

entity hysf is
  port (u : in real; ysgcn : buffer real; r : buffer real; elkdkziz : linkage std_logic_vector(0 to 4));
end hysf;

architecture yrtn of hysf is
  signal sc : severity_level;
  signal zkcbrgxnwp : bit_vector(3 downto 0);
  signal lqjgiqnju : string(4 to 5);
  signal gf : real;
  signal bwjtgh : severity_level;
  signal qd : bit_vector(3 downto 0);
  signal opv : string(4 to 5);
  signal xspnd : severity_level;
  signal pz : bit_vector(3 downto 0);
  signal z : string(4 to 5);
begin
  ukxale : entity work.lrrkqm
    port map (apzp => r, suwn => z, rjfyngp => pz, wgjzwtzfh => xspnd);
  jhi : entity work.lrrkqm
    port map (apzp => ysgcn, suwn => opv, rjfyngp => qd, wgjzwtzfh => bwjtgh);
  jgliogr : entity work.lrrkqm
    port map (apzp => gf, suwn => lqjgiqnju, rjfyngp => zkcbrgxnwp, wgjzwtzfh => sc);
end yrtn;



-- Seed after: 2731331129951060119,4080032123900078489
